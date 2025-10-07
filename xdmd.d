#!/usr/bin/rdmd

alias Line = string;

enum Op {
	chk, ///< Check.
	run, ///< Run.
	lnt, ///< Lint using Dscanner.
	drn, ///< Run test in dmd.
	all, ///< All.
}

enum TaskType {
	chk, ///< Check.
	run, ///< Run.
	lnt, ///< Lint using Dscanner.
	drn,
}

/++ CLI command including leading process name/path. +/
alias Cmd = const(string)[];

/++ CLI arguments exludding leading process name/path. +/
alias CmdArgs = const(string)[];

/++ CLI switches. +/
alias CmdSwitches = const(string)[];

/++ Process Environment.. +/
alias Environment = string[string];

static immutable lstExt = `.lst`;
static immutable dExt = `.d`;
static immutable dbgFlag = false; // Flags for debug logging via `dbg`.

import std.process : ProcessPipes, Redirect, pipeProcess, wait;
import std.algorithm : map, count, filter, endsWith, startsWith, canFind, findSplitAfter, skipOver, findSplit, either, findSplitAfter, chunkBy;
import std.array : array, join, replace;
import std.path : expandTilde, baseName, stripExtension, buildPath;
import std.file : exists, getcwd, dirEntries, SpanMode, getSize, remove, readText, tempDir, mkdirRecurse;
import std.stdio : stdout, stderr, File;
import std.exception : enforce;
import std.uuid : randomUUID;

struct Task {
	this(TaskType tt, FilePath exe, Cmd cmd, CmdSwitches switches, string[] srcPaths, DirPath cwd, Redirect redirect) {
		CmdArgs cmdArgs = cmd[1 .. $];
		const ddmPath = findExecutable(FileName("ddemangled"));

		// force use ldc if sanitizers has been asked for
		static immutable sanitizeAddressFlag = "-fsanitize=address";
		if (switches.canFind(sanitizeAddressFlag)) {
			const exeLDMD2 = findExecutable(FileName(`ldmd2`));
			if (tt == TaskType.run && exeLDMD2) {
				exe = exeLDMD2; // override
			}
			cmdArgs = cmdArgs.filter!(_ => _ != sanitizeAddressFlag).array; // TODO: merge with filter below
		}

		// force usage of dmd when checking
		const chkdmd = "-chk=dmd";
		if (cmdArgs.canFind(chkdmd)) {
			if (const exeDMD = findExecutable(FileName(`dmd`)))
				exe = exeDMD; // override
			cmdArgs = cmdArgs.filter!(_ => _ != chkdmd).array; // TODO: merge with filter below
		}

		// debug writeln("In ", cwd, ": ", tt, ": ", (exe.str ~ cmdArgs).join(' '));
		this.tt = tt;
		final switch (tt) {
		case TaskType.chk:
			this.cmdArgs = cmdArgs.filter!(_ => _ != "-main" && _ != "-run").array ~ [`-o-`];
			this.use = true;
			break;
		case TaskType.lnt:
			this.cmdArgs = ["lint", "--styleCheck", "--errorFormat=digitalmars"] ~ cmdArgs.filter!(_ => _.endsWith(".d") || _.startsWith("-I")).array;
			this.use = true;
			break;
		case TaskType.drn:
			this.cmdArgs = cmdArgs.filter!(_ => _.endsWith(".d")).array;
			this.use = true;
			break;
		case TaskType.run:
			this.cmdArgs = cmdArgs;
			this.use = switches.canFind("-run") && canBeUnittested(srcPaths);
			if (this.use)
				this.cmdArgs ~= cmdArgs ~ "-d"; // don't show deprecations that already shown in check TaskType.chk
			break;
		}

		this.cwd = cwd;
		auto ppArgs = (ddmPath ? [ddmPath.str] : []) ~ [exe.str] ~ this.cmdArgs;
		debug writeln("args:", ppArgs.join(' '));
		this.redirect = redirect;

		// modify environment
		Environment env;
		const libmimallocPath = Path("~/.local/mimalloc-snapshot/lib/libmimalloc.so");
		const libmimallocAbsPath = Path(libmimallocPath.str.expandTilde);
		if (libmimallocAbsPath.str.exists) {
			// with LDC this reduces check time by 12.5%
			if (dbgFlag) dbg("xdmd: Overriding default C allocator with ", libmimallocPath);
			env["LD_PRELOAD"] = libmimallocAbsPath.str;
		}

		this.pp = pipeProcess(ppArgs, redirect, env);
	}
	TaskType tt;
	CmdArgs cmdArgs;
	bool use;
	DirPath cwd;
	ProcessPipes pp;
	char[] outLines;
	char[] errLines;
	Redirect redirect;
}

int main(scope Cmd cmd) {
	const argsOk = cmd.count("xdmd.d") <= 1 && cmd.count("-run") <= 1 && cmd.count("-main") <= 1;

	enforce(argsOk, "Potential self-recursion, where args: " ~ cmd.join(' '));

	// analyze D source arguments
	bool selfFlag = false;
	string[] srcPaths; // source file paths
	foreach (const ref c; cmd[1 .. $]) {
		if (!isDSourcePathCLIArgument(c))
			continue;
		if (c.baseName == __FILE__.baseName)
			selfFlag = true;
		srcPaths ~= c;
	}

	// analyze import paths
	string[] iDirs;
	CmdSwitches switches;
	foreach (const ref c; cmd[1 .. $]) {
		if (c.startsWith('-')) {
			if (const split = c.findSplitAfter("-I"))
				iDirs ~= split[1].expandTilde;
			else
				switches ~= c;
		}
	}

	// if (dbgFlag) dbg("iDirs: ", iDirs);
	// if (dbgFlag) dbg("switches: ", switches);
	// if (dbgFlag) dbg("srcPath: ", srcPaths);

	if (selfFlag) {
		// if (dbgFlag) dbg("xdmd: Skipping analysis of itself for now until self-recursion has been fixed");
		return 0;
	}

	const doRun = switches.canFind("-run") && canBeUnittested(srcPaths);
	const doCov = switches.canFind("-cov") && canBeUnittested(srcPaths);

	// Flags:
	const op = doRun ? Op.run : Op.chk;
	const cwd = DirPath(getcwd);

	// Scan for presence of compiler/tools/linter executables
	const exeLDC2 = findExecutable(FileName(`ldc2`));
	const exeLDMD2 = findExecutable(FileName(`ldmd2`));
	const exeDMD = findExecutable(FileName(`dmd`));
	const exeDscanner = findExecutable(FileName(`dscanner`));
	const exeRunD = FilePath("/home/per/Work/dmd/compiler/test/run.d");

	const onChk = (op == Op.chk || op == Op.all);
	const onRun = (op == Op.run || op == Op.all) && !selfFlag;
	const onLnt = (op == Op.run || op == Op.lnt || op == Op.all) && exeDscanner;
	const onDrn = false && exeRunD.str.exists; // TODO: Any source file is under
	const numOn = onChk + onRun + onLnt + onDrn;
	const onRdr = numOn >= 2;
	const redirect = onRdr ? Redirect.all : Redirect.init;

	scope(exit) {
		if (onRun && cmd.canFind(`-cov`) && srcPaths.length >= 1) {
			const lastLstFileName = srcPaths[$-1].baseName.stripExtension ~ ".lst";
			// clean up .lst files
			foreach (ref de; cwd.str.dirEntries(SpanMode.shallow)) {
				if (!de.isDir && de.name.endsWith(lstExt)) {
					const bn = de.name.baseName;
					if ((lastLstFileName && bn == lastLstFileName) ||
						bn == "__main.lst" ||
						bn.canFind("-")) {
						if (bn.getSize == 0) {
							// writeln("Removing ", de.name);
							de.name.remove();
							continue;
						}
						// TODO: functionize
						size_t cnt = 0;
						foreach (const line; File(de.name).byLine)
							if (line[7] == '|' || line[8] == '|' || line[9] == '|')
								cnt += 1;
						if (cnt >= 1) {
							// writeln("Removing ", de.name);
							de.name.remove();
							continue;
						}
					}
				}
			}
		}
	}

	const exeChk = either(exeDMD, exeLDMD2); // `ldmd2` used to be fastest at check but isn't anymore
	const exeRun = either(exeDMD, exeLDMD2); // `dmd` fastest at compiling/building
	const exeDrn = FilePath("/home/per/Work/dmd/compiler/test/run.d"); // TODO: Lookup relative path instead

	if (dbgFlag && onChk) dbg("xdmd: Checking on: using ", exeChk);
	if (dbgFlag && onRun) dbg("xdmd: Running on: using ", exeRun);
	if (dbgFlag && onLnt) dbg("xdmd: Linting on: using ", exeDscanner);
	if (dbgFlag && onLnt) dbg("xdmd: Running test on: using ", exeDrn);
	if (dbgFlag && onRdr) dbg("xdmd: Redirecting on");

	auto chk = onChk ? Task(TaskType.chk, exeChk, cmd, switches, srcPaths, cwd, redirect) : Task.init;
	auto run = onRun ? Task(TaskType.run, exeRun, cmd, switches, srcPaths, cwd, redirect) : Task.init;
	// linter
	auto lnt = onLnt ? Task(TaskType.lnt, exeDscanner, cmd, switches, srcPaths, cwd, Redirect.all) : Task.init;
	auto drn = onDrn ? Task(TaskType.drn, exeDrn, cmd, switches, srcPaths, cwd, Redirect.all) : Task.init;

	const bool chkExitEarlyUponFailure = false; // TODO: Doesn't seem to be needed at the moment.
	int chkES; // check exit status
	if (chk.use) {
		chkES = chk.pp.pid.wait();
		if (dbgFlag) dbg("xdmd: Check exit status: ", chkES);
		if (redirect != Redirect.init) {
			if (dbgFlag) dbg("xdmd: Check is redirected");
			foreach (ref ln; chk.pp.stdout.byLine.byMessage)
				if (const lnF = ln.filterDMDMessage)
					stdout.writeln(lnF, " [check]");
			foreach (ref ln; chk.pp.stderr.byLine.byMessage)
				if (const lnF = ln.filterDMDMessage)
					stderr.writeln(lnF, " [check]");
		}
		if (chkExitEarlyUponFailure && chkES) {
			if (dbgFlag) dbg("xdmd: Exiting eagerly because check failed, potentially aborting other phases");
			return chkES; // early failure return
		}
	}

	int lntES; // lint exit status
	if (lnt.use) {
		lntES = lnt.pp.pid.wait();
		if (lntES == -11)
			warn(exeDscanner, " failed with exit status ", lntES, " (segmentation fault)");
		if (dbgFlag) dbg("xdmd: Lint exit status: ", lntES);
		if (lnt.redirect != Redirect.init) {
			if (dbgFlag) dbg("xdmd: Lint is redirected");
			foreach (ref ln; lnt.pp.stdout.byLine.byMessage)
				if (const lnF = ln.filterDscannerMessage)
					stderr.writeln(lnF, " [lint]"); // forward to stderr for now
			foreach (ref ln; lnt.pp.stderr.byLine.byMessage)
				if (const lnF = ln.filterDscannerMessage)
					stderr.writeln(lnF, " [lint]"); // forward to stderr for now
		}
	}

	int runES; // run exit status
	if (run.use) {
		runES = run.pp.pid.wait();
		if (dbgFlag) dbg("xdmd: Run exit status: ", runES);
		if (redirect != Redirect.init) {
			if (dbgFlag) dbg("xdmd: Run is redirected");
			foreach (ref ln; run.pp.stdout.byLine.byMessage)
				if (const lnF = ln.filterDMDMessage)
					stdout.writeln(lnF, " [run]");
			foreach (ref ln; run.pp.stderr.byLine.byMessage)
				if (const lnF = ln.filterDMDMessage)
					stderr.writeln(lnF, " [run]");
		}
		if (runES) {
			// don't 'return runES here to let lntES complete
		}

		// TODO: show other files

		/+ Show coverage only upon no failures to prevent FlyCheck and other
		   checkers from choking and becoming disabled locally in buffer.
		   TODO: Adjust to scanning output for parse errors when
            `redirect != Redirect.init` as a exit status of one might signal
           something other than a failing `assert` or `enforce`. +/
		if (runES != 1 &&
			cmd.canFind(`-cov`)) {
			// process .lst files
			foreach (const srcPath; srcPaths) {
				if (!srcPath.isDSourcePathCLIArgument)
					continue;
				if (srcPath.isDMainOrNoUnittestsFile()) {
					stderr.writeln(srcPath, "(", 1, "): Coverage: Skipping analysis because of presence of `main` function and absence of any `unittest`s");
					continue;
				}
				const lst = srcPath.replace(`/`, `-`).stripExtension ~ lstExt;
				try {
					size_t nr;
					foreach (const line; File(lst).byLine) {
						if (line.startsWith(`0000000|`))
							stderr.writeln(srcPath, "(", nr + 1, "): Coverage: Line not covered by unitests");
						nr += 1;
					}
				} catch (Exception _) {
					// Ok if not exists
					if (dbgFlag) dbg("xdmd: Missing coverage file, ", lst);
				}
			}
		}
	}

	if (chkES != 0)
		return chkES;

	if (runES != 0)
		return runES;

	// don't care about lntES for now
	if (lntES != 0) {
		if (lntES == -11) {
		 	// ignore segmentation fault for now
		} else if (lntES == 1) { // there were warning
			// skip forwarding of "normal" exit status for now because it's only a linter
		} else {
			return lntES;
		}
	}

	return 0;
}

/++ Returns: Range over diagnostics messages in `lines`. +/
auto byMessage(Range)(Range lines) {
	static struct Result {
		void popFront() in(!empty) {
			_front = []; // reset message
			while (!_input.empty && !(_input.front.canFind(": Warning: ") || _input.front.canFind(": Error: ") || _input.front.canFind(": Coverage: "))) {
				if (_front.length)
					_front ~= '\n';
				_front ~= _input.front;
				_input.popFront();
			}
		}
	@property:
		bool empty() const scope => !_front;
		auto front() inout scope in(!empty) => _front;
	private:
		Range _input;
		string _front; ///< Current message.
	}
	return Result(lines);
}

bool canFindAmong(alias pred = eq, T)(in T[] haystack, in T[] needles) @trusted {
	static if (is(T : const(char)))
		foreach (const ref needle; needles)
			assert(needle < 128); // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
	if (haystack.length == 0)
		return false;
	// TODO: Extend to enum or use `StaticDenseSet`
	static if (T.sizeof == 1 && __traits(isSame, pred, eq)) {
		enum n = 2^^(8*T.sizeof); // number of possible `T` values
		size_t[n / size_t.sizeof] set; // set of needles to search
		import core.bitop : bt, bts;
		foreach (const ref needle; needles) // construct set
			bts(&set[0], cast(ubyte)needle);
		foreach (const ref elm; haystack) // use set
			if (bt(&set[0], cast(ubyte)elm))
				return true;
	} else {
		foreach (const ref elm; haystack)
			foreach (const ref needle; needles)
				if (pred(elm, needle))
					return true;
	}
	return false;
}

/// Returns: `true` iff `a` and `b` are equal.
bool eq(T)(T a, T b) {
	return a == b;
	version(none) {
		// TODO: Activate this or move to compiler: Find plain memory region of
		// plain old data to compare:
		enum isArray = is(T U : U[]);
		static if (!isArray && __traits(isPOD, T) && T.sizeof >= _sizeMin_memcmp) {
			import core.sys.linux.string : memcmp;
			return memcmp(&a, &b, T.sizeof) == 0;
		}
	}
}

/++ Filter DMD message `msg`. +/
private const(char)[] filterDMDMessage(return const(char)[] msg) pure /+nothrow+/ {
	if (msg.canFind("cannot inline function"))
		return [];
	return msg;
}

/++ Filter DScanner message `msg`. +/
private const(char)[] filterDscannerMessage(return const(char)[] msg) pure /+nothrow+/ {
	auto split = msg.findSplitAfter("Warning: ");
	if (!split)
		return [];
	auto rest = split[1];

	if ((rest.skipOver("Parameter _") || rest.skipOver("Variable _")) && rest.canFind("is never used"))
		return [];

	// See_Also: https://github.com/Dlang-UPB/D-scanner/issues/160
	if (rest.skipOver("Parameter ") && rest.canFind("is never used"))
		return msg ~ " Prefix parameter name with underscore to ignore";

	if (rest.skipOver("Variable ") && rest.canFind("is never used"))
		return msg ~ " Prefix variable name with underscore to ignore";

	if (rest.skipOver("Variable") && rest.canFind("is never modified and could have been declared const or immutable"))
		return []; // currently gives to many false positives

	if (rest.skipOver("Public declaration") && rest.canFind("is undocumented"))
		return [];

	if (rest.skipOver("Line is longer than") && rest.canFind("characters"))
		return [];

	if (rest.skipOver("Template name") && rest.canFind("does not match style guidelines"))
		return [];

	if (rest.canFind("has method 'opEquals', but not 'toHash'"))
		return [];

	// TODO: Remove when DScanner don't emit false positives for lint.
	if (rest.canFind("is never modified and could have been declared const or immutable"))
		return [];

	return msg;
}

private bool canBeUnittested(scope CmdArgs srcPaths) {
	foreach (const srcPath; srcPaths)
		if (srcPath.isDMainOrNoUnittestsFile())
			return false;
	return true;
}

private bool isDMainOrNoUnittestsFile(in char[] path) {
	if (!path.isDSourcePathCLIArgument)
		return false;
	const text = path.readText;
	// TODO: process (treesit) nodes of parseTree(text) instead
	return text.canFind("main(string[] args)") || !text.canFindAtLeastOneUnittest;
}

private bool canFindAtLeastOneUnittest(scope const(char)[] src) {
	while (auto split = src.findSplit("unittest")) {
		const prefix = split[0];
		const suffix = split[2];
		import std.ascii : isAlphaNum;
		const isAtSymbolStart = prefix.length == 0 || !(prefix[$-1].isAlphaNum || prefix[$-1] == '_');
		const isAtSymbolEnd = suffix.length == 0 || !(suffix[0].isAlphaNum || suffix[0] == '_');
		if (isAtSymbolStart && split[1].length != 0 && isAtSymbolEnd)
			return true;
		src = split[2];
	}
	return false;
}

private bool isDSourcePathCLIArgument(in char[] arg) @safe pure nothrow @nogc {
	return (!arg.startsWith('-')) && arg.endsWith(dExt);
}

void dbg(Args...)(scope auto ref Args args, in string file = __FILE_FULL_PATH__, const uint line = __LINE__) {
	stderr.writeln(file, "(", line, "):", " Debug: ", args, "");
}

void warn(Args...)(scope auto ref Args args, in string file = __FILE_FULL_PATH__, const uint line = __LINE__) {
	stderr.writeln(file, "(", line, "):", " Warning: ", args, "");
}

private string mkdirRandom() {
    const dirName = buildPath(tempDir(), "xdmd-" ~ randomUUID().toString());
    dirName.mkdirRecurse();
    return dirName;
}

/++ Path.

	The concept of a "pure path" doesn't need to be modelled in D as
	it has `pure` functions.  See
	https://docs.python.org/3/library/pathlib.html#pure-paths.

	See: SUMO:`ComputerPath`.
 +/
struct Path {
	this(string str) pure nothrow @nogc {
		this.str = str;
	}
	string str;
pure nothrow @nogc:
	bool opCast(T : bool)() const scope => str !is null;
	string toString() const @property => str;
}

/++ File (local) name.
 +/
struct FileName {
	this(string str, in bool normalize = false) pure nothrow @nogc {
		this.str = str;
	}
	string str;
	bool opCast(T : bool)() const scope pure nothrow @nogc => str !is null;
	string toString() inout return scope @property pure nothrow @nogc => str;
}

/++ (Regular) File path.
	See: https://hackage.haskell.org/package/filepath-1.5.0.0/docs/System-FilePath.html#t:FilePath
 +/
struct FilePath {
	this(string str) pure nothrow @nogc {
		this.path = Path(str);
	}
	Path path;
	alias path this;
}

struct DirPath {
	this(string str) pure nothrow @nogc {
		this.path = Path(str);
	}
	Path path;
	alias path this;
}

/++ Find path for `a` (or `FilePath.init` if not found) in `pathVariableName`.
	TODO: Add caching of result and detect changes via inotify.
 +/
private FilePath findExecutable(FileName a, scope const(char)[] pathVariableName = "PATH") {
	return findFileInPath(a, "PATH");
}

/++ Find path for `a` (or `FilePath.init` if not found) in `pathVariableName`.
	TODO: Add caching of result and detect changes via inotify.
 +/
FilePath findFileInPath(FileName a, scope const(char)[] pathVariableName) {
	import std.algorithm : splitter;
	import std.process : environment;
	const envPATH = environment.get(pathVariableName, "");
	foreach (const p; envPATH.splitter(':')) {
		import std.path : buildPath;
		const path = p.buildPath(a.str);
		if (path.exists)
			return FilePath(path); // pick first match
	}
	return typeof(return).init;
}
