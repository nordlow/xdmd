struct S {
	int x;
}

@safe pure unittest {
	// assert(S(1) == S(2));
}

struct T {
	int x;
	this(int x) nothrow {
		this.x = x;
	}
}
