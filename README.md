# xdmd

## Description
The application `xdmd.d` is a wrapper (script) around the D compilers `dmd` and
`ldmd` that automatically compiles and executes unittests along with diagnostics
of accompaning coverage analysis results.

## Usage

`./xdmd -checkaction\=context -allinst -unittest -cov -cov=ctfe -main -run test.d`

## TODO
- Modify `dmd` to emit warning-style diagnostics instead of `.lst` files via say
  `-cov=diagnose`.
