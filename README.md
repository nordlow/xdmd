# xdmd

## Description
The application `xdmd.d` is a wrapper (script) around the D compilers `dmd` and
`ldmd` that automatically compiles and executes unittests along with diagnostics
of accompaning coverage analysis results.

## Usage

`./xdmd -checkaction=context -allinst -unittest -cov -cov=ctfe -main -run test.d`

## TODO
- Use ASan enable LDC call aswell.
- Extend `dmd` to emit warning-style diagnostics instead of `.lst` files via say
  `-cov=diagnose`.
- Extend `dmd`to support `-unitest=modules...` to speed up `dmd -i`.
