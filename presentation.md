# Presentation

- Syntax Checking via -o-
- Save => Verify
- Editor Integration such as Emacs FlyCheck or Vim Syntastic
- Tests
- Coverage
- Library code Architecture:
  - Each module has source accompaning with tests
- Diagnostics: `cov=diagnose`.
- Select tests based on purity (or even side-effects)
- Having `-unitest=modules...` would speed up `dmd -i`
