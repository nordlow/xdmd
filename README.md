The application `xdmd.d` is a wrapper (script) around the D compilers `dmd` and
`ldmd` for automating the compilation, execution of unit tests, and coverage
analysis of D language source files. It supports dynamic handling of task types
such as checking, running, and linting, using tools like `Dscanner` and
`ldmd2`. The script also manages environment variables, dynamically selects
compilers, and processes output from test runs, ensuring efficient task
execution and debugging. It includes features for removing stale files and
adjusting compilation flags, particularly for sanitizers like
`-fsanitize=address`.
