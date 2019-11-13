# About the Spec

The Neutrino project derives its spec from its documentation. It users a formal language, atop Markdown, to build an executable spec. It allows for code examples to be executed as tests.

This document serves a double purpose. First, it documents this formal specification language, and second, it is used for testing it.

## Successful Compilation

Since GitHub's syntax highlighting does not support neutrino, we mark Neutrino code blocks as `prolog` code. A `prolog` code block preceded by a line including the phrase `compiles successfully`, ending with a colon (`:`), will be considered a successful scenario test. This test will extract the enclosed code as a file, call the Neutrino compiler on it, and expect success.

The following is not a test:
```prolog
% Some Neutrino code here...
```

But the following is a test, asserting it compiles successfully:
```prolog
% Some Neutrino code that must compile.
```

Of course, further ```prolog``` code fragments are not considered tests.
```prolog
% This is not a test.
```
