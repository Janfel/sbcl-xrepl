# sbcl-xrepl
Improved REPL for SBCL

## Prerequisites
This package depends on [cl-readline](https://github.com/vindarel/cl-readline), so make sure that [quicklisp](https://www.quicklisp.org) is loaded.

## Installation

```sh
git clone https://github.com/Janfel/sbcl-xrepl/ ~/common-lisp/sbcl-xrepl/
```

Add this to your `~/.sbclrc`:

```common-lisp
(when (interactive-stream-p *standard-input*)
  (asdf:load-system "sbcl-xrepl"))
```
