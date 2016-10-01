# sablecc-mode
An Emacs major mode for editing SableCC grammar specifications.

## Install

Ideally, when this is done, it will be submitted to MELPA as a package. Until then...

1. Clone the repo
2. Move `sablecc-mode.el` to a folder on your load-path. You can add a folder to your load path by adding the line `(add-to-list 'load-path "path/to/neverland/")` to your `.emacs` file.
3. Add `(require 'sablecc-mode)` to your `.emacs` file.

## Functions

### `sablecc-compile-buffer`
Compiles the current buffer with SableCC. Prompts for arguments to SableCC. Can be run with `C-c C-c`.

### `sablecc-compile-file`
Prompts for a file path and runs SableCC. Prompts for arguments to SableCC. Can be run with `C-c C-f`

Note that if `--pretty-print` is provided as an argument, a new `sablecc-mode` buffer is created with the output. For all runs, a temporary buffer with the output of SableCC (to `stdout`), called `*sablecc-info*`, is opened.

## Issues
	- M-j doesn't work correctly in comments
	- Multiline comment indentation doesn't seem to get it right all the time.
	- Error messages from java may not work correctly.
	- Could use some nicer highlighting for AST names etc.
