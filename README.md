# jest-coverage.el
Emacs minor mode for displaying code coverage from jest JavaScript framework.

## Requirement

[Highlight.el](https://www.emacswiki.org/emacs/highlight.el) by Drew Adams

## Limitations

It only work for git controlled repositories (it take root git directory out of git shell
command and use this to get coverage file in coverage directory).

### Usage

```
(require 'jest-coverage)
```

then run

```
M-x jest-coverage-mode
```

Enabling the mode is very slow, if your JavaScript source file that's big. To refresh the view
after made changes to coverage file you can call `M-x jc/mark-buffer`.

## License

Copyright (C) 2018 Jakub Jankiewicz
Released under GPLv3 license

