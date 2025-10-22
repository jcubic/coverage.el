# coverage.el
Emacs minor mode for displaying code coverage.

## Supporting Languages:

* `jest` - JavaScript framework (also with TypeScript using `ts-jest`) require running `jest --coverage`,
* `vitest` - works with JavaScript and TypeScript (require adding `json` reporter)
* phpunit - xml file generated using coverage-clover option.

## Requirement

[Highlight.el](https://www.emacswiki.org/emacs/highlight.el) by Drew Adams

plus default `xml` and `json` packages

## Limitations

It only work for git controlled repositories (it take root git directory out of git shell
command and use this to get coverage file in coverage directory).

### Usage

```
(require 'coverage)
```

then run

```
M-x coverage-mode
```

Enabling the mode is very slow, if your JavaScript source file is big. To refresh the view
after made changes to coverage file you can call `M-x jc/mark-buffer`.

To change colors

```
;; if you're using light theme
(face-spec-set 'jc/covered '((t :background "light green")))
(face-spec-set 'jc/not-covered '((t :background "light red")))
```

## License

Copyright (C) 2018-2023 [Jakub T. Jankiewicz](https://jcubic.pl/me)<br/>
Released under GPLv3 license

