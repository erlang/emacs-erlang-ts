# Contributing to erlang-ts

If you discover issues, have ideas for improvements or new features,
please report them to the [issue tracker][1] of the repository or
submit a pull request. Please, try to follow these guidelines when you
do so.

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `main`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Mention your Emacs version and operating system.
* Include any relevant code to the issue summary.

## Pull requests

* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][2].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Fix font-lock for ...`).
* Use the same coding conventions as the rest of the project.
* Verify your Emacs Lisp code with `checkdoc` (`C-c ? d`).
* [Squash related commits together][3].
* Open a [pull request][4] that relates to *only* one subject with a clear title
  and description in grammatically correct, complete sentences.

## Getting started

The project uses [Eldev][5] for building, testing, and linting. Make sure
you have it installed before proceeding.

### Installing Eldev

The simplest way to install Eldev:

```
curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh
```

See the [Eldev documentation][5] for alternative installation methods.

### Installing the tree-sitter grammar

erlang-ts requires the Erlang tree-sitter grammar. Install it from
within Emacs:

```
M-x treesit-install-language-grammar
Language: erlang
URL: https://github.com/WhatsApp/tree-sitter-erlang
```

Or add the grammar source to your config so it's always available:

```emacs-lisp
(add-to-list 'treesit-language-source-alist
             '(erlang "https://github.com/WhatsApp/tree-sitter-erlang"))
```

### Running the tests

The test suite uses [Buttercup][6]. Run it with:

```
eldev test
```

### Byte-compiling

```
eldev byte-compile
```

To treat warnings as errors (as CI does):

```
eldev byte-compile --warnings-as-errors
```

### Linting

```
eldev lint
```

This runs `checkdoc` and `package-lint` checks.

### All checks at once

To run the same checks CI runs:

```
eldev byte-compile --warnings-as-errors && eldev test && eldev lint
```

## Debugging

Use `M-x treesit-explore-mode` to inspect the tree-sitter syntax tree
and find the right node types for font-lock rules.

## Project layout

```
erlang-ts.el          Main package file
test/
  erlang-ts-font-lock-test.el      Font-lock tests (Buttercup)
  erlang-ts-indentation-test.el    Indentation tests (Buttercup)
  resources/
    sample.erl                     Sample Erlang file for testing
Eldev                 Build configuration
.github/
  workflows/
    ci.yml            GitHub Actions CI
```

[1]: https://github.com/erlang/emacs-erlang-ts/issues
[2]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[3]: http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html
[4]: https://help.github.com/articles/using-pull-requests
[5]: https://github.com/emacs-eldev/eldev
[6]: https://github.com/jorgenschaefer/emacs-buttercup
