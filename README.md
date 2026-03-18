
# Emacs Erlang mode using treesitter #

Requires emacs-29 compiled with treesitter support.

Uses tree-sitter for syntax highlighting and indentation. Other features
(compilation, REPL, etc.) are inherited from erlang-mode.

There is a lot of work to do to convert the old erlang mode to use tree-sitter, but
by re-using the old one we can do the conversion little by little.

Help is appreciated.

# Install #

Add to your .emacs file:

```
 (add-to-list 'load-path "~/emacs-erlang-ts")
 (add-to-list 'treesit-language-source-alist '(erlang "https://github.com/WhatsApp/tree-sitter-erlang"))

 (use-package erlang-ts
     :mode ("\\.erl\\'" . erlang-ts-mode)
     :defer 't)
```
Install/compile erlang treesitter support (first time or update treesitter grammer):

```
  M-x treesit-install-language-grammar
  Language: erlang
```

# Customization #

Customize `treesit-font-lock-level` variable to increase/decrease the coloring,
or use `M-x erlang-font-lock-level-1-4` to change it.

## Indentation ##

By default, `erlang-ts-mode` uses tree-sitter based indentation. It respects
the standard `erlang-indent-level` (default 4) and `erlang-indent-guard` (default 2)
variables.

To switch back to the classic erlang-mode indentation engine, use
`M-x erlang-ts-toggle-indent-function` or set `erlang-ts-use-treesit-indent` to `nil`.

## Markdown in doc attributes ##

`erlang-ts-mode` can highlight markdown syntax (bold, italic, code spans, links)
inside `-doc` and `-moduledoc` attributes using the `markdown-inline` tree-sitter
grammar. This requires Emacs 30+ and is enabled by default when the grammar is
available.

Install the grammar with `M-x erlang-ts-install-markdown-grammar`. To disable
markdown highlighting, set `erlang-ts-use-markdown-inline` to `nil`.

# Hacking #

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup, how to run tests,
and guidelines for submitting changes.

