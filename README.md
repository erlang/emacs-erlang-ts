
# Emacs Erlang mode using treesitter #

Requires emacs-29 compiled with treesitter support.

Currently uses treesitter only for syntax-highlighting, *font-lock-mode*, and uses the old
erlang-mode for everything else.

There is a lot of work to do to convert the old erlang mode to use treesitter, but
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




