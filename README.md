# erlang-ts — Emacs Erlang mode using tree-sitter

[![MELPA](https://melpa.org/packages/erlang-ts-badge.svg)](https://melpa.org/#/erlang-ts)
[![CI](https://github.com/erlang/emacs-erlang-ts/actions/workflows/ci.yml/badge.svg)](https://github.com/erlang/emacs-erlang-ts/actions/workflows/ci.yml)

Requires Emacs 29+ compiled with tree-sitter support.

`erlang-ts-mode` is a tree-sitter powered major mode for editing Erlang code.
It derives from [erlang-mode](https://github.com/erlang/otp/tree/master/lib/tools/emacs)
and progressively replaces its features with tree-sitter based implementations.

## Features

- Tree-sitter based font-locking (4 levels)
- Tree-sitter based indentation (experimental, opt-in)
- Embedded markdown highlighting in `-doc` and `-moduledoc` attributes (Emacs 30+)
- Imenu support for functions, macros, records, and types
- Easy grammar installation via `M-x erlang-ts-install-grammar`
- LSP integration (Eglot language ID configured automatically)
- Everything else inherited from erlang-mode (compilation, REPL, navigation, etc.)

## Installation

### MELPA

`erlang-ts` is available on [MELPA](https://melpa.org/#/erlang-ts). If you
have MELPA in your `package-archives`, install it with:

    M-x package-install <RET> erlang-ts <RET>

Or with `use-package`:

```emacs-lisp
(use-package erlang-ts
  :ensure t
  :mode ("\\.erl\\'" . erlang-ts-mode))
```

### From GitHub

You can install directly from the repository:

    M-x package-vc-install <RET> https://github.com/erlang/emacs-erlang-ts <RET>

Or with `use-package` on Emacs 30+:

```emacs-lisp
(use-package erlang-ts
  :vc (:url "https://github.com/erlang/emacs-erlang-ts" :rev :newest)
  :mode ("\\.erl\\'" . erlang-ts-mode))
```

The first time you open an Erlang file, the mode will offer to install the
tree-sitter grammar if it's not already available. You can also install it
manually with `M-x erlang-ts-install-grammar`.

## Customization

### Font-locking

`erlang-ts-mode` provides 4 levels of font-locking. The default level in
Emacs is 3. You can change it with:

```emacs-lisp
;; this font-locks everything erlang-ts supports
(setq treesit-font-lock-level 4)
```

You can also use `M-x erlang-font-lock-level-1` through
`M-x erlang-font-lock-level-4` to change the level interactively.

The font-lock features available at each level are:

**Level 1** (minimal):

- `string` — string literals
- `comment` — comments
- `keyword` — language keywords: `case`, `of`, `end`, `if`, `receive`, `try`, `catch`, `fun`, `begin`, `when`, etc.
- `doc` — doc attribute strings (`-doc`, `-moduledoc`)

**Level 2** (add preprocessor, definitions, types):

- `preprocessor` — module attributes, preprocessor directives, macro calls
- `operator-atoms` — operator keywords
- `definition` — function names, callback and spec definitions
- `type` — type names, record names, record field names

**Level 3** (default):

- `builtin` — built-in functions and guards
- `variable` — variables
- `guards` — guard expressions
- `function-call` — function calls
- `constant` — quoted atoms, char literals, predefined macros

**Level 4** (maximum detail):

- `operator` — operators: `->`, `||`, `+`, `-`, `>=`, etc.
- `remote-module` — module names in remote calls (`io:format`)
- `delimiter` — delimiters: `.`, `,`, `;`, `|`
- `bracket` — brackets: `()`, `[]`, `{}`, `<<>>`
- `number` — numeric literals
- `index-atom` — record field names in expressions, map keys

#### Selecting features

You don't have to use the level system. If you want fine-grained control over
what gets highlighted, you can cherry-pick individual features using
`treesit-font-lock-recompute-features`:

```emacs-lisp
(defun my-erlang-ts-font-lock-setup ()
  (treesit-font-lock-recompute-features
   ;; enable these features
   '(comment string keyword doc
     preprocessor definition type
     builtin variable constant
     operator number)
   ;; disable these features
   '(bracket delimiter)))

(add-hook 'erlang-ts-mode-hook #'my-erlang-ts-font-lock-setup)
```

#### Customizing faces

The faces used are standard `font-lock-*-face` faces, so any theme applies
automatically. To tweak specific faces in erlang-ts buffers only, use
buffer-local remapping:

```emacs-lisp
(add-hook 'erlang-ts-mode-hook
  (lambda ()
    (face-remap-add-relative 'font-lock-type-face
                             :foreground "DarkSeaGreen4")))
```

### Indentation

By default, `erlang-ts-mode` uses erlang-mode's classic indentation engine.
An experimental tree-sitter based indentation engine is also available.
Both respect the standard `erlang-indent-level` (default 4) and
`erlang-indent-guard` (default 2) variables.

To try the tree-sitter indentation, use `M-x erlang-ts-toggle-indent-function`
or set `erlang-ts-use-treesit-indent` to `t`.

#### Known indentation limitations

The tree-sitter indentation handles common Erlang constructs well but has
some known gaps compared to erlang-mode. Improvements are ongoing and
contributions are welcome!

##### Continuation lines

Multi-line expressions where a line continues an expression from the
previous line may not align correctly in all cases:

```erlang
%% Comprehension generators may misalign:
Var = [X ||
                  #record{a=X} <- lists:seq(1, 10),  %% expected
                  true = (X rem 2)]

%% Multi-line call arguments inside try/catch:
try
    io:format("~s ~s~n",
                      [St0#leex.xfile]),  %% expected
    parse_rules(Xfile, Line2, Macs, St2)
catch ...
```

##### Inline if clauses

When `if` has clauses on the same line, subsequent clauses use a slightly
different alignment than erlang-mode:

```erlang
%% erlang-mode aligns subsequent clauses at column 3 after `if':
    if Z >= 0 ->
            ok;
       Z =< 10 ->     %% column 7
            err
    end

%% tree-sitter aligns at erlang-indent-level:
    if Z >= 0 ->
            ok;
        Z =< 10 ->    %% column 8
            err
    end
```

##### Comma-first / pipe-first style

Code using comma-first or pipe-first formatting may not indent as expected:

```erlang
%% erlang-mode:
[ a
, b
, c
]

%% tree-sitter:
[ a
  , b
  , c
]
```

##### Records with `{` on a separate line

When the opening brace is on a different line from `-record(name,`:

```erlang
%% erlang-mode:
-record(record0,
        {           %% aligned after (
         r0a,
         r0b
        }).

%% tree-sitter doesn't handle this multi-line pattern yet
```

If you encounter indentation issues, you can always switch to erlang-mode's
indentation with `M-x erlang-ts-toggle-indent-function`.

### Markdown in doc attributes

`erlang-ts-mode` can highlight markdown syntax (bold, italic, code spans, links)
inside `-doc` and `-moduledoc` attributes using the `markdown-inline` tree-sitter
grammar. This requires Emacs 30+ and is enabled by default when the grammar is
available.

Install the grammar with `M-x erlang-ts-install-markdown-grammar`. To disable
markdown highlighting, set `erlang-ts-use-markdown-inline` to `nil`.

### LSP (Eglot) integration

`erlang-ts-mode` automatically configures the Eglot language ID for Erlang.
To use Eglot with [erlang_ls](https://github.com/erlang-ls/erlang_ls):

```emacs-lisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(erlang-ts-mode . ("erlang_ls"))))

(add-hook 'erlang-ts-mode-hook #'eglot-ensure)
```

## Relationship with erlang-mode

`erlang-ts-mode` derives from `erlang-mode` and inherits all of its
functionality. The following features are overridden with tree-sitter
implementations:

| Feature          | erlang-mode     | erlang-ts-mode           |
|------------------|-----------------|--------------------------|
| Font-locking     | Regex-based     | Tree-sitter (4 levels)   |
| Indentation      | Custom engine   | Both (see below)         |
| Imenu            | Regex-based     | Tree-sitter based        |
| Doc highlighting | Not available   | Embedded markdown-inline |

For indentation, `erlang-ts-mode` defaults to erlang-mode's engine. An
experimental tree-sitter indentation engine is available via
`erlang-ts-use-treesit-indent` (see [Indentation](#indentation)).

Everything else (compilation, REPL, shell, navigation, skeleton templates,
keybindings, etc.) is inherited directly from erlang-mode.

## Migrating from erlang-mode

### File associations

`erlang-ts-mode` registers the same file patterns as `erlang-mode` (`.erl`,
`.hrl`, `.xrl`, `.yrl`, `.escript`, etc.) during setup. If both modes are
installed, the last one to load wins. To ensure `erlang-ts-mode` takes
precedence, load it after `erlang-mode`:

```emacs-lisp
(use-package erlang-ts
  :ensure t
  :after erlang
  :mode ("\\.erl\\'" . erlang-ts-mode))
```

### What you gain

- Tree-sitter powered font-locking (more accurate, 4 configurable levels)
- Tree-sitter powered imenu
- Embedded markdown highlighting in doc attributes
- A foundation for future tree-sitter features (navigation, structural
  editing, etc.)

### What's the same

Since `erlang-ts-mode` derives from `erlang-mode`, all of its functionality
is preserved: compilation, REPL, shell, navigation, keybindings, skeleton
templates, and more.

### What to watch out for

- The tree-sitter indentation is experimental and opt-in. By default you
  get erlang-mode's indentation, so there should be no surprises.
- `erlang-ts-mode` requires the Erlang tree-sitter grammar to be installed.
  The mode will prompt you to install it on first use.

## Hacking

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup, how to run tests,
and guidelines for submitting changes. Architecture and design notes live in
[doc/DESIGN.md](doc/DESIGN.md).
