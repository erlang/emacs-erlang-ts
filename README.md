
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

By default, `erlang-ts-mode` uses erlang-mode's classic indentation engine.
An experimental tree-sitter based indentation engine is also available.
Both respect the standard `erlang-indent-level` (default 4) and
`erlang-indent-guard` (default 2) variables.

To try the tree-sitter indentation, use `M-x erlang-ts-toggle-indent-function`
or set `erlang-ts-use-treesit-indent` to `t`.

### Known indentation limitations ###

The tree-sitter indentation handles common Erlang constructs well but has
some known gaps compared to erlang-mode. Improvements are ongoing and
contributions are welcome!

#### Continuation lines ####

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

#### Inline if clauses ####

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

#### Comma-first / pipe-first style ####

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

#### Records with `{` on a separate line ####

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

