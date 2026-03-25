# erlang-ts Design Notes

## Architecture

`erlang-ts-mode` derives from `erlang-mode` and incrementally replaces its
features with tree-sitter implementations.  This allows users to benefit from
tree-sitter font-locking and indentation while keeping all of erlang-mode's
battle-tested functionality (compilation, REPL, navigation, etc.).

The mode setup happens in two stages:

1. `erlang-mode` initializes everything (keybindings, syntax table, comment
   handling, etc.)
2. `erlang-ts-setup` overrides font-lock, imenu, indentation, and
   syntax-propertize with tree-sitter implementations

## Lazy initialization

Top-level `defvar` forms that call `treesit-font-lock-rules` or
`treesit-query-compile` would fail during byte-compilation if the tree-sitter
grammar isn't installed.  To avoid this, font-lock rules and the
syntax-propertize query are computed lazily on first use:

```elisp
(defvar erlang-ts-font-lock-rules nil
  "Computed lazily on first use.")

(defun erlang-ts--build-font-lock-rules ()
  "Build tree-sitter font-lock rules for Erlang."
  (treesit-font-lock-rules ...))

;; In erlang-ts-setup:
(unless erlang-ts-font-lock-rules
  (setq erlang-ts-font-lock-rules (erlang-ts--build-font-lock-rules)))
```

This follows the same pattern used by
[neocaml](https://github.com/bbatsov/neocaml) and avoids issues with
byte-compilation across different Emacs versions.

## Font-locking

Font-lock rules are defined in `erlang-ts--build-font-lock-rules` using
`treesit-font-lock-rules`.  The features are organized into 4 levels:

- **Level 1**: string, comment, keyword, doc
- **Level 2**: preprocessor, operator-atoms, definition, type
- **Level 3** (default): builtin, variable, guards, function-call, constant
- **Level 4**: operator, remote-module, delimiter, bracket, number, index-atom

The rules use Erlang-specific predicates like `erlang-ts-in-type-context-p`
(to fontify type names only in type contexts) and
`erlang-ts-predefined-macro-p` (to highlight built-in macros).

## Indentation

Tree-sitter indentation is driven by `treesit-simple-indent-rules` — a list
of `(MATCHER ANCHOR OFFSET)` triples tried in order.  The first matching rule
wins.

The rules in `erlang-ts--indent-rules` are roughly ordered as follows:

1. **Comment conventions** — `%%%` at column 0, `%%` context-dependent,
   `%` at `comment-column`
2. **String preservation** — `no-indent` for lines inside strings to avoid
   changing string values
3. **Top-level** — `(parent-is "source_file")` pins everything at column 0
4. **Closing delimiters** — `end`, `)`, `]`, `}` align with their opening
   counterpart
5. **Keyword alignment** — `catch` aligns with `try`, `receive_after` aligns
   with `receive`
6. **Clause body** — special double-indent for `fun_clause`, `receive_after`,
   and inline `if_clause` bodies
7. **Block constructs** — children of `case_expr`, `if_expr`, `try_expr`,
   `receive_expr`, etc.
8. **Record/map fields** — align after `{`
9. **Function arguments** — smart alignment: after `(` when content follows,
   `erlang-argument-indent` from function name when `(` is at EOL (Okeefe
   style)
10. **Collections** — align after opening delimiter (`[`, `{`)
11. **Multi-line expressions** — `binary_op_expr`, `match_expr`, `pipe`, etc.
12. **Type specs** — body indented at 2x `erlang-indent-level`
13. **Error recovery** — `(parent-is "ERROR")` provides basic indentation for
    incomplete code
14. **Catch-all** — `(no-node prev-line 0)` preserves previous line's
    indentation

### Custom anchors

Several custom anchor functions handle Erlang-specific alignment:

- `erlang-ts--anchor-matching-open` — matches closing delimiters to their
  openers (`)` to `(`, `]` to `[`, `}` to `{`)
- `erlang-ts--anchor-after-open-delim` — returns position after the first
  `(`, `[`, or `{` in the parent node
- `erlang-ts--anchor-args` — smart function argument alignment that detects
  Okeefe style (paren at end of line)
- `erlang-ts--anchor-after-open-brace` — `{`-specific for records/maps
  (skips `(` which comes first in `-record(name, {...})`)
- `erlang-ts--anchor-after-open-bracket` — `[`-specific for export/import
  attributes (skips `(` in `-export([...])`)
- `erlang-ts--grand-parent-bol` — like `parent-bol` but one level up in the
  tree

### Custom matchers

- `erlang-ts--match-clause-body-in` — matches clause_body children inside a
  specific grandparent type (e.g., `fun_clause`, `receive_after`)
- `erlang-ts--match-inline-clause-body` — matches clause_body when the clause
  starts on the same line as the enclosing block keyword (for double-indent)
- `erlang-ts--match-triple-comment` / `erlang-ts--match-single-comment` —
  detect `%%%` and `%` comments for special indentation

### Known limitations

The main gap is **continuation lines** — cases where a multi-line expression
wraps and the tree-sitter node at BOL is deep inside a node that started on
the previous line.  The `treesit-simple-indent-rules` system doesn't have a
native concept of "this line is a continuation," and erlang-mode uses
heuristics to detect this.

The **ERROR node handling** is also basic — when code is incomplete during
editing, tree-sitter produces ERROR nodes and the indentation falls back to
`parent-bol + erlang-indent-level`, which doesn't always produce the right
result.  Improving this would require context-aware ERROR handling (detecting
whether the ERROR is inside a case, function, etc.).

## Embedded markdown in doc attributes

The `-doc` and `-moduledoc` attributes contain markdown text.  On Emacs 30+,
`erlang-ts-mode` uses tree-sitter language injection to highlight markdown
syntax inside these strings.

The implementation uses `:local t` range settings, which creates local
(per-range) parsers managed internally by treesit.  This means:

- Font-lock works correctly (bold, italic, code spans, links are highlighted)
- The markdown-inline parsers don't appear in `(treesit-parser-list)` (only
  global parsers are listed)
- `erlang-ts--language-at-point` may not detect the local parsers (this is a
  treesit limitation, not a bug)

The feature is gated on `(>= emacs-major-version 30)` because the `:local`
keyword in `treesit-range-rules` was added in Emacs 30.

## Tips for development

### Reloading code

The lazy initialization pattern means that `eval-buffer` on `erlang-ts.el`
won't update font-lock rules or indent rules in existing buffers (they're
cached in buffer-local variables set during mode setup).  To fully reload:

1. `M-x eval-buffer` on `erlang-ts.el`
2. Reset cached values: `(setq erlang-ts-font-lock-rules nil)`
3. Re-enable the mode: `M-x erlang-ts-mode`

Or simply close and reopen the Erlang buffer.

### Adding new indentation rules

- Use `M-x treesit-explore-mode` to inspect the AST at point
- Set `treesit--indent-verbose` to `t` to see which rules match during
  indentation
- Order matters: more specific rules must come before general ones
- The `parent-bol` anchor resolves to the first non-whitespace column on the
  parent node's starting line
- `parent-is` uses **regexp matching**, so `"binary"` matches both `binary`
  and `binary_op_expr` — use `"^binary$"` for exact matches
- Test new rules with `eldev test`

### Running tests

```
eldev byte-compile --warnings-as-errors
eldev test
eldev lint
```

## Sources of inspiration

- [neocaml](https://github.com/bbatsov/neocaml) — tree-sitter OCaml mode
  (indentation architecture, lazy init, embedded markdown)
- [clojure-ts-mode](https://github.com/clojure-emacs/clojure-ts-mode) —
  embedded markdown in docstrings
- [erlang-mode](https://github.com/erlang/otp/tree/master/lib/tools/emacs) —
  the parent mode

## References

- [Parsing Program Source](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html)
- [Tree-sitter Major Modes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tree_002dsitter-Major-Modes.html)
- [Parser-based Indentation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html)
- [WhatsApp/tree-sitter-erlang](https://github.com/WhatsApp/tree-sitter-erlang) — the Erlang tree-sitter grammar
