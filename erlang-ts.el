;;; erlang-ts.el --- Major modes for editing Erlang -*- lexical-binding: t; -*-

;; %CopyrightBegin%
;;
;; Copyright Ericsson AB 2024-2025. All Rights Reserved.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; %CopyrightEnd%
;;

;; Author: Dan Gudmundsson
;; Keywords: erlang, languages, treesitter
;; URL: https://github.com/erlang/emacs-erlang-ts
;; Package-Requires: ((emacs "29.2") (erlang "27.2"))
;; Package-Version: 0.3

;;; Commentary:

;; `erlang-ts-mode' is a tree-sitter powered major mode for editing Erlang.
;; It derives from `erlang-mode' and progressively replaces its features
;; with tree-sitter based implementations.
;;
;; Features:
;; - Tree-sitter based font-locking (4 levels)
;; - Tree-sitter based indentation (experimental, opt-in)
;; - Embedded markdown highlighting in doc attributes (Emacs 30+)
;; - Imenu support for functions, macros, records, and types
;; - Everything else inherited from erlang-mode
;;
;; Install the tree-sitter grammar with:
;;   M-x erlang-ts-install-grammar
;;
;; See the README for full documentation.

;;; Code:

(require 'treesit)
(require 'erlang)

;;; Grammar installation

(defconst erlang-ts-grammar-recipes
  '((erlang "https://github.com/WhatsApp/tree-sitter-erlang")
    (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                     nil "tree-sitter-markdown-inline/src"))
  "Tree-sitter grammar recipes for Erlang and Markdown-Inline.
Each entry is a list suitable for `treesit-language-source-alist'.")

(defconst erlang-ts-grammar-recipe
  (car erlang-ts-grammar-recipes)
  "Tree-sitter grammar recipe for Erlang.
A list of (LANGUAGE URL) suitable for use in
`treesit-language-source-alist'.")

;;;###autoload
(defun erlang-ts-install-grammar (&optional force)
  "Install the Erlang tree-sitter grammar if not already available.
With prefix argument FORCE, reinstall even if already installed.
This is useful after upgrading to a version that requires a newer
grammar."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'erlang nil)))
    (message "Installing Erlang tree-sitter grammar...")
    (let ((treesit-language-source-alist (list erlang-ts-grammar-recipe)))
      (treesit-install-language-grammar 'erlang))))

;;;###autoload
(defun erlang-ts-install-markdown-grammar (&optional force)
  "Install the markdown-inline tree-sitter grammar.
This enables rich markdown highlighting inside -doc and -moduledoc
attributes.  With prefix argument FORCE, reinstall even if already
installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'markdown-inline nil)))
    (message "Installing markdown-inline tree-sitter grammar...")
    (let ((treesit-language-source-alist erlang-ts-grammar-recipes))
      (treesit-install-language-grammar 'markdown-inline))))

;; Override erlang font-lock functions
;; So the menus (and functions) work as expected
(defun erlang-ts--font-lock-level-1 (func &rest args)
  "To be used as :around advice.
FUNC with ARGS will be called if `erlang-ts-mode' is not active."
  (if (derived-mode-p 'erlang-ts-mode)
      (erlang-ts-set-font-lock-level 1)
    (apply func args)))

(defun erlang-ts--font-lock-level-2 (func &rest args)
  "To be used as :around advice.
FUNC with ARGS will be called if `erlang-ts-mode' is not active."
  (if (derived-mode-p 'erlang-ts-mode)
      (erlang-ts-set-font-lock-level 2)
    (apply func args)))

(defun erlang-ts--font-lock-level-3 (func &rest args)
  "To be used as :around advice.
FUNC with ARGS will be called if `erlang-ts-mode' is not active."
  (if (derived-mode-p 'erlang-ts-mode)
      (erlang-ts-set-font-lock-level 3)
    (apply func args)))

(defun erlang-ts--font-lock-level-4 (func &rest args)
  "To be used as :around advice.
FUNC with ARGS will be called if `erlang-ts-mode' is not active."
  (if (derived-mode-p 'erlang-ts-mode)
      (erlang-ts-set-font-lock-level 4)
    (apply func args)))

(defun erlang-ts-set-font-lock-level (level)
  "Fontify current buffer to LEVEL."
  (setq treesit-font-lock-level level)
  (treesit-font-lock-recompute-features)
  (treesit-font-lock-fontify-region (point-min) (point-max)))

(defvar erlang-ts-keywords
  '("after"
    "begin"
    "catch"
    "case"
;;    "cond"   ;; reserved but not included in tree-sitter grammer
    "end"
    "fun"
    "if"
;;    "let"    ;; reserved but not included in tree-sitter grammer
    "of"
    "receive"
    "try"
    "maybe"
    "else"
    "when")
  "Erlang reserved keywords.")

(defun erlang-ts--build-font-lock-rules ()
  "Build tree-sitter font-lock rules for Erlang."
  (treesit-font-lock-rules
   :language 'erlang
   :feature 'doc
   `((wild_attribute name: (attr_name name: (atom)
                                      @name (:equal "doc" @name))
                     value: (string) @font-lock-doc-face)
     (wild_attribute name: (attr_name name: (atom)
                                      @name (:equal "moduledoc" @name))
                     value: (string) @font-lock-doc-face))

   :language 'erlang
   :feature 'string
   `((string) @font-lock-string-face)

   :language 'erlang
   :feature 'comment
   `((comment) @font-lock-comment-face)

   :language 'erlang
   :feature 'keyword
   `([,@erlang-ts-keywords] @font-lock-keyword-face)

   :language 'erlang
   :feature 'operator-atoms
   ;; We use keyword-face here because by default
   ;; operator-font is black and we want to differ between atoms and builtins
   `([,@erlang-operators] @font-lock-keyword-face)

   :language 'erlang
   :feature 'type
   :override t
   `(  ;; Might be slow but don't know a better way to do it
     (call expr: (_) @font-lock-type-face
           (:pred erlang-ts-in-type-context-p @font-lock-type-face))
     (type_name name: (atom) @font-lock-type-face)
     (export_type_attribute types: (fa fun: (atom) @font-lock-type-face))
     (record_decl name: (atom) @font-lock-type-face
                  (record_field name: (atom) @font-lock-property-name-face))
     ;; for records without fields e.g
     ;; `-record(name, {}).`
     (record_decl name: (atom) @font-lock-type-face)
     (record_name name: (atom) @font-lock-type-face))

   :language 'erlang
   :feature 'definition
   `((function_clause name: (atom) @font-lock-function-name-face)
     (callback fun: (atom) @font-lock-function-name-face)
     (spec fun: (atom) @font-lock-function-name-face)
     (fa fun: (atom) @font-lock-function-name-face)
     (binary_op_expr lhs: (atom) @font-lock-function-name-face "/"
                     rhs: (integer))
     (internal_fun fun: (atom) @font-lock-function-name-face)
     (external_fun module: (module name: (atom) @font-lock-function-name-face)
                   fun: (atom) @font-lock-function-name-face))

   :language 'erlang
   :feature 'guards
   `((call expr: (atom) @font-lock-builtin-face
           (:match ,erlang-guards-regexp @font-lock-builtin-face)))

   :language 'erlang
   :feature 'builtin
   `((call expr: (atom) @font-lock-builtin-face
           (:match ,erlang-int-bif-regexp @font-lock-builtin-face))
     (call expr:
           (remote module:
                   (remote_module module: (atom)
                                  @module (:equal "erlang" @module))
                   fun: (atom) @fun (:match ,erlang-ext-bif-regexp @fun))
           @font-lock-builtin-face)
     (call expr: (atom) @font-lock-builtin-face
           (:match ,erlang-guards-regexp @font-lock-builtin-face)))

   :language 'erlang
   :feature 'preprocessor
   :override t
   `((wild_attribute name: (_) @font-lock-preprocessor-face)
     (module_attribute (["-" "module"]) @font-lock-preprocessor-face)
     (behaviour_attribute (["-" "behaviour" "behavior"]) @font-lock-preprocessor-face)
     (deprecated_attribute (["-" "deprecated"]) @font-lock-preprocessor-face)
     (export_attribute (["-" "export"]) @font-lock-preprocessor-face)
     (import_attribute (["-" "import"]) @font-lock-preprocessor-face)
     (export_type_attribute (["-" "export_type"]) @font-lock-preprocessor-face)
     (compile_options_attribute (["-" "compile"]) @font-lock-preprocessor-face)
     (file_attribute (["-" "file"]) @font-lock-preprocessor-face)
     (feature_attribute (["-" "feature"]) @font-lock-preprocessor-face)
     (optional_callbacks_attribute (["-" "optional_callbacks"]) @font-lock-preprocessor-face)

     (pp_define (["-" "define"]) @font-lock-preprocessor-face)
     (pp_include (["-" "include"]) @font-lock-preprocessor-face)
     (pp_include_lib (["-" "include_lib"]) @font-lock-preprocessor-face)
     (pp_undef (["-" "undef"]) @font-lock-preprocessor-face)
     (pp_ifdef (["-" "ifdef"]) @font-lock-preprocessor-face)
     (pp_ifndef (["-" "ifndef"]) @font-lock-preprocessor-face)
     (pp_else (["-" "else"]) @font-lock-preprocessor-face)
     (pp_endif (["-" "endif"]) @font-lock-preprocessor-face)
     (pp_if (["-" "if"]) @font-lock-preprocessor-face)
     (pp_elif (["-" "elif"]) @font-lock-preprocessor-face)

     (record_decl (["-" "record"]) @font-lock-preprocessor-face)
     (macro_call_expr name: (_) @font-lock-preprocessor-face)
     (callback (["-" "callback"]) @font-lock-preprocessor-face)

     (type_alias (["-" "type"]) @font-lock-preprocessor-face)
     (opaque (["-" "opaque"]) @font-lock-preprocessor-face)
     (spec (["-" "spec"]) @font-lock-preprocessor-face))

   :language 'erlang
   :feature 'constant
   :override t
   `((macro_call_expr name: (var) @font-lock-constant-face
                      (:pred erlang-ts-predefined-macro-p @font-lock-constant-face))

     ((atom) @font-lock-constant-face (:match ,erlang-atom-quoted-regexp @font-lock-constant-face))
     ((char) @font-lock-constant-face))

   :language 'erlang
   :feature  'index-atom
   `((record_field_name name: (atom) @font-lock-property-use-face)
     (record_field name: (atom) @font-lock-property-use-face)
     (map_field key: (_) @font-lock-property-use-face))

   :language 'erlang
   :feature 'number
   `((integer) @font-lock-number-face
     (float) @font-lock-number-face)

   :language 'erlang
   :feature 'variable
   `((var) @font-lock-variable-name-face)

   :language 'erlang
   :feature 'remote-module
   :override t
   `((call expr: (remote module: (remote_module module: (atom) @font-lock-constant-face)
                         fun: (atom) @font-lock-function-name-face))
     (external_fun module: (module name: (atom) @font-lock-constant-face))
     (remote module: (remote_module module: (atom) @font-lock-constant-face)))

   :language 'erlang
   :feature 'function-call
   `((call expr:  (_) @font-lock-function-call-face))

   :language 'erlang
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}" "<<" ">>"]) @font-lock-bracket-face)

   :language 'erlang
   :feature 'delimiter
   '((["." "," ";" "|"]) @font-lock-delimiter-face)

   :language 'erlang
   :feature 'operator
   '(([ "->" "||" "<-" "<=" "+" "-" "*" "/" "++"
        ">" ">=" "<" "=<" "=" "==" "=:=" "=/="
        "<:-" "<:=" "&&"])
     @font-lock-operator-face)))

(defvar erlang-ts-font-lock-rules nil
  "Tree-sitter font-lock settings for `erlang-ts-mode'.
Computed lazily on first use.  Use `treesit-font-lock-level' or
`treesit-font-lock-feature-list' to change settings.")

(defun erlang-ts-in-type-context-p (node)
  "Check if NODE is within a type definition context."
  (when node
    (let ((parent (treesit-node-parent node)))
      (cond
       ((null parent) nil)
       ((member (treesit-node-type parent)
                '("type_alias" "ann_type" "type_sig" "opaque" "field_type")) t)
       (t (erlang-ts-in-type-context-p parent))))))

(defun erlang-ts-predefined-macro-p (node)
  "Check if macro_call_expr var NODE is a builtin macro."
  (when node
    (if (member (treesit-node-text node)
                '("OTP_RELEASE" "MACHINE"
                  "MODULE" "MODULE_STRING"
                  "FILE" "LINE"
                  "FUNCTION_NAME" "FUNCTION_ARITY"
                  "FEATURE_AVAILABLE" "FEATURE_ENABLED"))
        t
      nil)))

(defvar erlang-ts--syntax-propertize-query nil
  "Compiled tree-sitter query for `syntax-propertize'.
Computed lazily on first use.")

(defun erlang-ts--build-syntax-propertize-query ()
  "Build and cache the `syntax-propertize' query."
  (unless erlang-ts--syntax-propertize-query
    (when (treesit-language-available-p 'erlang)
      (setq erlang-ts--syntax-propertize-query
            (treesit-query-compile
             'erlang
             '(((char) @node-char)
               ((atom) @node-atom)
               ((string) @node-string-triple-quoted
                         (:match "^\"\"\"" @node-string-triple-quoted))
               ((string) @node-string))))))
  erlang-ts--syntax-propertize-query)

(defun erlang-ts--process-node (node)
  "Process a single or double quoted string or atom node.
NODE is the treesit node to process."
  (let* ((node-text (treesit-node-text node))
         (node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (first-char (aref node-text 0))
         (last-char (aref node-text (1- (length node-text)))))
    (when (and (or (eq first-char ?\") (eq first-char ?\'))
               (eq first-char last-char))
      (let ((escaped-last-quote (and (eq last-char ?\")
                                    (> (length node-text) 1)
                                    (eq (aref node-text (- (length node-text) 2)) ?\\))))
        (put-text-property node-start (1+ node-start) 'syntax-table (string-to-syntax "|"))
        (put-text-property (1- node-end) node-end 'syntax-table (string-to-syntax "|"))
        (unless escaped-last-quote
          (put-text-property (1- node-end) node-end 'syntax-table (string-to-syntax "|")))
        (let ((content-start (1+ node-start))
              (content-end (1- node-end)))
          (when (> content-end content-start)
            (put-text-property content-start content-end 'syntax-table (syntax-table))))))))

(defun erlang-ts--process-node-char (node)
  "Process char NODE like `$\'' or `$\"'."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node)))
    (when (> node-end node-start)
      (let ((custom-table (copy-syntax-table (syntax-table))))
        (modify-syntax-entry ?' "w" custom-table)
        (modify-syntax-entry ?\" "w" custom-table)
        (put-text-property node-start node-end 'syntax-table custom-table)))))

(defun erlang-ts--process-node-triple-quoted (node)
  "Process a triple quoted string node.
NODE is the treesit node to process."
  (let* ((node-text (treesit-node-text node))
         (node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (text-length (length node-text)))
    (put-text-property node-start (+ node-start 3) 'syntax-table (string-to-syntax "|"))
    (when (>= text-length 3)
      (put-text-property (- node-end 3) node-end 'syntax-table (string-to-syntax "|")))
    (let ((content-start (+ node-start 3))
          (content-end (- node-end 3)))
      (when (> content-end content-start)
        (put-text-property content-start content-end 'syntax-table (syntax-table))))))

(defun erlang-ts--syntax-propertize (start end)
  "Apply syntax properties for Erlang specific patterns from START to END."
  (let ((captures
         (treesit-query-capture 'erlang (erlang-ts--build-syntax-propertize-query) start end)))
    (pcase-dolist (`(,name . ,node) captures)
      (pcase name
        ('node-char   (erlang-ts--process-node-char node))
        ('node-atom   (erlang-ts--process-node node))
        ('node-string (erlang-ts--process-node node))
        ('node-string-triple-quoted (erlang-ts--process-node-triple-quoted node))))))

(defvar erlang-ts-mode-syntax-table nil
  "Syntax table in use in Erlang-ts-mode buffers.")

(defun erlang-ts-syntax-table-init ()
  "Initialize the syntax table for `erlang-ts-mode'."
  (unless erlang-ts-mode-syntax-table
    (let ((table (copy-syntax-table erlang-mode-syntax-table)))
      (modify-syntax-entry ?$ "w" table)
      (setq erlang-ts-mode-syntax-table table)))
  (set-syntax-table erlang-ts-mode-syntax-table))

;;; Embedded markdown in doc attributes

(defcustom erlang-ts-use-markdown-inline t
  "When non-nil, use markdown-inline grammar for doc attribute highlighting.
This provides rich highlighting of markdown syntax (bold, italic,
code spans, links, etc.) inside -doc and -moduledoc attributes.
Requires the markdown-inline tree-sitter grammar to be installed."
  :type 'boolean
  :group 'erlang)

(defvar erlang-ts--markdown-font-lock-rules nil
  "Font-lock rules for markdown-inline in doc attributes.
Computed lazily on first use.")

(defun erlang-ts--build-markdown-font-lock-rules ()
  "Build font-lock rules for markdown-inline inside doc attributes."
  (treesit-font-lock-rules
   :language 'markdown-inline
   :feature 'doc
   :override 'prepend
   '([(code_span) @font-lock-constant-face]
     [(emphasis) @italic]
     [(strong_emphasis) @bold]
     [(inline_link (link_text) @link)]
     [(inline_link (link_destination) @font-lock-constant-face)]
     [(shortcut_link (link_text) @link)]
     [(image_description) @link])))

(defun erlang-ts--doc-range-rules ()
  "Return range rules for embedding markdown-inline in doc attributes."
  (treesit-range-rules
   :embed 'markdown-inline
   :host 'erlang
   :local t
   '((wild_attribute
      name: (attr_name name: (atom) @_name
                       (:equal "doc" @_name))
      value: (string) @capture)
     (wild_attribute
      name: (attr_name name: (atom) @_name
                       (:equal "moduledoc" @_name))
      value: (string) @capture))))

(defun erlang-ts--language-at-point (pos)
  "Return the tree-sitter language at POS.
Returns `markdown-inline' when inside a doc attribute string,
`erlang' otherwise."
  (if (and (seq-some (lambda (p) (eq (treesit-parser-language p) 'markdown-inline))
                     (treesit-parser-list))
           (let ((node (treesit-node-at pos 'markdown-inline)))
             (and node (not (equal (treesit-node-type node) "document")))))
      'markdown-inline
    'erlang))

;;; Indentation

(defcustom erlang-ts-use-treesit-indent nil
  "When non-nil, use tree-sitter based indentation.
When nil, use the classic erlang-mode indentation engine.
The tree-sitter indentation is experimental and may not handle
all cases correctly, especially incomplete code during editing."
  :type 'boolean
  :group 'erlang)

(defun erlang-ts--grand-parent-bol (_node parent _bol &rest _)
  "Return the first non-whitespace position on PARENT's parent's line."
  (when-let* ((gp (treesit-node-parent parent)))
    (save-excursion
      (goto-char (treesit-node-start gp))
      (back-to-indentation)
      (point))))

(defun erlang-ts--anchor-matching-open (node parent _bol &rest _)
  "Return position of the opening delimiter matching NODE in PARENT.
Matches `)' to `(', `]' to `[', and `}' to `{'."
  (let ((close (treesit-node-type node)))
    (save-excursion
      (goto-char (treesit-node-start parent))
      (let ((open (pcase close
                    (")" "(")
                    ("]" "\\[")
                    ("}" "{"))))
        (when (and open (re-search-forward open (treesit-node-end parent) t))
          (1- (point)))))))

(defun erlang-ts--anchor-after-open-delim (_node parent _bol &rest _)
  "Return position right after the opening delimiter in PARENT.
Finds the first `(', `[', or `{' in PARENT and returns the
position after it.  Used to align elements with the first element."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (when (re-search-forward "[[({]" (treesit-node-end parent) t)
      (point))))

(defun erlang-ts--anchor-args (_node parent _bol &rest _)
  "Return anchor position for function arguments in PARENT.
When the opening `(' has content on the same line, align after it.
When `(' is at end of line (Okeefe style), indent from parent start."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (when (re-search-forward "(" (treesit-node-end parent) t)
      (if (looking-at "[ \t]*$")
          ;; Paren at end of line: use erlang-argument-indent from parent
          (let ((target-col (+ (save-excursion
                                 (goto-char (treesit-node-start
                                             (treesit-node-parent parent)))
                                 (current-column))
                               (if (boundp 'erlang-argument-indent)
                                   erlang-argument-indent
                                 erlang-indent-level))))
            (goto-char (treesit-node-start parent))
            (beginning-of-line)
            (move-to-column target-col t)
            (point))
        ;; Content follows: align after (
        (point)))))

(defun erlang-ts--anchor-after-open-brace (_node parent _bol &rest _)
  "Return position right after the opening `{' in PARENT.
Used for record and map fields where `{' is the relevant delimiter."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (when (search-forward "{" (treesit-node-end parent) t)
      (point))))

(defun erlang-ts--anchor-after-open-bracket (_node parent _bol &rest _)
  "Return position right after the opening `[' in PARENT.
Used for export/import attributes where `[' is the relevant delimiter."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (when (search-forward "[" (treesit-node-end parent) t)
      (point))))

(defun erlang-ts--match-triple-comment (node _parent _bol &rest _)
  "Match NODE when it is a %%% comment (three or more percent signs)."
  (and node
       (equal (treesit-node-type node) "comment")
       (string-prefix-p "%%%" (treesit-node-text node))))

(defun erlang-ts--match-single-comment (node _parent _bol &rest _)
  "Match NODE when it is a single % comment (not %% or %%%)."
  (and node
       (equal (treesit-node-type node) "comment")
       (let ((text (treesit-node-text node)))
         (and (string-prefix-p "%" text)
              (not (string-prefix-p "%%" text))))))

(defun erlang-ts--offset-comment-column (_node _parent _bol &rest _)
  "Return `comment-column' as an absolute offset for single-% comments."
  (or comment-column 48))

(defun erlang-ts--double-indent-offset (_node _parent _bol &rest _)
  "Return double `erlang-indent-level'."
  (* 2 erlang-indent-level))

(defun erlang-ts--match-clause-body-in (grandparent-type)
  "Return a matcher for clause_body children inside GRANDPARENT-TYPE."
  (lambda (_node parent _bol &rest _)
    (and (equal (treesit-node-type parent) "clause_body")
         (equal (treesit-node-type (treesit-node-parent parent))
                grandparent-type))))

(defun erlang-ts--match-inline-clause-body (clause-type block-type)
  "Return a matcher for clause_body children in inline-style clauses.
Matches when the parent is a clause_body inside CLAUSE-TYPE, and the
clause starts on the same line as the enclosing BLOCK-TYPE keyword."
  (lambda (_node parent _bol &rest _)
    (and (equal (treesit-node-type parent) "clause_body")
         (let ((clause (treesit-node-parent parent)))
           (and (equal (treesit-node-type clause) clause-type)
                (let ((block (treesit-node-parent clause)))
                  (and (equal (treesit-node-type block) block-type)
                       ;; Check if clause starts on same line as block keyword
                       (= (line-number-at-pos (treesit-node-start block))
                          (line-number-at-pos (treesit-node-start clause))))))))))

(defun erlang-ts--indent-rules ()
  "Return tree-sitter indentation rules for Erlang.
The return value is suitable for `treesit-simple-indent-rules'."
  `((erlang
     ;; Erlang comment conventions:
     ;; %%% always at column 0
     ;; %% context-dependent (follows code indentation)
     ;; % right-aligned to comment-column
     (erlang-ts--match-triple-comment column-0 0)
     (erlang-ts--match-single-comment column-0 erlang-ts--offset-comment-column)

     ;; Strings: never re-indent content inside strings
     ;; (changing indentation would change the string value)
     ((parent-is "^string$") no-indent 0)

     ;; Top-level: column 0
     ((parent-is "source_file") column-0 0)

     ;; `end' aligns with opening construct
     ((node-is "end") parent-bol 0)

     ;; Keywords that align with their opening construct
     ((match "^catch$" "try_expr") parent-bol 0)

     ;; receive_after aligns with receive
     ((node-is "receive_after") parent-bol 0)

     ;; Closing delimiters: align with their opening counterpart
     ((match "^[])}]$" nil) erlang-ts--anchor-matching-open 0)

     ;; clause_body inside fun_clause/receive_after needs 2x indent
     ;; because the clause starts on the same line as the keyword
     (,(erlang-ts--match-clause-body-in "fun_clause")
      erlang-ts--grand-parent-bol erlang-ts--double-indent-offset)
     (,(erlang-ts--match-clause-body-in "receive_after")
      erlang-ts--grand-parent-bol erlang-ts--double-indent-offset)
     ;; if_clause body needs 2x indent only when the clause is on
     ;; the same line as the `if' keyword (inline style)
     (,(erlang-ts--match-inline-clause-body "if_clause" "if_expr")
      erlang-ts--grand-parent-bol erlang-ts--double-indent-offset)

     ;; Expressions inside clause_body: indent from the clause line
     ((parent-is "clause_body") parent-bol erlang-indent-level)

     ;; Clauses inside block constructs
     ((parent-is "case_expr") parent-bol erlang-indent-level)
     ((parent-is "receive_expr") parent-bol erlang-indent-level)
     ((parent-is "if_expr") parent-bol erlang-indent-level)
     ((parent-is "try_expr") parent-bol erlang-indent-level)
     ((parent-is "catch_clause") parent-bol erlang-indent-level)
     ((parent-is "receive_after") parent-bol erlang-indent-level)
     ((parent-is "anonymous_fun") parent-bol erlang-indent-level)
     ((parent-is "block_expr") parent-bol erlang-indent-level)
     ((parent-is "maybe_expr") parent-bol erlang-indent-level)

     ;; Macro body: align after opening (
     ((parent-is "pp_define") erlang-ts--anchor-after-open-delim 0)

     ;; Record/map fields: align after { (not ( which comes earlier)
     ((node-is "record_field") erlang-ts--anchor-after-open-brace 0)
     ((parent-is "record_expr") erlang-ts--anchor-after-open-brace 0)
     ((parent-is "map_expr") erlang-ts--anchor-after-open-brace 0)

     ;; Function arguments: smart alignment (Okeefe-aware)
     ((parent-is "expr_args") erlang-ts--anchor-args 0)
     ;; Collections: align after opening delimiter
     ((parent-is "^list$") erlang-ts--anchor-after-open-delim 0)
     ((parent-is "^tuple$") erlang-ts--anchor-after-open-delim 0)
     ((parent-is "^binary$") erlang-ts--anchor-after-open-delim 0)

     ;; Export/import attributes: align after [
     ((parent-is "export_attribute") erlang-ts--anchor-after-open-bracket 0)
     ((parent-is "import_attribute") erlang-ts--anchor-after-open-bracket 0)
     ((parent-is "export_type_attribute") erlang-ts--anchor-after-open-bracket 0)

     ;; Comprehensions
     ((parent-is "list_comprehension") parent-bol erlang-indent-level)
     ((parent-is "binary_comprehension") parent-bol erlang-indent-level)
     ((parent-is "lc_exprs") parent-bol 0)
     ((parent-is "lc_or_zc_expr") parent-bol 0)
     ((parent-is "generator") parent-bol erlang-indent-level)
     ((parent-is "b_generator") parent-bol erlang-indent-level)

     ;; Multi-line expressions
     ((parent-is "binary_op_expr") parent-bol erlang-indent-level)
     ((parent-is "match_expr") parent-bol erlang-indent-level)
     ((parent-is "unary_op_expr") parent-bol erlang-indent-level)
     ((parent-is "pipe") parent-bol 0)
     ((parent-is "paren_expr") parent-bol erlang-indent-level)

     ;; Guard
     ((parent-is "guard") parent-bol erlang-indent-guard)

     ;; Type specs: body indented 2x, | alternatives aligned at indent+2
     ((parent-is "spec") parent-bol erlang-indent-level)
     ((parent-is "type_alias") parent-bol erlang-ts--double-indent-offset)
     ((parent-is "opaque") parent-bol erlang-ts--double-indent-offset)
     ((parent-is "type_sig") parent-bol erlang-indent-level)

     ;; Error recovery
     ((parent-is "ERROR") parent-bol erlang-indent-level)

     ;; Catch-all: preserve previous line indentation
     (no-node prev-line 0))))

(defun erlang-ts-toggle-indent-function ()
  "Toggle between tree-sitter and erlang-mode indentation."
  (interactive)
  (if (eq indent-line-function 'treesit-indent)
      (progn
        (setq-local indent-line-function #'erlang-indent-command)
        (setq-local indent-region-function #'erlang-indent-region)
        (message "Switched to erlang-mode indentation"))
    (setq-local indent-line-function #'treesit-indent)
    (setq-local indent-region-function #'treesit-indent-region)
    (message "Switched to tree-sitter indentation")))

;;  imenu support

(defun erlang-ts-imenu-node-func-p (node)
  "Check if NODE is the first function clause."
  (when-let ((prev (treesit-node-prev-sibling node)))
    (not (and (equal (treesit-node-type prev) "fun_decl")
              (equal (erlang-ts-imenu-func-name prev)
                     (erlang-ts-imenu-func-name node))))))

(defun erlang-ts-imenu-func-name (node)
  "Fetch function name at NODE."
  (let* ((clause (treesit-node-child-by-field-name node "clause"))
         (name (treesit-node-text (treesit-node-child-by-field-name clause "name")))
         (args (treesit-node-child-by-field-name clause "args")))
    (format "%s/%s" name (treesit-node-child-count args "args"))))

(defun erlang-ts-imenu-pp-name (node)
  "Fetch macro name at NODE."
  (let* ((lhs (treesit-node-child-by-field-name node "lhs"))
         (name (treesit-node-text (treesit-node-child-by-field-name lhs "name")))
         (args (treesit-node-child-by-field-name lhs "args")))
    (if args (format "%s/%d" name (treesit-node-child-count args "args"))
      name)))

(defun erlang-ts-imenu-record-name (node)
  "Fetch record name at NODE."
  (let ((name (treesit-node-text (treesit-node-child-by-field-name node "name"))))
    (format "#%s{}" name )))

(defun erlang-ts-imenu-type-name (node)
  "Fetch type name at NODE."
  (treesit-node-text
   (treesit-node-child-by-field-name
    (treesit-node-child-by-field-name node "name") "name")))

(defun erlang-ts-imenu-setup ()
  "Setup imenu functions.
This conflicts with lsp's imenu functionality if lsp is used
Use (setq lsp-enable-imenu nil) to disable lsp-imenu"
  (setq-local
   treesit-simple-imenu-settings
   '(("macros" "\\`pp_define\\'" nil erlang-ts-imenu-pp-name)
     ("records" "\\`record_decl\\'" nil erlang-ts-imenu-record-name)
     ("types" "\\`type_alias\\'" nil erlang-ts-imenu-type-name)
     ("functions" "\\`fun_decl\\'" erlang-ts-imenu-node-func-p erlang-ts-imenu-func-name))))

(defun erlang-ts-setup ()
  "Setup treesit for erlang."

  ;; Compute font-lock rules lazily if not done at load time
  ;; (e.g. grammar wasn't available during byte-compilation)
  (unless erlang-ts-font-lock-rules
    (setq erlang-ts-font-lock-rules (erlang-ts--build-font-lock-rules)))

  (let ((use-markdown (and erlang-ts-use-markdown-inline
                           (>= emacs-major-version 30)
                           (treesit-ready-p 'markdown-inline t))))
    ;; Embedded markdown in doc attributes
    (when use-markdown
      (setq-local treesit-range-settings (erlang-ts--doc-range-rules))
      (setq-local treesit-language-at-point-function
                  #'erlang-ts--language-at-point)
      (unless erlang-ts--markdown-font-lock-rules
        (setq erlang-ts--markdown-font-lock-rules
              (erlang-ts--build-markdown-font-lock-rules))))

    (setq-local treesit-font-lock-settings
                (append erlang-ts-font-lock-rules
                        (when use-markdown
                          erlang-ts--markdown-font-lock-rules))))

  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-feature-list
              '(
                (string             ;; Level 1
                 comment
                 keyword
                 doc)
                (preprocessor       ;; Level 2
                 operator-atoms
                 definition
                 type)
                (builtin            ;; Level 3
                 variable
                 guards
                 function-call
                 constant)
                (operator           ;; Level 4
                 remote-module
                 delimiter
                 bracket
                 number
                 index-atom)))

  ;; Should we set this or let the user decide?
  ;;  (setq-local treesit-font-lock-level 3)

  ;;  If default color for: font-lock-property-name-face and/or
  ;;  font-lock-property-use-face change it since they by default
  ;;  inherit from font-lock-variable-name-face which looks strange in
  ;;  erlang

  (if (eq (face-user-default-spec 'font-lock-property-name-face)
          (face-default-spec 'font-lock-property-name-face))
      (face-remap-add-relative 'font-lock-property-name-face
                               :inherit font-lock-constant-face))

  (setq-local treesit-simple-indent-rules (erlang-ts--indent-rules))
  (when erlang-ts-use-treesit-indent
    (setq-local indent-line-function #'treesit-indent)
    (setq-local indent-region-function #'treesit-indent-region))

  (if (boundp 'lsp-language-id-configuration)
      (add-to-list 'lsp-language-id-configuration
                   '(erlang-ts-mode . "erlang")))

  (dolist (r '("\\.erl$" "\\.app\\.src$" "\\.escript"
               "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app"))
    (add-to-list 'auto-mode-alist (cons r 'erlang-ts-mode)))

  ;; Override some erlang-mode functions
  (advice-add #'erlang-font-lock-level-1 :around #'erlang-ts--font-lock-level-1)
  (advice-add #'erlang-font-lock-level-2 :around #'erlang-ts--font-lock-level-2)
  (advice-add #'erlang-font-lock-level-3 :around #'erlang-ts--font-lock-level-3)
  (advice-add #'erlang-font-lock-level-4 :around #'erlang-ts--font-lock-level-4)

  (erlang-ts-imenu-setup)

  (treesit-major-mode-setup)
  (setq-local syntax-propertize-function #'erlang-ts--syntax-propertize))


(defun erlang-ts-unload-function ()
  "Used by `unload-feature'."

  (while (rassoc 'erlang-ts-mode auto-mode-alist)
    (setq auto-mode-alist
          (assq-delete-all (car (rassoc 'erlang-ts-mode auto-mode-alist))
                           auto-mode-alist)))

  (advice-remove #'erlang-font-lock-level-1 #'erlang-ts--font-lock-level-1)
  (advice-remove #'erlang-font-lock-level-2 #'erlang-ts--font-lock-level-2)
  (advice-remove #'erlang-font-lock-level-3 #'erlang-ts--font-lock-level-3)
  (advice-remove #'erlang-font-lock-level-4 #'erlang-ts--font-lock-level-4))


;;; Keymap and menu

(defvar erlang-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define erlang-ts-mode-menu map "Erlang-TS Mode Menu"
      '("Erlang-TS"
        ("Navigate"
         ["Beginning of Function" beginning-of-defun]
         ["End of Function" end-of-defun])
        "--"
        ["Mark Function" mark-defun]
        "--"
        ("Font-lock level"
         ["Level 1 (minimal)" erlang-font-lock-level-1]
         ["Level 2 (keywords)" erlang-font-lock-level-2]
         ["Level 3 (default)" erlang-font-lock-level-3]
         ["Level 4 (all)" erlang-font-lock-level-4])
        "--"
        ["Toggle tree-sitter indentation" erlang-ts-toggle-indent-function]
        ["Install tree-sitter grammar" erlang-ts-install-grammar]
        ["Install markdown grammar" erlang-ts-install-markdown-grammar]))
    map)
  "Keymap for `erlang-ts-mode'.")

;;;###autoload
(define-derived-mode erlang-ts-mode erlang-mode "erl-ts"
  "Major mode for editing erlang with tree-sitter."
  :syntax-table nil
  (erlang-ts-syntax-table-init)
  (unless (treesit-language-available-p 'erlang)
    (when (y-or-n-p "Erlang tree-sitter grammar is not installed.  Install it now?")
      (erlang-ts-install-grammar)))
  (when (treesit-ready-p 'erlang)
    (treesit-parser-create 'erlang)
    (erlang-ts-setup)))

(provide 'erlang-ts)
;;; erlang-ts.el ends here
