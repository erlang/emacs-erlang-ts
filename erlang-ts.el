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
;; Package-Version: 0.2

;;; Commentary:

;; # Emacs Erlang mode using treesitter #
;;
;; Requires emacs-29 compiled with treesitter support.
;;
;; Currently uses treesitter only for syntax-highlighting,
;; *font-lock-mode*, and uses the "old" erlang-mode for everything else.
;;
;; # Install #
;;
;; Add to your .emacs file:
;;
;; ```
;;  (add-to-list 'treesit-language-source-alist
;;       '(erlang "https://github.com/WhatsApp/tree-sitter-erlang"))
;;
;;  (use-package erlang-ts
;;      :mode ("\\.erl\\'" . erlang-ts-mode)
;;      :defer 't)
;; ```
;; Install/compile erlang treesitter support (first time only):
;;
;; ```
;;   M-x treesit-install-language-grammar
;;   Language: erlang
;; ```
;;
;;; Code:

;; Introduction:
;; ------------
;; Currently handles font-locking for erlang-mode
;;
;; Devs See:
;;    M-x treesit-explore-mode
;; and also
;;   (info "(elisp) Parsing Program Source")

(require 'treesit)
(require 'erlang)

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

(defvar erlang-ts-font-lock-rules
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
     (internal_fun fun: (atom) @font-lock-function-name-face))

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
   `((module_attribute name: (atom) @font-lock-constant-face)
     (behaviour_attribute name: (_) @font-lock-constant-face)

     (pp_define lhs: (macro_lhs name: (_) @font-lock-constant-face))
     (pp_undef name: (_) @font-lock-constant-face)
     (pp_ifdef name: (_) @font-lock-constant-face)
     (pp_ifndef name: (_) @font-lock-constant-face)

     (macro_call_expr name: (var) @font-lock-constant-face
                      (:pred erlang-ts-predefined-macro-p @font-lock-constant-face))

     ((atom) @font-lock-constant-face (:match ,erlang-atom-quoted-regexp @font-lock-constant-face))
     ((char) @font-lock-constant-face
      (:match "\\(\\$\\([^\\]\\|\\\\\\([^0-7^\n]\\|[0-7]+\\|\\^[a-zA-Z]\\)\\)\\)"
              @font-lock-constant-face)))

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
   :feature 'function-call
   `(
     (call expr: (atom) @font-lock-function-call-face)
     (call expr: (remote module: (remote_module module: (atom) @font-lock-constant-face)
                         fun: (atom) @font-lock-function-call-face))
     (call expr: (remote fun: (atom) @font-lock-function-call-face))
     (remote module: (remote_module module: (atom) @font-lock-constant-face)))

   :language 'erlang
   :feature 'bracket
   '((["(" ")" "[" "]" "{"  "{" "}" "<<" ">>"]) @font-lock-bracket-face)

   :language 'erlang
   :feature 'delimiter
   '((["." "," ";" "|"]) @font-lock-delimiter-face)

   :language 'erlang
   :feature 'operator
   ;; Add "<:-" "<:=" "&&"  when available in tree-sitter
   '(([ "->" "||" "<-" "<=" "+" "-" "*" "/" "++"
        ">" ">=" "<" "=<" "=" "==" "=:=" "=/="])
     @font-lock-operator-face))

  "Tree-sitter font-lock settings for `erlang-ts-mode'.
Use `treesit-font-lock-level' or `treesit-font-lock-feature-list'
 to change settings")

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

(defvar erlang-ts--syntax-propertize-query
  (when (treesit-available-p)
    (treesit-query-compile
     'erlang
     '(((atom) @node-atom)
       ((string) @node-string-triple-quoted (:match "^\"\"\"" @node-string-triple-quoted))
       ((string) @node-string)))))

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
            (let ((custom-table (copy-syntax-table (syntax-table))))
              (modify-syntax-entry ?$ "w" custom-table)
              (put-text-property content-start content-end 'syntax-table custom-table))))))))

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
        (let ((custom-table (copy-syntax-table (syntax-table))))
          (modify-syntax-entry ?$ "w" custom-table)
          (put-text-property content-start content-end 'syntax-table custom-table))))))

(defun erlang-ts--syntax-propertize (start end)
  "Apply syntax properties for Erlang specific patterns from START to END."
  (let ((captures
         (treesit-query-capture 'erlang erlang-ts--syntax-propertize-query start end)))
    (pcase-dolist (`(,name . ,node) captures)
      (pcase name
        ('node-atom   (erlang-ts--process-node node))
        ('node-string (erlang-ts--process-node node))
        ('node-string-triple-quoted (erlang-ts--process-node-triple-quoted node))))))

(defun erlang-ts-setup ()
  "Setup treesit for erlang."

  (setq-local treesit-font-lock-settings erlang-ts-font-lock-rules)

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

  ;; (setq-local treesit-simple-indent-rules erlang-ts-indent-rules)

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


;;;###autoload
(define-derived-mode erlang-ts-mode erlang-mode "erl-ts"
  "Major mode for editing erlang with tree-sitter."
  :syntax-table erlang-mode-syntax-table
  (when (treesit-ready-p 'erlang)
    (treesit-parser-create 'erlang)
    (erlang-ts-setup)))

(provide 'erlang-ts)
;;; erlang-ts.el ends here
