;;; erlang-ts-font-lock-test.el --- Font-lock tests for erlang-ts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Buttercup tests for erlang-ts-mode font-lock rules.
;; Tests are organized by font-lock feature, matching the 4 levels
;; defined in `treesit-font-lock-feature-list'.

;;; Code:

(require 'buttercup)
(require 'erlang-ts)

;;;; Buttercup matchers

(buttercup-define-matcher :to-include-face (face expected)
  "Check that FACE (a symbol or list) includes EXPECTED."
  (let* ((f (funcall face))
         (e (funcall expected))
         (faces (if (listp f) f (list f))))
    (if (memq e faces)
        t
      `(nil . ,(format "Expected face %S to include %S" f e)))))

;;;; Test helpers

(defmacro with-fontified-erlang-ts-buffer (content &rest body)
  "Set up a temporary buffer with CONTENT, apply erlang-ts-mode font-lock, run BODY.
All four font-lock levels are activated so every feature is testable."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (let ((treesit-font-lock-level 4))
       (erlang-ts-mode))
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun erlang-ts-test-face-at (start end)
  "Return the face at range [START, END] in the current buffer.
If all positions in the range share the same face, return it.
Otherwise return the symbol `various-faces'."
  (let ((face (get-text-property start 'face)))
    (if (= start end)
        face
      (let ((pos (1+ start))
            (consistent t))
        (while (and consistent (<= pos end))
          (unless (equal (get-text-property pos 'face) face)
            (setq consistent nil))
          (setq pos (1+ pos)))
        (if consistent face 'various-faces)))))

(defun erlang-ts-test-expect-faces-at (content &rest face-specs)
  "Fontify CONTENT with `erlang-ts-mode' and assert FACE-SPECS.
Each element of FACE-SPECS is a list (START END EXPECTED-FACE)
where START and END are buffer positions (1-indexed, inclusive)."
  (with-fontified-erlang-ts-buffer content
    (dolist (spec face-specs)
      (let* ((start (nth 0 spec))
             (end (nth 1 spec))
             (expected (nth 2 spec))
             (actual (erlang-ts-test-face-at start end)))
        (expect actual :to-equal expected)))))

(defmacro when-fontifying-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in Erlang code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE (START END FACE) ...)
where CODE is an Erlang source string and each (START END FACE)
triple asserts that positions START through END have FACE."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (apply #'erlang-ts-test-expect-faces-at test))))

;;;; Tests

(describe "erlang-ts font-lock"
  (before-all
    (unless (treesit-language-available-p 'erlang)
      (signal 'buttercup-pending "tree-sitter Erlang grammar not available")))

  ;; ---- Level 1 features ------------------------------------------------

  (describe "comment feature"
    (when-fontifying-it "fontifies line comments"
      ;; % a comment
      ;; 12345678901
      ("% a comment"
       (1 11 font-lock-comment-face)))

    (when-fontifying-it "fontifies double-percent comments"
      ;; %% a comment
      ;; 123456789012
      ("%% a comment"
       (1 12 font-lock-comment-face))))

  (describe "string feature"
    (when-fontifying-it "fontifies string literals"
      ;; "hello"
      ;; 1234567
      ("\"hello\""
       (1 7 font-lock-string-face))))

  (describe "keyword feature"
    (when-fontifying-it "fontifies case/of/end"
      ;; case X of\n  _ -> ok\nend
      ;; 1234
      ("case X of\n  _ -> ok\nend"
       (1 4 font-lock-keyword-face)))

    (when-fontifying-it "fontifies if/end"
      ;; if\n  true -> ok\nend
      ;; 12
      ("if\n  true -> ok\nend"
       (1 2 font-lock-keyword-face)))

    (when-fontifying-it "fontifies try/catch/end"
      ;; try\n  ok\ncatch\n  _:_ -> error\nend
      ;; 123
      ("try\n  ok\ncatch\n  _:_ -> error\nend"
       (1 3 font-lock-keyword-face)))

    (when-fontifying-it "fontifies receive"
      ;; receive\n  _ -> ok\nend
      ;; 1234567
      ("receive\n  _ -> ok\nend"
       (1 7 font-lock-keyword-face)))

    (when-fontifying-it "fontifies fun keyword"
      ;; fun(X) -> X end
      ;; 123
      ("fun(X) -> X end"
       (1 3 font-lock-keyword-face)))

    (when-fontifying-it "fontifies begin/end"
      ;; begin ok end
      ;; 12345
      ("begin ok end"
       (1 5 font-lock-keyword-face)))

    (when-fontifying-it "fontifies maybe/else"
      ;; maybe\n  ok\nelse\n  error\nend
      ;; 12345
      ("maybe\n  ok\nelse\n  error\nend"
       (1 5 font-lock-keyword-face)))

    (when-fontifying-it "fontifies when"
      ;; f(X) when X > 0 -> X.
      ;; 123456789012345
      ;; "when" starts at 6
      ("f(X) when X > 0 -> X."
       (6 9 font-lock-keyword-face))))

  (describe "doc feature"
    (when-fontifying-it "fontifies -doc string"
      ;; -doc "A doc string".
      ;; 12345678901234567890
      ("-doc \"A doc string\"."
       (6 19 font-lock-doc-face)))

    (describe "embedded markdown"
      (it "fontifies bold in -doc"
        (assume (and (>= emacs-major-version 30)
                     (treesit-language-available-p 'markdown-inline)))
        (with-fontified-erlang-ts-buffer "-doc \"This **bold** text\"."
          (expect (get-text-property 14 'face) :to-include-face 'bold)))

      (it "fontifies code spans in -doc"
        (assume (and (>= emacs-major-version 30)
                     (treesit-language-available-p 'markdown-inline)))
        (with-fontified-erlang-ts-buffer "-doc \"Use `code` here\"."
          (expect (get-text-property 12 'face)
                  :to-include-face 'font-lock-constant-face)))

      (it "fontifies bold in -moduledoc"
        (assume (and (>= emacs-major-version 30)
                     (treesit-language-available-p 'markdown-inline)))
        (with-fontified-erlang-ts-buffer "-moduledoc \"This **bold** text\"."
          (expect (get-text-property 20 'face) :to-include-face 'bold)))

      (it "preserves doc face on plain text"
        (with-fontified-erlang-ts-buffer "-doc \"plain text\"."
          (expect (get-text-property 7 'face) :to-equal 'font-lock-doc-face)))))

  ;; ---- Level 2 features ------------------------------------------------

  (describe "preprocessor feature"
    (when-fontifying-it "fontifies -module attribute"
      ;; -module(foo).
      ;; 12345678
      ("-module(foo)."
       (1 7 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies -export attribute"
      ;; -export([foo/1]).
      ;; 1234567
      ("-export([foo/1])."
       (1 7 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies -define"
      ;; -define(MAX, 100).
      ;; 1234567
      ("-define(MAX, 100)."
       (1 7 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies -record"
      ;; -record(foo, {bar}).
      ;; 1234567
      ("-record(foo, {bar})."
       (1 7 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies -spec"
      ;; -spec foo(integer()) -> ok.
      ;; 12345
      ("-spec foo(integer()) -> ok."
       (1 5 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies -type"
      ;; -type color() :: red | green.
      ;; 12345
      ("-type color() :: red | green."
       (1 5 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies -callback"
      ;; -callback init(Args) -> ok.
      ;; 123456789
      ("-callback init(Args) -> ok."
       (1 9 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies -ifdef"
      ;; -ifdef(TEST).
      ;; 123456
      ("-ifdef(TEST)."
       (1 6 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies macro calls"
      ;; ?MAX_SIZE
      ;;  23456789
      ("?MAX_SIZE"
       (2 9 font-lock-preprocessor-face))))

  (describe "definition feature"
    (when-fontifying-it "fontifies function names"
      ;; hello(Name) ->
      ;; 12345
      ("hello(Name) ->\n    ok."
       (1 5 font-lock-function-name-face)))

    (when-fontifying-it "fontifies function names in -spec"
      ;; -spec hello(string()) -> ok.
      ;; 1234567890
      ("-spec hello(string()) -> ok."
       (7 11 font-lock-function-name-face)))

    (when-fontifying-it "fontifies function names in -callback"
      ;; -callback init(term()) -> ok.
      ;; 12345678901234
      ("-callback init(term()) -> ok."
       (11 14 font-lock-function-name-face))))

  (describe "type feature"
    (when-fontifying-it "fontifies type names in -type"
      ;; -type color() :: red.
      ;; 123456789012
      ("-type color() :: red."
       (7 11 font-lock-type-face)))

    (when-fontifying-it "fontifies record names"
      ;; -record(person, {name}).
      ;; 12345678901234
      ("-record(person, {name})."
       (9 14 font-lock-type-face)))

    (when-fontifying-it "fontifies record field names in declarations"
      ;; -record(person, {name}).
      ;; 123456789012345678901
      ("-record(person, {name})."
       (18 21 font-lock-property-name-face))))

  ;; ---- Level 3 features ------------------------------------------------

  (describe "variable feature"
    (when-fontifying-it "fontifies variables"
      ;; foo(Name) ->
      ;; 12345678
      ("foo(Name) ->\n    Name."
       (5 8 font-lock-variable-name-face))))

  (describe "constant feature"
    (when-fontifying-it "fontifies quoted atoms"
      ;; 'hello world'
      ;; 1234567890123
      ("'hello world'"
       (1 13 font-lock-constant-face)))

    (when-fontifying-it "fontifies char literals"
      ;; $A
      ;; 12
      ("$A"
       (1 2 font-lock-constant-face))))

  (describe "function-call feature"
    (when-fontifying-it "fontifies function calls"
      ;; foo(X) ->\n    bar(X).
      ;; position 15-17 = bar
      ("foo(X) ->\n    bar(X)."
       (15 17 font-lock-function-call-face))))

  ;; ---- Level 4 features ------------------------------------------------

  (describe "operator feature"
    (when-fontifying-it "fontifies -> operator"
      ;; foo() -> ok.
      ;; 1234567
      ("foo() -> ok."
       (7 8 font-lock-operator-face)))

    (when-fontifying-it "fontifies arithmetic operators"
      ;; X + Y
      ;; 12345
      ("X + Y"
       (3 3 font-lock-operator-face)))

    (when-fontifying-it "fontifies comparison operators"
      ;; X >= Y
      ;; 123456
      ("X >= Y"
       (3 4 font-lock-operator-face)))

    (when-fontifying-it "fontifies list operators"
      ;; X ++ Y
      ;; 123456
      ("X ++ Y"
       (3 4 font-lock-operator-face))))

  (describe "bracket feature"
    (when-fontifying-it "fontifies parentheses"
      ;; f(X)
      ;; 1234
      ("f(X)"
       (2 2 font-lock-bracket-face)
       (4 4 font-lock-bracket-face)))

    (when-fontifying-it "fontifies square brackets"
      ;; [1, 2]
      ;; 123456
      ("[1, 2]"
       (1 1 font-lock-bracket-face)
       (6 6 font-lock-bracket-face)))

    (when-fontifying-it "fontifies curly braces"
      ;; {ok, 1}
      ;; 1234567
      ("{ok, 1}"
       (1 1 font-lock-bracket-face)
       (7 7 font-lock-bracket-face)))

    (when-fontifying-it "fontifies binary delimiters"
      ;; <<1, 2>>
      ;; 12345678
      ("<<1, 2>>"
       (1 2 font-lock-bracket-face)
       (7 8 font-lock-bracket-face))))

  (describe "delimiter feature"
    (when-fontifying-it "fontifies dots"
      ;; foo() -> ok.
      ;; 123456789012
      ("foo() -> ok."
       (12 12 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies commas"
      ;; {1, 2}
      ;; 123456
      ("{1, 2}"
       (3 3 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies semicolons"
      ;; case X of\n  a -> 1;\n  b -> 2\nend
      ;; the ; is at position 19
      ("case X of\n  a -> 1;\n  b -> 2\nend"
       (19 19 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies pipes in lists"
      ;; [H | T]
      ;; 1234567
      ("[H | T]"
       (4 4 font-lock-delimiter-face))))

  (describe "number feature"
    (when-fontifying-it "fontifies integers"
      ;; 42
      ;; 12
      ("42"
       (1 2 font-lock-number-face)))

    (when-fontifying-it "fontifies floats"
      ;; 3.14
      ;; 1234
      ("3.14"
       (1 4 font-lock-number-face)))

    (when-fontifying-it "fontifies base-N integers"
      ;; 16#FF
      ;; 12345
      ("16#FF"
       (1 5 font-lock-number-face))))

  (describe "remote-module feature"
    (when-fontifying-it "fontifies module in remote call"
      ;; io:format("~p~n", [X])
      ;; 12
      ("io:format(\"~p~n\", [X])"
       (1 2 font-lock-constant-face)))

    (when-fontifying-it "fontifies function in remote call"
      ;; io:format("~p~n", [X])
      ;; 1234567890
      ("io:format(\"~p~n\", [X])"
       (4 9 font-lock-function-name-face)))))

;;; erlang-ts-font-lock-test.el ends here
