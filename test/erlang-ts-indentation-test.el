;;; erlang-ts-indentation-test.el --- Indentation tests for erlang-ts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Buttercup tests for erlang-ts-mode indentation.
;; Each test verifies that indentation works correctly with both the
;; classic erlang-mode engine and the tree-sitter indentation engine.

;;; Code:

(require 'buttercup)
(require 'erlang-ts)

(defun erlang-ts-test--strip-indentation (code)
  "Remove all leading whitespace from each line of CODE."
  (mapconcat
   (lambda (line) (string-trim-left line))
   (split-string code "\n")
   "\n"))

(defun erlang-ts-test--indent-with (code indent-fn region-fn)
  "Indent CODE using INDENT-FN and REGION-FN, return the result."
  (with-temp-buffer
    (insert (erlang-ts-test--strip-indentation code))
    (erlang-ts-mode)
    (setq-local indent-line-function indent-fn)
    (when region-fn
      (setq-local indent-region-function region-fn))
    (setq-local indent-tabs-mode nil)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defmacro when-indenting-it (description &rest code-strings)
  "Create Buttercup tests asserting CODE-STRINGS indent correctly.
DESCRIPTION is the test name.  Each element of CODE-STRINGS is a
properly-indented Erlang code string.  Two tests are generated:
one for erlang-mode indentation, one for tree-sitter indentation."
  (declare (indent 1))
  `(progn
     (it ,(concat description " (erlang-mode)")
       ,@(mapcar
          (lambda (code)
            `(let ((expected ,code))
               (expect (erlang-ts-test--indent-with
                        expected #'erlang-indent-command #'erlang-indent-region)
                       :to-equal expected)))
          code-strings))
     (it ,(concat description " (tree-sitter)")
       ,@(mapcar
          (lambda (code)
            `(let ((expected ,code))
               (expect (erlang-ts-test--indent-with
                        expected #'treesit-indent #'treesit-indent-region)
                       :to-equal expected)))
          code-strings))))

(describe "erlang-ts indentation"
  (before-all
    (unless (treesit-language-available-p 'erlang)
      (signal 'buttercup-pending "tree-sitter Erlang grammar not available")))

  (when-indenting-it "indents a simple function"
    "hello(Name) ->
    io:format(\"Hello, ~s!~n\", [Name]).")

  (when-indenting-it "indents a function with multiple clauses"
    "factorial(0) -> 1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).")

  (when-indenting-it "indents a case expression"
    "process(Type) ->
    case Type of
        parse ->
            ok;
        validate ->
            error
    end.")

  (when-indenting-it "indents an if expression"
    "check(X) ->
    if
        X > 0 ->
            positive;
        X < 0 ->
            negative;
        true ->
            zero
    end.")

  (when-indenting-it "indents try/catch"
    "safe_call(F) ->
    try
        F()
    catch
        error:Reason ->
            {error, Reason};
        exit:Reason ->
            {error, Reason};
        Throw ->
            {throw, Throw}
    end.")

  (when-indenting-it "indents try-of/catch"
    "safe_call(F) ->
    try F()
    of
        ok ->
            ok;
        Error ->
            {error, Reason}
    catch
        error:Reason ->
            {error, Reason};
        exit:Reason ->
            {error, Reason}
    end.")

  (when-indenting-it "indents a receive expression"
    "loop() ->
    receive
        {msg, Msg} ->
            handle(Msg),
            loop();
        stop ->
            ok
    end.")

  (when-indenting-it "indents a receive with after"
    "wait() ->
    receive
        Msg ->
            Msg
    after 5000 ->
            timeout
    end.")

  (when-indenting-it "indents a fun expression"
    "make_adder(N) ->
    fun(X) ->
            N + X
    end.")

  (when-indenting-it "indents a list"
    "make_list() ->
    A = [ foo,
          bar
        ],
    [
     a,b,c,d,
     e,f
    | A].")

 (when-indenting-it "indents a tuple"
   "make_tuple() ->
    A = { foo,
          bar
        },
    {a,b,c,d,
     e,f,A}.")

  (when-indenting-it "indents a map"
   "make_map() ->
    #{ foo => 1,
       bar =>
           2,
       {complex,
        key} => 3
     }.")

  (when-indenting-it "indents a binary"
   "make_bin(Str) ->
    Bin1 = << 1:1, 0:1, 1:1, 1:1,
              0:4,
              (byte_size(Str)):32/little
           >>,
    <<Bin1:binary,
      Str:binary>>.")

  (when-indenting-it "indents a multi-line binary operation"
    "add(X, Y) ->
    X +
        Y.")

  (when-indenting-it "indents a record definition"
    "-record(person, {
                 name :: string(),
                 age :: non_neg_integer()
                }).")

  (when-indenting-it "indents record constructs"
    "make_record(A,B,C) ->
    R1 = #record1{
            field_1 =
                A,
            field_2 = B},
    #record2{f1 = data,
             f2 = R1}.")

  (when-indenting-it "indents a type definition"
    "-type color() :: red | green | blue.")

  (when-indenting-it "indents begin/end"
    "run() ->
    begin
        step1(),
        step2()
    end.")

  (when-indenting-it "indents maybe"
    "run() ->
    maybe
        foo ?= step1(),
        bar ?= step2()
    else
        Err ->
            {error, Err}
    end.")

  (when-indenting-it "indents nested case expressions"
    "nested(X, Y) ->
    case X of
        a ->
            case Y
            of
                b ->
                    ok;
                _ ->
                    error
            end;
        _ ->
            skip
    end.")

  ;; Assignment indentation: block constructs on the RHS of = should
  ;; indent relative to where the construct starts, not the line start.

  (when-indenting-it "indents case in assignment"
    "f(T) ->
    Var = case T of
              parse -> ok;
              validate -> error
          end.")

  (when-indenting-it "indents if in assignment"
    "f() ->
    Var = if
              true -> ok;
              false -> err
          end.")

  (when-indenting-it "indents try/catch in assignment"
    "f() ->
    Var = try
              ok
          catch
              _:_ -> err
          end.")

  (when-indenting-it "indents receive in assignment"
    "f() ->
    Var = receive
              Msg -> Msg
          after 5000 ->
                  timeout
          end.")

  (when-indenting-it "indents begin/end in assignment"
    "f() ->
    Var = begin
              step1(),
              step2()
          end.")

  (when-indenting-it "indents fun in assignment"
    "f(N) ->
    F = fun(X) ->
                N + X
        end.")

  (when-indenting-it "indents block construct on next line after ="
    "f(T) ->
    Var =
        case T of
            parse -> ok
        end.")

  (when-indenting-it "indents when constructs"
   "function(X, Y)
  when X >= Y ->
    F = fun (A)
              when A > X ->
                X;
            (A) ->
                A
        end.")


  (it "does not re-indent content inside strings (tree-sitter)"
    (let ((code "foo() ->\n    \"\nsome text\n  inside string\n\"."))
      (expect
       (with-temp-buffer
         (insert code)
         (erlang-ts-mode)
         (setq-local indent-line-function #'treesit-indent)
         (setq-local indent-region-function #'treesit-indent-region)
         (setq-local indent-tabs-mode nil)
         (indent-region (point-min) (point-max))
         (buffer-string))
       :to-equal code)))

  (it "does not re-indent content inside doc strings (tree-sitter)"
    (let ((code "-doc \"\"\"\n```erlang\nbar() ->\n    ok\n```\n\"\"\"."))
      (expect
       (with-temp-buffer
         (insert code)
         (erlang-ts-mode)
         (setq-local indent-line-function #'treesit-indent)
         (setq-local indent-region-function #'treesit-indent-region)
         (setq-local indent-tabs-mode nil)
         (indent-region (point-min) (point-max))
         (buffer-string))
       :to-equal code))))

;;;; File-based indentation tests (from OTP emacs_SUITE_data)

(defvar erlang-ts-test--resource-dir
  (expand-file-name "resources/emacs_SUITE_data/"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing OTP indentation test files.")

(defun erlang-ts-test--indent-file (file indent-fn region-fn)
  "Read FILE, strip indentation, re-indent, and return the result.
Uses INDENT-FN and REGION-FN for indentation."
  (let ((expected (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
    (with-temp-buffer
      (insert (erlang-ts-test--strip-indentation expected))
      (erlang-ts-mode)
      (setq-local indent-line-function indent-fn)
      (when region-fn
        (setq-local indent-region-function region-fn))
      (setq-local indent-tabs-mode nil)
      (indent-region (point-min) (point-max))
      (cons (buffer-string) expected))))

(defmacro when-indenting-file-it (description file)
  "Create tests that assert FILE indents correctly with erlang-mode.
DESCRIPTION is the test name prefix.  FILE is relative to the
OTP test data directory."
  (declare (indent 1))
  (let ((path `(expand-file-name ,file erlang-ts-test--resource-dir)))
    `(it ,(concat description " (erlang-mode)")
       (let ((result (erlang-ts-test--indent-file
                      ,path
                      #'erlang-indent-command #'erlang-indent-region)))
         (expect (car result) :to-equal (cdr result))))))

(defmacro when-indenting-file-it-treesit (description file)
  "Create a tree-sitter indentation test for FILE.
DESCRIPTION is the test name prefix.  FILE is relative to the
OTP test data directory."
  (declare (indent 1))
  (let ((path `(expand-file-name ,file erlang-ts-test--resource-dir)))
    `(it ,(concat description " (tree-sitter)")
       (let ((result (erlang-ts-test--indent-file
                      ,path
                      #'treesit-indent #'treesit-indent-region)))
         (expect (car result) :to-equal (cdr result))))))

(describe "erlang-ts file indentation (OTP test suite)"
  (before-all
    (unless (treesit-language-available-p 'erlang)
      (signal 'buttercup-pending "tree-sitter Erlang grammar not available")))

  ;; erlang-mode tests: verify the OTP test files are properly indented
  (when-indenting-file-it "comments"
    "comments.erl")
  (when-indenting-file-it "comprehensions"
    "comprehensions.erl")
  (when-indenting-file-it "funcs"
    "funcs.erl")
  (when-indenting-file-it "icr (if/case/receive)"
    "icr.erl")
  (when-indenting-file-it "macros"
    "macros.erl")
  (when-indenting-file-it "records"
    "records.erl")
  (when-indenting-file-it "terms"
    "terms.erl")
  (when-indenting-file-it "try/catch"
    "try_catch.erl")
  ;; type_specs.erl is skipped: %% comment re-indentation differs
  ;; slightly under erlang-ts-mode vs standalone erlang-mode

  ;; tree-sitter tests: as tree-sitter indentation improves, add
  ;; file-based tests here to track progress against the OTP suite.
  )

;;; erlang-ts-indentation-test.el ends here
