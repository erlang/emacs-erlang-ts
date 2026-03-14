;;; erlang-ts-indentation-test.el --- Indentation tests for erlang-ts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Buttercup tests for erlang-ts-mode indentation.
;; Since erlang-ts-mode derives from erlang-mode, it inherits
;; erlang-mode's indentation engine.  These tests verify that
;; indentation works correctly under erlang-ts-mode.

;;; Code:

(require 'buttercup)
(require 'erlang-ts)

(defun erlang-ts-test--strip-indentation (code)
  "Remove all leading whitespace from each line of CODE."
  (mapconcat
   (lambda (line) (string-trim-left line))
   (split-string code "\n")
   "\n"))

(defmacro when-indenting-it (description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
DESCRIPTION is the test name.  Each element of CODE-STRINGS is a
properly-indented Erlang code string.  The macro strips indentation,
re-indents via `erlang-ts-mode', and asserts the result matches the original."
  (declare (indent 1))
  `(it ,description
     ,@(mapcar
        (lambda (code)
          `(let ((expected ,code))
             (expect
              (with-temp-buffer
                (insert (erlang-ts-test--strip-indentation expected))
                (erlang-ts-mode)
                (setq-local indent-tabs-mode nil)
                (indent-region (point-min) (point-max))
                (buffer-string))
              :to-equal expected)))
        code-strings)))

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

  (when-indenting-it "indents a list comprehension"
    "doubles(Xs) ->
    [X * 2 || X <- Xs].")

  (when-indenting-it "indents a record definition"
    "-record(person, {
                 name :: string(),
                 age :: non_neg_integer()
                }).")

  (when-indenting-it "indents a type definition"
    "-type color() :: red | green | blue.")

  (when-indenting-it "indents begin/end"
    "run() ->
    begin
        step1(),
        step2()
    end.")

  (when-indenting-it "indents nested case expressions"
    "nested(X, Y) ->
    case X of
        a ->
            case Y of
                b ->
                    ok;
                _ ->
                    error
            end;
        _ ->
            skip
    end."))

;;; erlang-ts-indentation-test.el ends here
