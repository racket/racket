#lang racket/base

(require "test-suite-utils.rkt"
         (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing highlight-range method
;;


(define (test-text-balanced? number str start end expected)
  (test
   (string->symbol (format "racket:text-balanced?-~a" number))
   (lambda (x) 
     (equal? x expected))
   (λ ()
     (queue-sexp-to-mred
      `(let ([t (new racket:text%)])
         (send t insert ,str)
         (racket:text-balanced? t ,start ,end))))))

(test-text-balanced? 0 "" 0 #f #f)
(test-text-balanced? 1 "  \n " 0 #f #f)
(test-text-balanced? 2 "foo)" 0 #f #t)
(test-text-balanced? 3 "(foo" 0 #f #f)
(test-text-balanced? 4 "(foo)" 0 #f #t)
(test-text-balanced? 5 "(foo 'bar))" 0 #f #t)
(test-text-balanced? 6 "(foo) bar ([buz])" 0 #f #t)
(test-text-balanced? 7 "(foo]" 0 #f #t)
(test-text-balanced? 8 "{foo} ((bar) [5.9])" 0 #f #t)
(test-text-balanced? 9 "#(1 2 . 3)" 0 #f #t)

(define (test-indentation which before after)
  (test
   (string->symbol (format "racket:test-indentation-~a" which))
   (λ (x) (equal? x after))
   (λ ()
     (queue-sexp-to-mred
      `(let* ([t (new racket:text%)]
              [f (new frame% [label ""] [width 600] [height 600])]
              [ec (new editor-canvas% [parent f] [editor t])])
         (send f reflow-container)
         (send t insert ,before)
         (send t tabify-all)
         (send t get-text))))))

(test-indentation 1 "a" "a")
(test-indentation 2 "(a\n b)" "(a\n b)")
(test-indentation 3 "(a\nb)" "(a\n b)")
(test-indentation 3 "(a b\nc)" "(a b\n   c)")
(test-indentation 3 "(a ...\nb)" "(a ...\n b)")
(test-indentation 4 "(lambda (x)\nb)" "(lambda (x)\n  b)")
(test-indentation 5 "(lambdaa (x)\nb)" "(lambdaa (x)\n         b)")
(test-indentation 6
                  "(define x\n  (let/ec return\n    (when 1\n      (when 2\n\t\t      3))\n    2))"
                  "(define x\n  (let/ec return\n    (when 1\n      (when 2\n        3))\n    2))")

(define (test-magic-square-bracket which before after)
  (test
   (string->symbol (format "racket:test-magic-square-bracket-~a" which))
   (λ (x) (equal? x after))
   (λ ()
     (queue-sexp-to-mred
      `(let* ([t (new racket:text%)]
              [f (new frame% [label ""] [width 600] [height 600])]
              [ec (new editor-canvas% [parent f] [editor t])])
         (send f reflow-container)
         (send t insert ,before)
         (send (racket:get-keymap) call-function "maybe-insert-[]-pair-maybe-fixup-[]" t (new event%))
         (send t get-text))))))

(queue-sexp-to-mred `(preferences:set 'framework:automatic-parens #f))
(queue-sexp-to-mred `(preferences:set 'framework:fixup-open-parens #t))
(test-magic-square-bracket 'mt "" "(")
(test-magic-square-bracket 'mt2 "(() " "(() (")
(test-magic-square-bracket 'mt3 "([] " "([] [")
(test-magic-square-bracket 'mt4 "(\"" "(\"[")
(test-magic-square-bracket 'mt4 "(#\\" "(#\\[")
(test-magic-square-bracket 'let1 "(let " "(let (")
(test-magic-square-bracket 'let2 "(let (" "(let ([")
(test-magic-square-bracket 'let3 "(let loop " "(let loop (")
(test-magic-square-bracket 'let3 "(let loop (" "(let loop ([")
(test-magic-square-bracket 'cond1 "(cond " "(cond [")
(test-magic-square-bracket 'cond2 "(cond [" "(cond [(")
(test-magic-square-bracket 'with-syntax1 "(syntax-case x " "(syntax-case x (")
(test-magic-square-bracket 'with-syntax2 "(syntax-case x () " "(syntax-case x () [")
(test-magic-square-bracket 'with-syntax3 "(syntax-case 'x " "(syntax-case 'x (")
(test-magic-square-bracket 'with-syntax4 "(syntax-case 'x () " "(syntax-case 'x () [")
(test-magic-square-bracket 'with-syntax3 "(syntax-case #'x " "(syntax-case #'x (")
(test-magic-square-bracket 'with-syntax4 "(syntax-case #'x () " "(syntax-case #'x () [")
(test-magic-square-bracket 'local1 "(local " "(local [")
(test-magic-square-bracket 'local2 "(local [" "(local [(")
(test-magic-square-bracket 'local2 "(local [(define x 1)] " "(local [(define x 1)] (")


(define (test-message-send/proc line before expected pos msg
                                #:check-result? [check-result? #f] 
                                . args)
  (define (maybe-quote x)
    (cond
      [(or (number? x) (boolean? x) (string? x)) x]
      [else `',x]))
  (test
   (string->symbol (format "line ~a: ~s"
                           line
                           `(,msg ,@(map maybe-quote args))))
   (λ (x) (equal? x expected))
   (λ ()
     (queue-sexp-to-mred
      `(let ()
         (define f (new frame% [label ""]))
         (define t (new racket:text%))
         (define ec (new editor-canvas% [parent f] [editor t]))
         (send t insert ,before)
         (send t set-position ,pos)
         (define ans (send t ,msg ,@(map maybe-quote args)))
         ,(if check-result?
              'ans
              '(send t get-text)))))))
(define-syntax (test-message-send stx)
  (syntax-case stx ()
    [(_ before expected pos mth . args)
     (with-syntax ([line (syntax-line stx)])
       #'(test-message-send/proc line before expected pos 'mth . args))]))

(test-message-send ""  "]"  0 insert-close-paren 0 #\] #t #t 'adjacent)
(test-message-send ""  "]"  0 insert-close-paren 0 #\] #t #t #f)
(test-message-send "(" "()" 1 insert-close-paren 1 #\] #t #t #f)
(test-message-send "(" "(]" 1 insert-close-paren 1 #\] #f #f #f)
(test-message-send ""  "]"  0 insert-close-paren 0 #\] #t #t 'forward)

(test-message-send "(1)" "1" 1 kill-enclosing-parens 1)
(test-message-send "(1 2 3)" "1 2 3" 3 kill-enclosing-parens 3)
(test-message-send "()" "" 1 kill-enclosing-parens 1)
(test-message-send "(1\n 2\n 3)" "1\n2\n3" 1 kill-enclosing-parens 1) ;; test tabify call

(test-message-send "abc" #f 1 backward-containing-sexp #:check-result? #t 1 3)

;; tests what happens when a given key/s is/are typed in an editor with initial
;;       text and cursor position, under different settings of the auto-parentheses and
;;       smart-skip-parentheses preferences   .nah.

;; test-auto-parens-behavior
;;    : any string 
;;      [or num (list num num)] [or char symbol 1string (list char) (list key-event%)]
;;      [or num (list num num)] string
(define (test-auto-parens-behavior which initial-text initial-pos keys final-text final-pos
                                   [auto-parens? #f])
  (test
   (string->symbol (format "racket:test-auto-parens-behavior ~a" which))
   (λ (x) (if (list? final-pos)
              (equal? x (list (car final-pos) (cadr final-pos) final-text))
              (equal? x (list final-pos final-pos final-text))))
   (λ ()
     (queue-sexp-to-mred
      `(let* ([t (new racket:text%)]
              [f (new frame% [label ""] [width 600] [height 600])]
              [ec (new editor-canvas% [parent f] [editor t])])
         (preferences:set 'framework:fixup-parens #t)
         (preferences:set 'framework:automatic-parens ,auto-parens?)
         (send f reflow-container)
         (send t insert ,initial-text)
         ,(if (number? initial-pos)
              `(send t set-position ,initial-pos)
              `(send t set-position ,(car initial-pos) ,(cadr initial-pos)))
         ,@(map
            (lambda (k)
              (cond [(char? k)
                     `(send (racket:get-keymap)
                            handle-key-event t (new key-event% [key-code ,k]))]
                    [(string? k)
                     `(send (racket:get-keymap)
                            handle-key-event t (new key-event% [key-code ,(car (string->list k))]))]
                    [(symbol? k)
                     `(send (racket:get-keymap)
                            handle-key-event t (new key-event% [key-code (quote ,k)]))]
                    [else `(send (racket:get-keymap) handle-key-event t ,k)]))
            (if (list? keys) keys (list keys)))
         (list (send t get-start-position) (send t get-end-position) (send t get-text)))))))


;; this takes an initial editor state (specified by the text before the cursor,
;;   some selected text (may be blank string), and text after the cursor), and
;;   a key(s), and runs tests to check what happens when that key(s) is/are
;;   typed - in both possible settings of the 'automatic-parens preference
;;
;; final-states is a list of 2-pairs of strings. each pair is the final text before
;;   and after the cursor, for auto-parens disabled and enabled respectively
;;  (NB. final-states could also contain 3-pairs of strings, the middle portion 
;;       representing text that is selected after the insertion)
(define (test-parens-behavior/full which
                               init-text-before init-text-selected init-text-after
                               keys
                               final-states)
  (define initial-text (string-append init-text-before init-text-selected init-text-after))
  (define initial-start-pos (string-length init-text-before))
  (define initial-end-pos (+ initial-start-pos (string-length init-text-selected)))
  (for-each
   (lambda (label auto? final-pair)
     (test-auto-parens-behavior (format "~a (~a)" which label)
                             initial-text (list initial-start-pos initial-end-pos) keys
                             (apply string-append final-pair)
                             (if (= 3 (length final-pair)) 
                                 ; final-pair could actually be a triplet 
                                 ; to indicate residual selection after insertion
                                 (list (string-length (car final-pair))
                                       (string-length (string-append (car final-pair)
                                                                     (cadr final-pair))))
                                 (string-length (car final-pair)))
                             auto?))
   '("no-auto-parens" "with-auto-parens")
   '(#f #t)
   final-states))


(define SPECIAL-CHARS '(#\( #\) #\[ #\] #\" #\| #\{ #\}))

(for ([k SPECIAL-CHARS])
  ;; test that character literals never result in a pair of characters typed...
  (test-parens-behavior/full (format "literal-~a" k)
                             "(list 1 #\\" "" ")"
                             k
                             `([,(string-append "(list 1 #\\" (string k)) ")"]
                               [,(string-append "(list 1 #\\" (string k)) ")"]))
  ;; test that escaped characters in a string never result in a pair of characters typed...
  ;; except for | which is a hard case to detect, because the tokenizer ends up
  ;; in an error state
  (unless (or (eq? #\| k))
      (test-parens-behavior/full (format "literal-~a-in-string" k)
                             "\"abc \\" "" "def\""
                             k
                             `([,(string-append "\"abc \\" (string k)) "def\""]
                               [,(string-append "\"abc \\" (string k)) "def\""])))
  ;; test that auto-parens has no effect in strings, *except for double quotes*
  (unless (eq? #\" k)
    (test-parens-behavior/full (format "~a-in-string" k)
                               "\" abc def " "" " \""
                               k
                               `([,(string-append "\" abc def " (string k)) " \""]
                                 [,(string-append "\" abc def " (string k)) " \""])))

  ;; test that auto-parens has no effect in various comment situations
  (define scenarios
    ;    description     before-cursor    after-cursor
    '(("in-line-comment"  ";; abc def "    " ghi ")
      ("end-of-line-comment" ";; abc def " "")
      ("end-of-line-comment-with-close-paren" ";; abc def " "   ) \n )")
      ("in-block-comment" "#| abc def "  " ghi |#")
      ))
  (for ([s scenarios])
    (let* ([before (cadr s)]
           [after (caddr s)]
           [before-final (string-append before (string k))]
           [result (list before-final after)])
      (test-parens-behavior/full (format "~a-~a" k (car s))
                                 before "" after k `(,result ,result)))))

;;; assorted other scenarios...
(test-parens-behavior/full 'open-parens
                           "abcd" "" "efg"  ; editor state: before, selected, after
                           #\(              ; key(s) pressed
                           '(["abcd(" "efg"]  ; result state sep by cursor, no auto-parens
                             ["abcd(" ")efg"])) ; result state with auto-parens

(test-parens-behavior/full 'close-1
                           "abcd" "" "efg"
                           #\)
                           '(["abcd)" "efg"]  ["abcd)" "efg"]))
(test-parens-behavior/full 'close-2
                           "(abcd" "" "efg"
                           #\)
                           '(["(abcd)" "efg"]  ["(abcd)" "efg"]))
(test-parens-behavior/full 'close-3
                           "(abcd" "" ")efg"
                           #\)
                           '(["(abcd)" ")efg"]  ["(abcd)" "efg"]))
(test-parens-behavior/full 'close-4
                           "(abcd efg " "" "   ) efg"
                           #\)
                           '(["(abcd efg )" "   ) efg"]
                             ["(abcd efg    )" " efg"]))
(test-parens-behavior/full 'close-5
                           "(define before+afters `([\"\" abc \"efg\" 12345 xyz]) [84])"
                           ""
                           ""
                           #\)
                           '(["(define before+afters `([\"\" abc \"efg\" 12345 xyz]) [84]))" ""]
                             ["(define before+afters `([\"\" abc \"efg\" 12345 xyz]) [84]))" ""]))
(test-parens-behavior/full 'close-6
                           "(define before+afters `([\"\" abc \"efg\""
                           ""
                           " 12345 xyz]) [84])"
                           #\)
                           '(["(define before+afters `([\"\" abc \"efg\"]" " 12345 xyz]) [84])"]
                             ["(define before+afters `([\"\" abc \"efg\"]" " 12345 xyz]) [84])"]))


(test-parens-behavior/full 'close-skip-1
                           "(define before+afters `([\"\" abc \"efg\" 12345 xyz]"
                           ""
                           "  ) [84])"
                           #\)
                           '(["(define before+afters `([\"\" abc \"efg\" 12345 xyz])" "  ) [84])"]
                             ["(define before+afters `([\"\" abc \"efg\" 12345 xyz]  )" " [84])"]))
(test-parens-behavior/full 'close-skip-fixup-1
                           "(define before+afters `{[abc 123]"
                           ""
                           "  ) [84])"
                           #\)   ; here the next close after ) doesn't match the {, so no skip happens
                           '(["(define before+afters `{[abc 123]}" "  ) [84])"]
                             ["(define before+afters `{[abc 123]}" "  ) [84])"]))
(test-parens-behavior/full 'close-skip-fixup-2
                           "(define before+afters `{[abc 123]"
                           ""
                           "  } [84])"
                           #\)   ; here the next close does match the {, so  skip
                           '(["(define before+afters `{[abc 123]}" "  } [84])"]
                             ["(define before+afters `{[abc 123]  }" " [84])"]))

(test-parens-behavior/full 'surround-open-1
                           "abcd" "ef" "g"
                           #\(
                           '(["abcd(" "g"]  ["abcd(" "ef)g"]))

(test-parens-behavior/full 'double-quote-1
                           "" "" ""
                           #\"
                           '(["\"" ""] ["\"" "\""]))
(test-parens-behavior/full 'double-quote-2
                           "abc " "" ""
                           #\"
                           '(["abc \"" ""] ["abc \"" "\""]))
(test-parens-behavior/full 'double-quote-selection-1
                           "(abc " "def 123" " xyz]"
                           #\"
                           '(["(abc \"" " xyz]"] ["(abc \"" "def 123\" xyz]"]))
(test-parens-behavior/full 'double-quote-skip-1
                           "\"abc def " "" "\" 123"
                           #\"
                           '(["\"abc def \"" "\" 123"] ["\"abc def \"" " 123"]))
(test-parens-behavior/full 'double-quote-escaped-1
                           "\"abcd \\" "" ""
                           #\"
                           '(["\"abcd \\\"" ""]
                             ["\"abcd \\\"" "\""])) ; this inserts double since string was not closed
(test-parens-behavior/full 'double-quote-escaped-2
                           "\"abcd \\" "" "\""
                           #\"
                           '(["\"abcd \\\"" "\""]
                             ["\"abcd \\\"" "\""]))

(test-parens-behavior/full 'bar
                           "abc " "" "123"
                           #\|
                           '(["abc |" "123"] ["abc |" "|123"]))
(test-parens-behavior/full 'bar-literal
                           "(list 1 #\\" "" ")"
                           #\|
                           '(["(list 1 #\\|" ")"] ["(list 1 #\\|" ")"]))
(test-parens-behavior/full 'bar-skip
                           "abc |def" "" "|123"
                           #\|
                           '(["abc |def|" "|123"] ["abc |def|" "123"]))
(test-parens-behavior/full 'bar-selection
                           "abc |def " "hij" "|123"
                           #\|
                           '(["abc |def |" "|123"] ["abc |def |" "hij||123"]))


(test-parens-behavior/full 'block-comment-1
                           " #" "" ""
                           #\|
                           '([" #|" ""]
                             [" #|" "|#"]))
(test-parens-behavior/full 'block-comment-2
                           "(123 abc#" "" " def 456)"
                           #\|
                           '(["(123 abc#|" " def 456)"]
                             ["(123 abc#|" "|# def 456)"]))
(test-parens-behavior/full 'block-comment-skip-1
                           "#| (123 abc" "" "|# def 456)"
                           #\|
                           '(["#| (123 abc|" "|# def 456)"]
                             ["#| (123 abc|#" " def 456)"]))

(test-parens-behavior/full 'close-adjusts-properly-when-space-follows-paren
                           "( x" "" ""
                           #\]
                           '(["( x)" "" ""]
                             ["( x)" "" ""]))
(test-parens-behavior/full 'close-adjusts-properly-when-inside-a-comment
                           "[();" "" ""
                           #\)
                           '(["[();)" "" ""]
                             ["[();)" "" ""]))
(test-parens-behavior/full 'close-adjusts-properly-when-inside-a-comment.2
                           "[;" "" "\n"
                           #\)
                           '(["[;)" "" "\n"]
                             ["[;)" "" "\n"]))
(test-parens-behavior/full 'close-adjusts-properly-when-inside-an-unclosed-string
                           "[()\"" "" ""
                           #\)
                           '(["[()\")" "" ""]
                             ["[()\")" "" ""]))
(test-parens-behavior/full 'close-adjusts-properly-when-inside-a-string
                           "[()\"" "" "\""
                           #\)
                           '(["[()\")" "" "\""]
                             ["[()\")" "" "\""]))

(test-parens-behavior/full 'close-adjusts-properly-when-no-containing-sexp
                           ")" "" ""
                           #\]
                           '([")]" "" ""]
                             [")]" "" ""]))

(test-parens-behavior/full '|"-splits-string|
                           " \"abcd" "" "efg\" "
                           #\"
                           '([" \"abcd\""     "efg\" "]
                             [" \"abcd\" "    "\"efg\" "]))
(test-parens-behavior/full '|"-splits-string-at-beginning|
                           " \"" "" "abcdefg\" "
                           #\"
                           '([" \"\""     "abcdefg\" "]
                             [" \"\" "    "\"abcdefg\" "]))
(test-parens-behavior/full '|"-splits-out-selected-string|
                           " \"abc" "def" "ghi\" "
                           #\"
                           '([" \"abc\"" "" "ghi\" "]
                             ; test that "def" remains selected afterwards...
                             [" \"abc\" "   "\"def\""    " \"ghi\" "]))



#| for these, the key-event with meta-down doesn't seem to work... maybe a Mac OS
  issue; and may cause problems with these tests on another platform? .nah. |#
(when (equal? 'macosx (system-type))
  (test-parens-behavior/full 'meta-open-1
                             "abcd" "" "efg"
                             '(escape #\()   ; '((new key-event% [key-code #\(] [meta-down #t]))
                             '(["abcd(" ")efg"]  ["abcd(" ")efg"]))

  (test-parens-behavior/full 'meta-close-skip-1
                             "(define before (list 1 2" "" " 3 4)"
                             '(escape #\))   ; '((new key-event% [key-code #\)] [meta-down #t]))
                             '(["(define before (list 1 2 3 4)" ""]
                               ["(define before (list 1 2 3 4)" ""]))
  (test-parens-behavior/full
   'meta-close-skip-2
   "#lang racket\n(define before+afters `([\"\" abc \"efg\""
   ""
   " 12345 xyz] [84])"
   '(escape #\))    ;'((new key-event% [key-code #\)] [meta-down #t]))
   '(["#lang racket\n(define before+afters `([\"\" abc \"efg\" 12345 xyz]" " [84])"]
     ["#lang racket\n(define before+afters `([\"\" abc \"efg\" 12345 xyz]" " [84])"]))
  (test-parens-behavior/full 'meta-close-skip-3
                             "(define before" "" " (list 1 2 3 4)"
                             '(escape #\))   ; '((new key-event% [key-code #\)] [meta-down #t]))
                             '(["(define before (list 1 2 3 4)" ""]
                               ["(define before (list 1 2 3 4)" ""])))

