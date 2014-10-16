#lang racket/base
(require racket/gui/base
         racket/class
         mrlib/text-string-style-desc
         "private/drracket-test-util.rkt"
         (for-syntax racket/base))

;; Test suite for the coverage annotations in the teaching languages.
;; Each test case specifies a teaching language (via a regexp), a string
;; to put in the definitions window and a list of strings that correspond to
;; maximal uncovered regions from the original program. Running this file
;; will fire up a drracket, put the program in the definitions, run the 
;; program, ensure that the output is only whitespace and numbers, and then 
;; make sure the coverage is as expected.

;; NB: the results of executing each of these tests (ie the stuff that shows up in the 
;; interactions window when you run the program) should be whitespace and numbers only
;; (well, plus the prompt)


(struct test (lang-regexp program uncovered line))

(define-syntax (t stx)
  (syntax-case stx ()
    [(t a b c)
     (with-syntax ([line (syntax-line #'t)])
       #'(test a b c line))]))

(define tests
  (list (t #rx"Beginning Student$"
           "(define (f x) x)"
           '("x"))
        (t #rx"Beginning Student$"
           "(define (f x) x) (f 1)"
           '())
        (t #rx"Beginning Student$"
           "(define-struct s (a b))"
           '())
        (t #rx"Beginning Student$"
           #<<--
(define (f x)
  (... (cond
         [(null? x) ???]
         [else      (first x)
                    (f (rest x))])))
--
           '())
        
        (t #rx"Intermediate Student$"
           "(define-struct s (a b))"
           '())
        (t #rx"Advanced Student$"
           #<<--
(define-struct foo (x))
;; ----------------------------------------------------
;; MF: something broke about signatures 
;; Shriram said at Dagstuhl that he doesn't use them,
;; and I don't know of anyone else who uses them in *SL
;; ----------------------------------------------------
;; (: make-foo (Number -> foo))
;; (: foo-x (foo -> Number))

(define x (make-foo 5))
(foo-x x)
--
           '())))

;; get-annotate-output : drscheme-frame -> (listof str/ann)
(define (get-annotated-output drs)
  (let ([chan (make-channel)])
    (queue-callback
     (λ ()
       (channel-put chan (get-string/style-desc (send drs get-definitions-text)))))
    (channel-get chan)))

;; returns #t if an element of the result of get-string/style-desc
;; corresponds to an uncovered region of the editor
(define (is-uncovered? ele)
  (let ([style (list-ref ele 1)])
    (eq? style 'test-coverage-off)))

;; find-uncovered-text : list[get-string/style-desc result] -> (listof string)
;; returns strings containing the uncovered text in the editor (in the order they appear in the file)
(define (find-uncovered-text string/style-desc)
  (map car (filter is-uncovered? string/style-desc)))

(fire-up-drracket-and-run-tests
 (λ ()
   (let* ([drr-frame (wait-for-drracket-frame)]
          [definitions-text (send drr-frame get-definitions-text)]
          [interactions-text (send drr-frame get-interactions-text)])
       
     (let ([last-lang #f])
       (for ([t (in-list tests)])
         
         
         (let* ([this-lang (test-lang-regexp t)]
                [same-last-time? (and (regexp? last-lang)
                                      (equal? (object-name last-lang)
                                              (object-name this-lang)))])
           (unless same-last-time?
             (set! last-lang this-lang)
             (set-language-level! (list this-lang))))
         
         (clear-definitions drr-frame)
         (insert-in-definitions drr-frame (test-program t))
         (do-execute drr-frame)
         
         (let ([result (fetch-output
                        drr-frame
                        (send interactions-text paragraph-start-position 2)
                        (send interactions-text last-position))])
           (unless (regexp-match #rx"^[ \n\t0-9>]*$" result)
             (eprintf "FAILED line ~a, got ~s for the output, but expected only digits and whitespace"
                      (test-line t)
                      result)))

         (let ([got (find-uncovered-text (get-annotated-output drr-frame))])
           (unless (equal? got (test-uncovered t))
             (eprintf "FAILED line ~a\n     got: ~s\nexpected: ~s\n"
                      (test-line t)
                      got
                      (test-uncovered t)))))))))
