
;; Basic checks for the beginner language. Error messages really
;; should be inspected manually, but there's some support for
;; automatic checking.

;; Limitations of this test suite:
;;  - It doesn't check reader-level parameterization, such as use of quotes
;;  - It doesn't check format of printed results
;;  - It doesn't check the absence of Racket forms

;; Don't try to run other tests from the test suite after loading this
;; one into a particular namespace.

;; See also htdp-image.rkt, which must be loaded into GRacket (but is in this
;; directory anyway)

;; Tests that apply to all languages go in beg-adv.rkt.
;; Tests that apply only to beginner through intermediate go in beg-intm.rkt,
;;  and so on.

;; Writing tests:
;;  
;;   (htdp-test <expected> <name> <expr>)
;;     checks a run-time result; <expected> is
;;     an expression evaluated at the top level,
;;     and <expr> is put into a module and evaluated;
;;     <name> is usually a symbol, and it is used only for
;;     naming the test in output
;;
;;   (htdp-err/rt-test <expr>)
;;   (htdp-err/rt-test <expr> <exn-predicate>)
;;   (htdp-err/rt-test <expr> <message-rx-string>)
;;     checks for a run-time error by putting <expr> into a
;;     module and evaluting; if <exn-predicate> is supplied, the
;;     predicate must produce #t for the resulting exception;
;;     if <message-rx-string> is supplied, the exception
;;     message string must match the regexp
;;
;;   (htdp-syntax-test #'<expr>)
;;   (htdp-syntax-test #'<expr> <message-rx-string>)
;;     check for a syntax error, putting <expr> into a module;
;;     if <message-rs-string> is supplied, the syntax error
;;     message must match the regexp
;;
;;   (htdp-top <expr>)
;;     imperatively adds an expression to be included into
;;     test modules
;;   (htdp-top-pop <n>)
;;     removes the last <n> added expressions
;;

(load-relative (collection-file-path "loadtest.rktl" "tests/racket"))

;; Check that expansion doesn't introduce non-equal ids that
;;  claim to be "original" at the same place
(let loop ([x (expand #'(module m (lib "htdp-beginner.rkt" "lang")
			  (define (f x) x)))])
  (let ([orig-ids (let loop ([x x])
		    (cond
		     [(identifier? x)
		      (if (syntax-original? x)
			  (list x)
			  null)]
		     [(null? x) null]
		     [(pair? x) (append (loop (car x))
					(loop (cdr x)))]
		     [(syntax? x) (loop (syntax-e x))]
		     [else null]))])
    (for-each (lambda (id1)
		(for-each (lambda (id2)
			    (when (and (= (syntax-position id1)
                                          (syntax-position id2))
                                       (not (free-identifier=? id1 id2)))
                              (error 'original "mismatch: ~e ~e"
                                     id1 id2)))
			  orig-ids))
	      orig-ids)))
    
;; Don't need these:
(define no-extra-if-tests? #t)

(require (only-in mzscheme exn:fail? exn:fail:contract?))

(define current-htdp-lang 'lang/htdp-beginner)
(load-relative "htdp-test.rktl")

(require (lib "htdp-beginner.rkt" "lang"))

(load-relative "beg-adv.rktl")
(load-relative "beg-intml.rktl")
(load-relative "beg-intm.rktl")
(load-relative "beg-bega.rktl")

(htdp-syntax-test #'quote "quote: expected an open parenthesis before quote, but found none")
(htdp-syntax-test #''1 "quote: expected the name of the symbol after the quote, but found a number")
(htdp-syntax-test #''"hello" "quote: expected the name of the symbol after the quote, but found a string")
(htdp-syntax-test #''(1 2) "quote: expected the name of the symbol after the quote, but found a part")
(htdp-syntax-test #'''a "quote: expected the name of the symbol after the quote, but found a part")

(report-errs)
