
;; Basic checks for the beginner language. Error messages really
;; should be inspected manually.

;; Limitations of this test suite:
;;  - It doesn't check reader-level parameterization, such as use of quotes
;;  - It doesn't check format of printed results
;;  - It doesn't check the absence of MzScheme forms

;; Don't try to run other tests from the test suite after loading this
;; one into a particular namespace.

;; See also htdp-image.ss, which must be loaded into MrEd (but is in this
;; directory anyway)


(load-relative "loadtest.ss")

;; Check that expansion doesn't introduce non-equal ids that
;;  claim to be "original" at the same place
(let loop ([x (expand #'(module m (lib "htdp-beginner.ss" "lang")
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
			    (if (and (= (syntax-position id1)
					(syntax-position id2))
				     (not (module-identifier=? id1 id2)))
				(error 'original "mismatch: ~e ~e"
				       id1 id2)))
			  orig-ids))
	      orig-ids)))
    
;; Don't need these:
(define no-extra-if-tests? #t)

(require (rename mzscheme exn:fail? exn:fail?)
	 (rename mzscheme exn:fail:contract? exn:fail:contract?))

(define current-htdp-lang '(lib "htdp-beginner.ss" "lang"))
(load-relative "htdp-test.ss")

(require (lib "htdp-beginner.ss" "lang"))

(load-relative "beg-adv.ss")
(load-relative "beg-intml.ss")
(load-relative "beg-intm.ss")
(load-relative "beg-bega.ss")

(htdp-syntax-test #'quote)
(htdp-syntax-test #''1)
(htdp-syntax-test #''"hello")
(htdp-syntax-test #''(1 2))
(htdp-syntax-test #'''a)


(report-errs)
