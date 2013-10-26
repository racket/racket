#lang racket/base

;; from Eli

(provide recover-source-syntax)

;; -------------------- utilities

(define (syntax-loc stx) (list (syntax-source stx) (syntax-position stx) (syntax-span stx)))


;; -------------------- the real stuff



;; Look for `lookfor' in `enclosing', return chain of syntaxes from
;; the innermost out of only syntaxes with the given src, returns #f
;; if it can't find it.
(define (enclosing-syntaxes-with-source enclosing lookfor src)
  (let loop ([r '()] [stx enclosing])
    ;(printf "stx is ~a\n" (syntax->datum stx))
    ;(printf "source is ~a\n" (syntax-source stx))
    (let* ([r* (if (and (syntax? stx) (eq? src (syntax-source stx)))
                   (cons stx r)
                   r)])
      (if (eq? stx lookfor)
          r*
          (let ([stx (if (syntax? stx) (syntax-e stx) stx)])
            (and (pair? stx)
                 (or (loop r* (car stx)) (loop r* (cdr stx)))))))))




;; Look for (the outermost) syntax in `orig' that has the same
;; location as `lookfor' which is coming from the expanded `orig',
;; given in `expanded'.
(define (recover-source-syntax orig expanded)
  (define src (syntax-source orig))

  ;; this maps source locations that are from orig to their syntax
  (define syntax-locs (make-hash))

  ;; build `syntax-locs`
  (let loop ([stx orig])
    (when (syntax? stx) (hash-set! syntax-locs (syntax-loc stx) stx))
    (let ([stx (if (syntax? stx) (syntax-e stx) stx)])
      (when (pair? stx) (loop (car stx)) (loop (cdr stx)))))

  ;; this maps syntax from expanded to the original
  (define parent-table (make-hasheq))

  ;; if `expanded` is mapped to something, then we'll start with it
  (define initial-target
      (hash-ref syntax-locs (syntax-loc expanded) #f))

  ;; this searches for lookfor in orig, building up the table as we go
  (define (add-to-table lookfor)
    (let loop ([stx expanded] [target initial-target])
      (cond
       [(syntax? stx)
	(define new-target
	  ;; check if `stx` has the same srcloc as something in orig
	  ;; in which case it's a good target to use
	  ;; otherwise keep using the old target
	  (hash-ref syntax-locs (syntax-loc stx) target))
	;; map `stx` to the best enclosing syntax we have, if it's not already there
	(hash-ref! parent-table stx new-target)
	(cond
	 ;; if we got what we came for, stop
	 [(eq? stx lookfor) new-target]

	 ;; take apart stx and loop on the components
	 [else
         (define stxe (syntax-e stx))
         (and (pair? stxe)
              (or (loop (car stxe) stx) (loop (cdr stxe) stx)))])]
       [else #f])))

  (lambda (lookfor)
    (or
     ;; we just might get a lookfor that is already in the original
     (and (eq? src (syntax-source lookfor))
          (hash-ref syntax-locs (syntax-loc lookfor) #f))
     (hash-ref parent-table lookfor (λ ()
                                      (add-to-table lookfor)
                                      (hash-ref parent-table lookfor #f))))))
