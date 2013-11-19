#lang racket/base

;; from Eli

(provide recover-source-syntax)

;; -------------------- utilities

(define (syntax-loc stx) (list (syntax-source stx) (syntax-position stx) (syntax-span stx)))


;; -------------------- the real stuff

;; Look for (the outermost) syntax in `orig' that has the same
;; location as `lookfor' which is coming from the expanded `orig',
;; given in `expanded'.
(define (recover-source-syntax orig expanded #:traverse-now? [now? #f])
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
  ;; add-to-table: stx or #f -> stx or #f
  ;; #f as `lookfor` indicates "traverse all of `expanded`
  (define (add-to-table lookfor)
    ;; stx is expanded syntax, target is source syntax
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
         [(and lookfor (eq? stx lookfor)) new-target]

         ;; take apart stx and loop on the components
         [else
          (let inner ([stxe (syntax-e stx)])
            (cond [(list? stxe)
                   (for/or ([x (in-list stxe)])
                     (loop x new-target))]
                  [(pair? stxe) ; may be an improper syntax list
                   (or (loop (car stxe) new-target) (inner (cdr stxe)))]
                  [(syntax? stxe) ; base case
                   (loop stxe new-target)]
                  [else
                   #f]))])]
       [else #f])))

  ;; if now?, add everything to the table
  (when now?
    (add-to-table #f))

  (lambda (lookfor)
    (or
     ;; we just might get a lookfor that is already in the original
     (and (eq? src (syntax-source lookfor))
          (hash-ref syntax-locs (syntax-loc lookfor) #f))
     (hash-ref parent-table lookfor (λ ()
                                      (cond [now? #f]
                                            [else (add-to-table lookfor)
                                                  (hash-ref parent-table lookfor #f)]))))))
