#lang racket/base
(require unstable/struct
         syntax/stx
         "minimatch.rkt")
(provide ps-empty
         ps-add-car
         ps-add-cdr
         ps-add-post
         ps-add-stx
         ps-add-unbox
         ps-add-unvector
         ps-add-unpstruct
         ps-add-opaque

         #|
         ps->stx+index
         |#
         ps-context-syntax
         ps-difference

         invert-ps
         maximal/progress

         progress->sexpr)

#|
Progress (PS) is a non-empty list of Progress Frames (PF).

A PF is one of
  - stx     ;; "Base" frame
  - 'car
  - nat     ;; Represents that many repeated cdrs
  - 'post
  - 'opaque

stx frame introduced
  - always at base (that is, by syntax-parse)
    - if syntax-parse has #:context arg, then two stx frames at bottom:
      (list to-match-stx context-stx)
  - by #:with/~parse
  - by #:fail-*/#:when/~fail & stx

Interpretation: Inner PS structures are applied first.
 eg, (list 'car 1 #'___)
      means ( car of ( cdr once of the term ) )
      NOT apply car, then apply cdr once, then stop
|#

(define (ps-empty stx ctx)
  (if (eq? stx ctx)
      (list stx)
      (list stx ctx)))
(define (ps-add-car parent)
  (cons 'car parent))
(define (ps-add-cdr parent [times 1])
  (if (zero? times)
      parent
      (match (car parent)
        [(? exact-positive-integer? n)
         (cons (+ times n) (cdr parent))]
        [_
         (cons times parent)])))
(define (ps-add-post parent)
  (cons 'post parent))
(define (ps-add-stx parent stx)
  (cons stx parent))
(define (ps-add-unbox parent)
  (ps-add-car parent))
(define (ps-add-unvector parent)
  (ps-add-car parent))
(define (ps-add-unpstruct parent)
  (ps-add-car parent))
(define (ps-add-opaque parent)
  (cons 'opaque parent))

;; ps-context-syntax : Progress -> syntax
(define (ps-context-syntax ps)
  ;; Bottom frame is always syntax
  (car (reverse ps)))

;; ps->stx+index : Progress -> (values stx nat)
;; Gets the innermost stx that should have a real srcloc, and the offset
;; (number of cdrs) within that where the progress ends.
(define (ps->stx+index ps)
  (define (interp ps)
    (match ps
      [(cons (? syntax? stx) _) stx]
      [(cons 'car parent)
       (let ([d (syntax-e (interp parent))])
         (cond [(pair? d) (car d)]
               [(vector? d) (vector->list d)]
               [(box? d) (unbox d)]
               [(prefab-struct-key d) (struct->list d)]
               [else (error 'ps->stx+index "INTERNAL ERROR: unexpected: ~e" d)]))]
      [(cons (? exact-positive-integer? n) parent)
       (for/fold ([stx (interp parent)]) ([i (in-range n)])
         (stx-cdr stx))]
      [(cons 'post parent)
       (interp parent)]))
  (match ps
    [(cons (? syntax? stx) _)
     (values stx 0)]
    [(cons 'car parent)
     (values (interp ps) 0)]
    [(cons (? exact-positive-integer? n) parent)
     (values (interp parent) n)]
    [(cons 'post parent)
     (ps->stx+index parent)]))

;; ps-difference : PS PS -> nat
;; Returns N s.t. B = (ps-add-cdr^N A)
(define (ps-difference a b)
  (define (whoops)
    (error 'ps-difference "~e is not an extension of ~e"
           (progress->sexpr b) (progress->sexpr a)))
  (match (list a b)
    [(list (cons (? exact-positive-integer? na) pa)
           (cons (? exact-positive-integer? nb) pb))
     (unless (equal? pa pb) (whoops))
     (- nb na)]
    [(list pa (cons (? exact-positive-integer? nb) pb))
     (unless (equal? pa pb) (whoops))
     nb]
    [_
     (unless (equal? a b) (whoops))
     0]))

;; ps-truncate-opaque : PS -> PS
(define (ps-truncate-opaque ps)
  (let/ec return
    (let loop ([ps ps])
      (cond [(null? ps)
             null]
            [(eq? (car ps) 'opaque)
             ;; Tricky! We only jump after loop returns,
             ;; so jump closest to end wins.
             (return (loop (cdr ps)))]
            [else
             ;; Either (loop _) jumps, or it is identity
             (loop (cdr ps))
             ps]))))

#|
Progress ordering
-----------------

Lexicographic generalization of partial order on frames
  CAR < CDR < POST, stx incomparable except to self

Progress equality
-----------------

If ps1 = ps2 then both must "blame" the same term,
ie (ps->stx+index ps1) = (ps->stx+index ps2).
|#

;; An Inverted PS (IPS) is a PS inverted for easy comparison.
;; An IPS may not contain any 'opaque frames.

;; invert-ps : PS -> IPS
(define (invert-ps ps)
  (reverse (ps-truncate-opaque ps)))

;; maximal/progress : (listof (cons A IPS)) -> (listof (listof A))
;; Returns a list of equivalence sets.
(define (maximal/progress items)
  (cond [(null? items)
         null]
        [(null? (cdr items))
         (list (list (car (car items))))]
        [else
         (let-values ([(rNULL rCAR rCDR rPOST rSTX leastCDR)
                       (partition/pf items)])
           (append (maximal/pf rNULL rCAR rCDR rPOST leastCDR)
                   (if (pair? rSTX)
                       (maximal/stx rSTX)
                       null)))]))

;; partition/pf : (listof (cons A IPS)) -> (listof (cons A IPS))^5 & nat/+inf.0
(define (partition/pf items)
  (let ([rNULL null]
        [rCAR null]
        [rCDR null]
        [rPOST null]
        [rSTX null]
        [leastCDR #f])
    (for ([a+ips (in-list items)])
      (let ([ips (cdr a+ips)])
        (cond [(null? ips)
               (set! rNULL (cons a+ips rNULL))]
              [(eq? (car ips) 'car)
               (set! rCAR (cons a+ips rCAR))]
              [(exact-positive-integer? (car ips))
               (set! rCDR (cons a+ips rCDR))
               (set! leastCDR
                     (if leastCDR
                         (min leastCDR (car ips))
                         (car ips)))]
              [(eq? (car ips) 'post)
               (set! rPOST (cons a+ips rPOST))]
              [(syntax? (car ips))
               (set! rSTX (cons a+ips rSTX))]
              [else
               (error 'syntax-parse "INTERNAL ERROR in partition/pf: ~e" ips)])))
    (values rNULL rCAR rCDR rPOST rSTX leastCDR)))

;; maximal/pf : (listof (cons A IPS))^4 & nat/+inf.0-> (listof (listof A))
(define (maximal/pf rNULL rCAR rCDR rPOST leastCDR)
  (cond [(pair? rPOST)
         (maximal/progress (rmap pop-item-ips rPOST))]
        [(pair? rCDR)
         (maximal/progress
          (rmap (lambda (a+ips)
                  (let ([a (car a+ips)] [ips (cdr a+ips)])
                    (cond [(= (car ips) leastCDR)
                           (cons a (cdr ips))]
                          [else
                           (cons a (cons (- (car ips) leastCDR) (cdr ips)))])))
                rCDR))]
        [(pair? rCAR)
         (maximal/progress (rmap pop-item-ips rCAR))]
        [(pair? rNULL)
         (list (map car rNULL))]
        [else
         null]))

;; maximal/stx : (listof (cons A IPS)) -> (listof (listof A))
(define (maximal/stx rSTX)
  (let ([stxs null]
        [table (make-hasheq)])
    (for ([a+ips (in-list rSTX)])
      (let* ([ips (cdr a+ips)]
             [entry (hash-ref table (car ips) null)])
        (when (null? entry)
          (set! stxs (cons (car ips) stxs)))
        (hash-set! table (car ips) (cons a+ips entry))))
    (apply append
           (map (lambda (key)
                  (maximal/progress (map pop-item-ips (hash-ref table key))))
                stxs))))

;; pop-item-ips : (cons A IPS) -> (cons A IPS)
(define (pop-item-ips a+ips)
  (let ([a (car a+ips)]
        [ips (cdr a+ips)])
    (cons a (cdr ips))))

(define (rmap f xs)
  (let rmaploop ([xs xs] [accum null])
    (cond [(pair? xs)
           (rmaploop (cdr xs) (cons (f (car xs)) accum))]
          [else
           accum])))

;; == Debugging ==

(provide progress->sexpr)

(define (progress->sexpr ps)
  (for/list ([pf (in-list (invert-ps ps))])
    (match pf
      [(? syntax? stx) 'stx]
      ['car 'car]
      ['post 'post]
      [(? exact-positive-integer? n) n]
      ['opaque 'opaque])))
