#lang racket/base
(require racket/list
         racket/contract
         slideshow/pict)

#|
TODO

- need a way to express "dependent" additions, when need to find a
  pict within a ppict, eg for lines, balloons, etc.

- [lcr]bl alignments... not sure about [lcr]tl
|#

;; ============================================================
;; Progressive Picts

#|
A ppict contains a pict and a placer (or #f).
A placer = (placer (pict (listof (U pict real #f)) -> ppict))
In a placer function's arguments:
  a number means to change the separation spacing
  a #f is just ignored
  FIXME: clarify, for following or including current gap?
|#
(struct ppict pict (placer))
(struct placer (fun))

(define (mk-ppict p placer)
  (ppict (pict-draw p)
         (pict-width p)
         (pict-height p)
         (pict-ascent p)
         (pict-descent p)
         (list (make-child p 0 0 1 1))
         #f
         (pict-last p)
         placer))

(define (ppict-pict dp)
  (child-pict (car (pict-children dp))))

;; ----

;; ppict-add : ppict (U pict real #f) ... -> ppict
(define (ppict-add dp . picts)
  (let ([pl (ppict-placer dp)])
    ((placer-fun pl) (ppict-pict dp) picts)))

;; ppict-go : pict placer -> ppict
(define (ppict-go dp pl)
  (cond [(ppict? dp)
         (mk-ppict (ppict-pict dp) pl)]
        [(pict? dp)
         (mk-ppict dp pl)]))

;; ----

(define (grid cols rows col row [align 'cc]
              #:abs-x [abs-x 0]
              #:abs-y [abs-y 0]
              #:compose [compose (halign->vcompose (align->h align))]
              #:sep [sep 0])
  ;; row, column indexes are 1-based
  (define halign (align->h align))
  (define valign (align->v align))
  (define xfrac (/ (+ (sub1 col) (align->frac halign)) cols))
  (define yfrac (/ (+ (sub1 row) (align->frac valign)) rows))
  (refpoint* xfrac yfrac abs-x abs-y halign valign compose sep #f))

(define (coord xfrac yfrac [align 'cc]
               #:abs-x [abs-x 0]
               #:abs-y [abs-y 0]
               #:compose [compose (halign->vcompose (align->h align))]
               #:sep [sep 0]
               #:internal:skip [skip #f])
  (define halign (align->h align))
  (define valign (align->v align))
  (refpoint* xfrac yfrac abs-x abs-y halign valign compose sep #f))

(define (refpoint* xfrac yfrac dxabs dyabs
                   halign valign compose sep continued?)
  (placer
   (lambda (scene picts)
     (define scene-w (pict-width scene))
     (define scene-h (pict-height scene))
     (define dx (+ (* scene-w xfrac) dxabs))
     (define dy (+ (* scene-h yfrac) dyabs))
     (define-values (newpict newsep)
       (apply-compose compose sep (cons (and continued? (blank 0)) picts)))
     (define newpict-w (pict-width newpict))
     (define newpict-h (pict-height newpict))
     (define newscene
       (let ([localrefx (* newpict-w (align->frac halign))]
             [localrefy (* newpict-h (align->frac valign))])
         (lt-superimpose scene (inset newpict (- dx localrefx) (- dy localrefy) 0 0))))
     (let ([result-pict (refocus newscene scene)])
       (cond [(and (eq? valign 't) (eq? compose (halign->vcompose halign)))
              ;; ie, going top-down and compose is the natural compose for this align
              (mk-ppict result-pict
                        (refpoint* 0 0 dx (+ dy newpict-h)
                                   halign valign compose newsep #t))]
             [(and (eq? halign 'l) (eq? compose (valign->hcompose valign)))
              ;; ie, going left-right and compose is the natural compose ...
              (mk-ppict result-pict
                        (refpoint* 0 0 (+ dx newpict-w) dy
                                   halign valign compose newsep #t))]
             [else result-pict])))))

;; ----

;; cascade : -> placer
(define (cascade [step-x0 'auto] [step-y0 'auto])
  ;; Auto cascade by largest bounding box.
  ;; FIXME: add align arg, determines position of each pict w/in bbox
  (placer
   (lambda (scene elems)
     (for/or ([e (in-list elems)])
       (when (real? e)
         (error 'cascade "spacing changes not allowed: ~e" e)))
     (let* ([picts (filter pict? elems)]
            [max-w (apply max 1 (map pict-width picts))]  ;; avoid 0
            [max-h (apply max 1 (map pict-height picts))] ;; avoid 0
            [auto-step-x (/ (- (pict-width scene) max-w) (+ 1 (length picts)))]
            [auto-step-y (/ (- (pict-height scene) max-h) (+ 1 (length picts)))]
            [step-x (if (eq? step-x0 'auto) auto-step-x step-x0)]
            [step-y (if (eq? step-y0 'auto) auto-step-y step-y0)]
            [bbox (blank max-w max-h)]
            [positioned-picts
             (for/list ([pict (in-list picts)]
                        [i (in-naturals 1)])
               (inset (cc-superimpose bbox pict)
                      (* i step-x) (* i step-y) 0 0))]
            [newscene
             (lt-superimpose scene
                             (apply lt-superimpose positioned-picts))]
            [result-pict (refocus newscene scene)])
       ;; Can't continue a cascade, since depends on number of picts.
       ;; FIXME: If step is given rather than computed, then we can.
       result-pict))))

;; ----

;; apply-compose : compose real (listof (U #f pict real)) -> (values pict real)
;; Returns composed pict and last given separator num in elems (or init-sep, if none)
(define (apply-compose compose init-sep elems)
  (define (start-loop sep elems)
    (cond [(and (pair? elems) (real? (car elems)))
           (start-loop (car elems) (cdr elems))]
          [(and (pair? elems) (pict? (car elems)))
           (join-loop (car elems) sep (cdr elems))]
          [(null? elems)
           (blank 0)]))
  (define (join-loop base sep elems)
    (cond [(and (pair? elems) (real? (car elems)))
           (join-loop base (car elems) (cdr elems))]
          [(and (pair? elems) (pict? (car elems)))
           (join-loop (compose sep base (car elems))
                      sep
                      (cdr elems))]
          [(null? elems) base]))
  (values (start-loop init-sep (filter values elems))
          (last (cons init-sep (filter real? elems)))))

;; ----

(define (align->frac align)
  (case align
    ((t l)   0)
    ((c)   1/2)
    ((b r)   1)))

(define (align->h align)
  (case align
    ((lt lc lb) 'l)
    ((ct cc cb) 'c)
    ((rt rc rb) 'r)))

(define (align->v align)
  (case align
    ((lt ct rt) 't)
    ((lc cc rc) 'c)
    ((lb cb rb) 'r)))

(define (halign->vcompose halign)
  (case halign
    ((l) vl-append)
    ((c) vc-append)
    ((r) vr-append)))

(define (valign->hcompose align)
  (case align
    ((t) ht-append)
    ((c) hc-append)
    ((b) hb-append)))

;; ----

#|
(define (ppict-do* base elems)
  (define (loop elems rchunks)
    (cond [(and (pair? elems) (placer? (car elems)))
           (loop (cdr elems) (cons (car elems) rchunks))]
          [(and (pair? elems))
           (loop* (cdr elems) rchunks (list (car elems)))]
          [(null? elems)
           (reverse rchunks)]))
  (define (loop* elems rchunks rchunk)
    (cond [(and (pair? elems) (placer? (car elems)))
           (loop elems (cons (reverse rchunk) rchunks))]
          [(and (pair? elems))
           (loop* (cdr elems) rchunks (cons (car elems) rchunk))]
          [(null? elems)
           (loop elems (cons (reverse rchunk) rchunks))]))
  (let ([chunks (loop elems null)])
    (for/fold ([acc base]) ([chunk (in-list chunks)])
      (cond [(placer? chunk)
             (ppict-go acc chunk)]
            [(list? chunk)
             (apply ppict-add acc chunk)]))))
|#

;; ----

(struct p:elem (value))
(struct p:out ())
(struct p:go (placer))

;; internal-ppict-do : pict (listof (U p:go p:out p:elem)) -> (values pict (listof pict))
(define (internal-ppict-do who base parts)
  (let* ([init-go
          (cond [(ppict? base) (p:go (ppict-placer base))]
                [else #f])]
         [gochunks (get-gochunks who init-go parts)])
    (do-gochunks base gochunks)))

;; ----

;; A gochunk is (cons p:go (listof (U p:elem p:next)))

;; get-gochunks : (U p:go #f) (listof (U p:elem p:out p:go)) -> (listof gochunk)
(define (get-gochunks who init-go elems)
  (define (loop init-go elems)
    (cond [(and (pair? elems) (p:go? (car elems)))
           (loop (car elems) (cdr elems))]
          [(pair? elems)
           (unless init-go
             (error who "missing initial placer"))
           (let-values ([(chunk tail) (split-until p:go? elems)])
             (cons (cons init-go chunk)
                   (if (pair? tail)
                       (loop (car tail) (cdr tail))
                       null)))]
          [(null? elems) null]))
  (loop init-go elems))

;; do-gochunks : pict (listof gochunk) -> (values pict (listof pict))
(define (do-gochunks base gochunks)
  (let-values ([(pict rpictss)
                (for/fold ([base base] [rpictss null]) ([gochunk (in-list gochunks)])
                  (let* ([placer (p:go-placer (car gochunk))]
                         [chunk (cdr gochunk)]
                         [base (ppict-go base placer)])
                    (let-values ([(pict picts)
                                  (do-chunk base chunk)])
                      (values pict (cons picts rpictss)))))])
    (values pict (apply append (reverse rpictss)))))

;; A chunk is (listof (U p:elem p:out))

;; do-chunk : ppict (listof (U p:elem p:out)) -> (values ppict (listof pict))
;; In second return value, one pict per p:out occurrence.
;; FIXME: avoid applying ghost to previously ghosted pict?
(define (do-chunk base chunk)
  (let ([elem-chunks
         ;; (listof (listof pict?))
         ;;   length is N+1, where N is number of (p:out) in chunk
         ;;   ghosted before visible
         (let elab ([chunk chunk])
           (cond [(and (pair? chunk) (p:out? (car chunk)))
                  (let ([elab-rest (elab (cdr chunk))])
                    (cons (map ghost* (car elab-rest))
                          elab-rest))]
                 [(and (pair? chunk) (p:elem? (car chunk)))
                  (for/list ([elem-chunk (in-list (elab (cdr chunk)))])
                    (cons (p:elem-value (car chunk))
                          elem-chunk))]
                 [(null? chunk)
                  (list null)]))])
    (let out-loop ([chunks elem-chunks] [rpicts null])
      (cond [(null? (cdr chunks))
             (values (apply ppict-add base (car chunks))
                     (reverse rpicts))]
            [else
             (out-loop (cdr chunks)
                       (cons (apply ppict-add base (car chunks))
                             rpicts))]))))

;; ----

(define (split-until stop? elems)
  (let loop ([elems elems] [rprefix null])
    (cond [(and (pair? elems) (stop? (car elems)))
           (values (reverse rprefix) elems)]
          [(pair? elems)
           (loop (cdr elems) (cons (car elems) rprefix))]
          [(null? elems)
           (values (reverse rprefix) null)])))

(define (ghost* x)
  (if (pict? x) (ghost x) x))

;; ============================================================
;; Exports

(define align/c
  (or/c 'lt 'ct 'rt
        'lc 'cc 'rc
        'lb 'cb 'rb))

(provide (all-defined-out))
