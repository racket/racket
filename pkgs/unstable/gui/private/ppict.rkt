#lang racket/base
(require (for-syntax racket/base)
         racket/list
         racket/class
         racket/stxparam
         racket/contract/base
         pict
         "tag-pict.rkt")

#|
TODO
- [lcr]bl alignments... not sure about [lcr]tl
- document composer contract
- generalize ppict-add to ppict-add* (put 'next support there)
- find a way to support slide animation
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

(define (mk-ppict p placer)
  (ppict (pict-draw p)
         (pict-width p)
         (pict-height p)
         (pict-ascent p)
         (pict-descent p)
         (list (make-child p 0 0 1 1 0 0))
         #f
         (pict-last p)
         placer))

(define (ppict-pict dp)
  (child-pict (car (pict-children dp))))

;; ----

(define-syntax-parameter ppict-do-state
  (lambda (stx)
    (raise-syntax-error #f "used out of context" stx)))

;; ppict-go : pict placer -> ppict
(define (ppict-go dp pl)
  (cond [(ppict? dp)
         (mk-ppict (ppict-pict dp) pl)]
        [(pict? dp)
         (mk-ppict dp pl)]))

;; ppict-add : ppict (U pict real #f 'next) ... -> ppict
(define (ppict-add dp . parts)
  (let-values ([(final intermediates)
                (ppict-add/internal 'ppict-add dp parts)])
    final))

;; ppict-add* : ppict (U pict real #f 'next) ... -> (values ppict (listof pict))
(define (ppict-add* dp . parts)
  (ppict-add/internal 'ppict-add* dp parts))

;; ppict-add/internal : symbol pict (listof (U pict real #f 'next))
;;                   -> (values pict (listof pict)
;; In second return value, one pict per 'next occurrence.
;; FIXME: avoid applying ghost to previously ghosted pict?
(define (ppict-add/internal who base parts)
  (cond [(for/and ([part (in-list parts)]) (eq? part 'next))
         ;; Special case; don't need a ppict
         (values base (make-list (length parts) base))]
        [else
         (unless (ppict? base) (error who "missing placer"))
         (let ([placer (ppict-placer base)]
               [base-pict (ppict-pict base)]
               [elem-chunks
                ;; (listof (listof pict?))
                ;;   length is N+1, where N is number of 'next in chunk
                ;;   ghosted before visible
                (let elab ([chunk parts])
                  (cond [(and (pair? chunk) (eq? 'next (car chunk)))
                         (let ([elab-rest (elab (cdr chunk))])
                           (cons (map ghost* (car elab-rest)) elab-rest))]
                        [(and (pair? chunk) (not (eq? 'next (car chunk))))
                         (for/list ([elem-chunk (in-list (elab (cdr chunk)))])
                           (cons (car chunk) elem-chunk))]
                        [(null? chunk) (list null)]))])
           (let out-loop ([chunks elem-chunks] [rpicts null])
             (cond [(null? (cdr chunks))
                    (values (send placer place base-pict (car chunks))
                            (reverse rpicts))]
                   [else
                    (out-loop (cdr chunks)
                              (cons (send placer place base-pict (car chunks))
                                    rpicts))])))]))

;; ----

(define (placer? x) (is-a? x placer<%>))
(define (refpoint-placer? x) (is-a? x refpoint%))

(define (merge-refpoints x y)
  (send x take-y-from y))

(define placer<%>
  (interface ()
    ;; place : pict (listof (U pict real #f)) -> pict
    place))

(define refpoint%
  (class* object% (placer<%>)
    (init-field xa ya depxy halign valign compose
                [sep 0]
                [cont? #f])
    (super-new)

    (define/public (place scene picts)
      (define-values (dx dy)
        (let-values ([(depx depy) (if depxy (depxy scene) (values 0 0))])
          (values (+ depx xa)
                  (+ depy ya))))
      (define-values (newpict newsep)
        (apply-compose compose sep (cons (and cont? (blank 0)) picts)))
      (define newscene
        (pin-over/align scene dx dy halign valign newpict))
      (cond [(and (eq? valign 't) (eq? compose (halign->vcompose halign)))
             ;; ie, going top-down and compose is the natural compose for this align
             (mk-ppict newscene
                       (new refpoint%
                            (xa dx) (ya (+ dy (pict-height newpict))) (depxy #f)
                            (halign halign) (valign valign)
                            (compose compose) (sep newsep) (cont? #t)))]
            [(and (eq? halign 'l) (eq? compose (valign->hcompose valign)))
             ;; ie, going left-right and compose is the natural compose ...
             (mk-ppict newscene
                       (new refpoint%
                            (xa (+ dx (pict-width newpict))) (ya dy) (depxy #f)
                            (halign halign) (valign valign)
                            (compose compose) (sep newsep) (cont? #t)))]
            [else newscene]))

    (define/public (take-y-from other)
      (new refpoint%
           (xa xa)
           (ya (get-field ya other))
           (depxy (let ([odepxy (get-field depxy other)])
                    (lambda (base)
                      (let-values ([(x _y) (if depxy (depxy base) (values 0 0))]
                                   [(_x y) (if odepxy (odepxy base) (values 0 0))])
                        (values x y)))))
           (halign halign)
           (valign valign)
           (compose compose)
           (sep sep)
           (cont? cont?)))))

;; --

(define (grid cols rows col row [align 'cc]
              #:abs-x [abs-x 0]
              #:abs-y [abs-y 0]
              #:compose [compose (halign->vcompose (align->h align))])
  ;; row, column indexes are 1-based
  (define halign (align->h align))
  (define valign (align->v align))
  (define xfrac (/ (+ (sub1 col) (align->frac halign)) cols))
  (define yfrac (/ (+ (sub1 row) (align->frac valign)) rows))
  (new refpoint%
       (xa abs-x) (ya abs-y)
       (depxy (lambda (p)
                (values (* xfrac (pict-width p))
                        (* yfrac (pict-height p)))))
       (halign halign) (valign valign) (compose compose)))

(define (coord xfrac yfrac [align 'cc]
               #:abs-x [abs-x 0]
               #:abs-y [abs-y 0]
               #:compose [compose (halign->vcompose (align->h align))])
  (define halign (align->h align))
  (define valign (align->v align))
  (new refpoint%
       (xa abs-x) (ya abs-y)
       (depxy (lambda (p)
                (values (* xfrac (pict-width p))
                        (* yfrac (pict-height p)))))
       (halign halign) (valign valign) (compose compose)))

;; ----

(define cascade%
  (class* object% (placer<%>)
    (init-field step-x0 step-y0)
    (super-new)

    (define/public (place scene elems)
      (for ([e (in-list elems)])
        (when (real? e) (error 'cascade "spacing changes not allowed: ~e" e)))
      (let* ([picts (filter pict? elems)]
             [max-w (apply max 1 (map pict-width picts))]  ;; avoid 0
             [max-h (apply max 1 (map pict-height picts))] ;; avoid 0
             [auto-step-x (/ (- (pict-width scene) max-w) (+ 1 (length picts)))]
             [auto-step-y (/ (- (pict-height scene) max-h) (+ 1 (length picts)))]
             [step-x (if (eq? step-x0 'auto) auto-step-x step-x0)]
             [step-y (if (eq? step-y0 'auto) auto-step-y step-y0)]
             [bbox (blank max-w max-h)]
             [newscene
              (for/fold ([scene scene])
                  ([pict (in-list picts)]
                   [i (in-naturals 1)])
                (pin-over scene (* i step-x) (* i step-y) (cc-superimpose bbox pict)))])
        ;; Can't continue a cascade, since depends on number of picts.
        ;; FIXME: If step is given rather than computed, then we can.
        newscene))))

;; cascade : ... -> placer
(define (cascade [step-x0 'auto] [step-y0 'auto])
  ;; Auto cascade by largest bounding box.
  ;; FIXME: add align arg, determines position of each pict w/in bbox
  (new cascade% (step-x0 step-x0) (step-y0 step-y0)))

(define tile%
  (class* object% (placer<%>)
    (init-field cols rows
                [start-at 0])
    (super-new)

    (define/public (place scene elems)
      (for ([e (in-list elems)])
        (when (real? e) (error 'tile "spacing changes not allowed: ~e" e)))
      (let* ([picts (filter pict? elems)]
             [scene-w (pict-width scene)]
             [scene-h (pict-height scene)]
             [dx (/ scene-w cols)]
             [dy (/ scene-h rows)]
             [newscene
              (for/fold ([scene scene])
                  ([pict (in-list picts)]
                   [i (in-naturals start-at)])
                (let ([r (quotient i cols)]
                      [c (remainder i cols)])
                  (pin-over/align scene
                                  (+ (/ dx 2) (* c dx))
                                  (+ (/ dy 2) (* r dy))
                                  'c 'c pict)))])
        (mk-ppict newscene
                  (new tile%
                       (cols cols)
                       (rows rows)
                       (start-at (+ start-at (length picts)))))))))

(define (tile cols rows)
  (new tile% (cols cols) (rows rows)))

;; at-find-pict : ... -> placer
(define (at-find-pict path
                      [find cc-find]
                      [align 'cc]
                      #:abs-x [abs-x 0]
                      #:abs-y [abs-y 0]
                      #:compose [compose (halign->vcompose (align->h align))])
  (define halign (align->h align))
  (define valign (align->v align))
  (new refpoint%
       (xa abs-x) (ya abs-y)
       (depxy (lambda (p)
                (let ([pict-path (if (tag-path? path) (find-tag p path) path)])
                  (unless pict-path
                    (error 'at-find-path "failed finding ~e" path))
                  (find p pict-path))))
       (halign halign) (valign valign) (compose compose)))

(define (pin-over/align scene x y halign valign pict)
  (let ([localrefx (* (pict-width pict) (align->frac halign))]
        [localrefy (* (pict-height pict) (align->frac valign))])
    (pin-over scene (- x localrefx) (- y localrefy) pict)))

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

(define (ghost* x)
  (if (pict? x) (ghost x) x))

;; ============================================================
;; Exports

(define align/c
  (or/c 'lt 'ct 'rt
        'lc 'cc 'rc
        'lb 'cb 'rb))

(provide (all-defined-out))
