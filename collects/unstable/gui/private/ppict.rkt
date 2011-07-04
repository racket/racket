#lang racket/base
(require (for-syntax racket/base)
         racket/list
         racket/class
         racket/stxparam
         racket/contract
         slideshow/pict)

#|
TODO
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

;; ppict-add : ppict (U pict real #f) ... -> ppict
(define (ppict-add dp . picts)
  (let ([pl (ppict-placer dp)])
    (send pl place (ppict-pict dp) picts)))

;; ppict-go : pict placer -> ppict
(define (ppict-go dp pl)
  (cond [(ppict? dp)
         (mk-ppict (ppict-pict dp) pl)]
        [(pict? dp)
         (mk-ppict dp pl)]))

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
      (define newpict-w (pict-width newpict))
      (define newpict-h (pict-height newpict))
      (define newscene
        (let ([localrefx (* newpict-w (align->frac halign))]
              [localrefy (* newpict-h (align->frac valign))])
          (pin-over scene (- dx localrefx) (- dy localrefy) newpict)))
      (cond [(and (eq? valign 't) (eq? compose (halign->vcompose halign)))
             ;; ie, going top-down and compose is the natural compose for this align
             (mk-ppict newscene
                       (new refpoint%
                            (xa dx) (ya (+ dy newpict-h)) (depxy #f)
                            (halign halign) (valign valign)
                            (compose compose) (sep newsep) (cont? #t)))]
            [(and (eq? halign 'l) (eq? compose (valign->hcompose valign)))
             ;; ie, going left-right and compose is the natural compose ...
             (mk-ppict newscene
                       (new refpoint%
                            (xa (+ dx newpict-w)) (ya dy) (depxy #f)
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

(define-syntax-parameter ppict-do-state
  (lambda (stx)
    (raise-syntax-error #f "used out of context" stx)))

;; internal-ppict-do : pict (listof (U pict real #f 'next))
;;                  -> (values pict (listof pict))
(define (internal-ppict-do who base parts)
  (unless (ppict? base)
    (error who "missing placer"))
  (do-chunk base parts))

;; ----

;; A chunk is (listof (U pict real #f 'next))

;; do-chunk : ppict (listof (U pict real #f 'next)) -> (values ppict (listof pict))
;; In second return value, one pict per 'next occurrence.
;; FIXME: avoid applying ghost to previously ghosted pict?
(define (do-chunk base chunk)
  (let ([elem-chunks
         ;; (listof (listof pict?))
         ;;   length is N+1, where N is number of 'next in chunk
         ;;   ghosted before visible
         (let elab ([chunk chunk])
           (cond [(and (pair? chunk) (eq? 'next (car chunk)))
                  (let ([elab-rest (elab (cdr chunk))])
                    (cons (map ghost* (car elab-rest)) elab-rest))]
                 [(and (pair? chunk) (not (eq? 'next (car chunk))))
                  (for/list ([elem-chunk (in-list (elab (cdr chunk)))])
                    (cons (car chunk) elem-chunk))]
                 [(null? chunk) (list null)]))])
    (let out-loop ([chunks elem-chunks] [rpicts null])
      (cond [(null? (cdr chunks))
             (values (apply ppict-add base (car chunks))
                     (reverse rpicts))]
            [else
             (out-loop (cdr chunks)
                       (cons (apply ppict-add base (car chunks))
                             rpicts))]))))

(define (ghost* x)
  (if (pict? x) (ghost x) x))

;; ============================================================
;; Tagged picts

(struct tagged-pict pict (tag))
;; tag is symbol

(define (tag-pict p tg)
  (tagged-pict (pict-draw p)
               (pict-width p)
               (pict-height p)
               (pict-ascent p)
               (pict-descent p)
               (list (make-child p 0 0 1 1 0 0))
               #f
               (pict-last p)
               tg))

;; find-tag : pict tag-path -> pict-path
(define (find-tag p tagpath)
  (let ([tagpath (if (symbol? tagpath) (list tagpath) tagpath)])
    (define (loop p tagpath)
      (cond [(pair? tagpath)
             (childrenloop (pict-children p) tagpath)]
            [(null? tagpath)
             (list p)]))
    (define (pairloop p tagpath)
      (or (and (tagged-pict? p)
               (eq? (tagged-pict-tag p) (car tagpath))
               (let ([r (loop p (cdr tagpath))])
                 (and r (cons p r))))
          (childrenloop (pict-children p) tagpath)))
    (define (childrenloop children tagpath)
      (for/or ([c (in-list children)])
        (pairloop (child-pict c) tagpath)))
    (loop p tagpath)))

(define (tag-path? x)
  (or (symbol? x)
      (and (list? x) (pair? x) (andmap symbol? x))))

(define (pict-tag p)
  (and (tagged-pict? p) (tagged-pict-tag p)))

;; ============================================================
;; Exports

(define align/c
  (or/c 'lt 'ct 'rt
        'lc 'cc 'rc
        'lb 'cb 'rb))

(provide (all-defined-out))
