#lang racket/base
(require racket/list
         racket/contract
         slideshow/pict)

#|
TODO

- need a way to express "dependent" additions, when need to find a
  pict within a ppict, eg for lines, balloons, etc.

- [lcr]bl alignments... not sure about [lcr]tl

- make ppict-do macro, move functionality from pslide to ppict-do
  eg require #:go for placers, use #:next to return multiple picts
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

;; ppict-do : pict (U pict real #f placer) ... -> pict
(define (ppict-do base . elems)
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

;; row, column indexes are 0-based
(define (grid cols rows col row [align 'cc]
              #:compose [composer (align->vcomposer align)]
              #:sep [sep 0])
  (define halign (align->h align))
  (define valign (align->v align))
  (define refxfrac (/ (+ col (align->frac halign)) cols))
  (define refyfrac (/ (+ row (align->frac valign)) rows))
  (coord refxfrac refyfrac align #:compose composer #:sep sep))

(define (coord refxfrac refyfrac [align 'cc]
               #:compose [composer (align->vcomposer align)]
               #:sep [sep 0]
               #:internal:skip [skip #f])
  (define halign (align->h align))
  (define valign (align->v align))
  (placer
   (lambda (scene picts)
     (define scene-w (pict-width scene))
     (define scene-h (pict-height scene))
     (define refx (* scene-w refxfrac))
     (define refy (* scene-h refyfrac))
     (define-values (newpict newsep)
       (apply-composer composer sep (cons skip picts)))
     (define newpict-w (pict-width newpict))
     (define newpict-h (pict-height newpict))
     (define localrefx (* newpict-w (align->frac halign)))
     (define localrefy (* newpict-h (align->frac valign)))
     (define newscene
       (lt-superimpose scene (inset newpict (- refx localrefx) (- refy localrefy) 0 0)))
     (let ([result-pict (refocus newscene scene)])
       (cond [(and (eq? valign 't) (eq? composer (align->vcomposer align)))
              ;; ie, going top-down and composer is the natural composer for this align
              (mk-ppict result-pict
                        (coord refxfrac refyfrac align
                               #:compose composer
                               #:sep newsep
                               #:internal:skip (blank 0 newpict-h)))]
             [(and (eq? halign 'l) (eq? composer (align->hcomposer align)))
              (mk-ppict result-pict
                        (coord refxfrac refyfrac align
                               #:compose composer
                               #:sep newsep
                               #:internal:skip (blank newpict-w 0)))]
             [else result-pict])))))

;; ----

;; apply-composer : composer real (listof (U #f pict real)) -> (values pict real)
;; Returns composed pict and last given separator num in elems (or init-sep, if none)
(define (apply-composer composer init-sep elems)
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
           (join-loop (composer sep base (car elems))
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

(define (align->vcomposer align)
  (case align
    ((lt lc lb) vl-append)
    ((ct cc cb) vc-append)
    ((rt rc rb) vr-append)))

(define (align->hcomposer align)
  (case align
    ((lt ct rt) ht-append)
    ((lc cc rc) hc-append)
    ((lb cb rb) hb-append)))

;; ==== Some examples ====

#|
(slide
 (let* ([dp (colorize (rectangle 200 200) "gray")]
        [dp (ppict-go dp (grid 2 2 1 0 'ct))]
        [dp (ppict-add dp (circle 20) (circle 20) (circle 20))])
   (vc-append gap-size
              (shframe dp)
              (shframe
               (ppict-add dp (colorize (circle 20) "red"))))))

(slide
 (let* ([dp (colorize (rectangle 200 200) "gray")]
        [dp (ppict-go dp (grid 2 2 0 0 'lb #:compose hbl-append))]
        [dp (ppict-add dp (circle 20) (circle 20) (circle 20))])
   (vc-append gap-size
              (shframe dp)
              (shframe
               (ppict-add dp (colorize (circle 20) "red"))))))
|#

;; ============================================================
;; Exports

(define align/c
  (or/c 'lt 'ct 'rt
        'lc 'cc 'rc
        'lb 'cb 'rb))

(provide ppict?
         placer?)

(provide/contract
 [ppict-do
  (->* (pict?)
       ()
       #:rest (listof (or/c pict? real? #f placer?))
       pict?)]
 [ppict-go
  (-> pict? placer? ppict?)]
 [ppict-add
  (->* (ppict?)
       ()
       #:rest (listof (or/c pict? real? #f))
       pict?)]
 [ppict-placer
  (-> ppict? placer?)]
 [placer
  (-> any/c boolean?)]
 [coord
  (->* (real? real?)
       (align/c
        #:compose procedure?)
       placer?)]
 [grid
  (->* (exact-positive-integer? exact-positive-integer?
        exact-nonnegative-integer? exact-nonnegative-integer?)
       (align/c
        #:compose procedure?)
       placer?)])
