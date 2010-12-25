#lang racket/base
(require racket/gui/base
         racket/class
         racket/contract)

(provide/contract
 [make-bitmap-label (->* (string?
                          (or/c path-string?
                                (is-a?/c bitmap%)))
                         ((is-a?/c font%))
                         (is-a?/c bitmap%))]
 [bitmap-label-maker (-> string?
                         (or/c path-string? (is-a?/c bitmap%))
                         (-> any/c (is-a?/c bitmap%)))])

(define (make-bitmap-label text filename-or-bitmap [font normal-control-font])
  (define outside-margin 2)
  (define-values (img-bitmap img-width img-height)
    (let ([q (if (filename-or-bitmap . is-a? . bitmap%)
                 filename-or-bitmap
                 (make-object bitmap% filename-or-bitmap 'unknown/mask))])
      (if (send q ok?)
          (values q
                  (send q get-width)
                  (send q get-height))
          (let* ([b (make-object bitmap% 1 1)]
                 [bdc (make-object bitmap-dc% b)])
            (send bdc clear)
            (send bdc set-bitmap #f)
            (values b 0 0)))))
  (define-values (width height _1 _2) 
    (let ([tmp-bitmap-dc (make-object bitmap-dc% (make-bitmap 1 1))])
      (send tmp-bitmap-dc get-text-extent text font)))
  (define middle-margin (if (and (zero? img-width)
                                 (zero? img-height))
                            0
                            3))
  (define new-width (inexact->exact
                     (floor
                      (+ outside-margin
                         img-width
                         middle-margin
                         width
                         outside-margin))))
  (define new-height (inexact->exact
                      (floor (+ outside-margin
                                (max img-height height)
                                outside-margin))))
  
  (define new-bitmap (make-screen-bitmap new-width new-height))
  (define bitmap-dc (make-object bitmap-dc% new-bitmap))
  (send bitmap-dc clear)
  
  (send bitmap-dc draw-bitmap
        img-bitmap
        outside-margin
        (- (/ new-height 2) (/ img-height 2))
        'solid
        (send the-color-database find-color "black")
        (send img-bitmap get-loaded-mask))
  
  (send bitmap-dc set-font font)
  (send bitmap-dc draw-text text 
        (+ outside-margin img-width middle-margin)
        (- (/ new-height 2) (/ height 2)))
  
  (send bitmap-dc set-bitmap #f)
  
  new-bitmap)

(define (bitmap-label-maker text filename-or-bitmap)
  (let ([bm (make-bitmap-label text filename-or-bitmap)])
    (lambda (area-container-window)
      bm)))
