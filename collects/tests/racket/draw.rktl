(load-relative "loadtest.rktl")

(Section 'draw)
(require racket/draw)

;; ----------------------------------------

(let ([color (new color%)])
  (test #f (λ (c) (send c is-immutable?)) color)
  (test 0 (λ (c) (send c red)) color)
  (test 0 (λ (c) (send c green)) color)
  (test 0 (λ (c) (send c blue)) color)
  (test 1.0 (λ (c) (send c alpha)) color))

(let ([color (make-immutable-color 101 102 103 0.9)])
  (test #t (λ (c) (send c is-immutable?)) color)
  (test 101 (λ (c) (send c red)) color)
  (test 102 (λ (c) (send c green)) color)
  (test 103 (λ (c) (send c blue)) color)
  (test 0.9 (λ (c) (send c alpha)) color))

(let ([color (make-immutable-color)])
  (test #t (λ (c) (send c is-immutable?)) color)
  (test 0 (λ (c) (send c red)) color)
  (test 0 (λ (c) (send c green)) color)
  (test 0 (λ (c) (send c blue)) color)
  (test 1.0 (λ (c) (send c alpha)) color))

;; ----------------------------------------

(let ([brush (new brush%)])
  (test #f (λ (b) (send b is-immutable?)) brush))

(let ([brush (make-immutable-brush)])
  (test #t (λ (b) (send b is-immutable?)) brush))

;; ----------------------------------------

(let ([pen (new pen%)])
  (test #f (λ (p) (send p is-immutable?)) pen))

(let ([pen (make-immutable-pen)])
  (test #t (λ (p) (send p is-immutable?)) pen))