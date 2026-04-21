#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type point_t (struct_t
                            [x int_t]
                            [y int_t]))

(define-ffi2-type dimen_t (struct_t
                            [width double_t]
                            [height double_t]))

(define-ffi2-type rect_t (struct_t
                           [topleft point_t]
                           [size dimen_t]))

(define-ffi2-type rect_shape_t (struct_t
                                 ;; no tag of `struct`s means that any pointer is assumed ok
                                 [topleft (struct_t [x int_t] [y int_t])]
                                 [size (struct_t [width double_t] [height double_t])]))

(define-ffi2-type picky_rect_shape_t (struct_t
                                       [topleft (struct_t a_point_t [x int_t] [y int_t])]
                                       [size (struct_t a_dimen_t [width double_t] [height double_t])]))
(define-ffi2-type a_point_t* void_t*)
(define-ffi2-type a_dimen_t* void_t*)

;; `ffi2-sizeof` and `ffi2-offsetof` generally depend on the
;; platform, but `int_t` and `double_t` size and alignment are
;; the same everywhere (currently)

(check-equal? (ffi2-sizeof point_t) 8)
(check-equal? (ffi2-sizeof dimen_t) 16)
(check-true ((+ (ffi2-sizeof point_t) (ffi2-sizeof dimen_t))
             . <= .
             (ffi2-sizeof rect_t)))
(check-equal? (ffi2-sizeof rect_t) (ffi2-sizeof rect_shape_t))

(check-equal? (ffi2-offsetof point_t x) 0)
(check-equal? (ffi2-offsetof point_t y) 4)
(check-equal? (ffi2-offsetof dimen_t width) 0)
(check-equal? (ffi2-offsetof dimen_t height) 8)
(check-true ((ffi2-offsetof rect_t size) . >= . (ffi2-sizeof point_t)))

(let ()
  (define pt (ffi2-malloc #:manual point_t))
  (check-true (void_t*? pt))
  (check-false (void_t*/gcable? pt))
  (check-true (point_t*? pt))
  (ffi2-free pt))

(define pt (ffi2-malloc point_t))
(check-true (void_t*? pt))
(check-true (void_t*/gcable? pt))
(check-true (point_t*? pt))
(check-equal? (set-point_t-x! pt 10) (void))
(check-equal? (set-point_t-y! pt 11) (void))
(check-equal? (point_t-x pt) 10)
(check-equal? (point_t-y pt) 11)
(check-exn exn:fail:contract? (lambda () (set-point_t-x! pt 0.0)))

(check-equal? (point_t-x ((black-box point_t) 1 2)) 1)

(let ()
  (define pts (ffi2-malloc point_t 2))
  (check-equal? (point_t*-ref pts 0) pts)
  (check-true (point_t*? (point_t*-ref pts 0)))
  (check-equal? (point_t*-ref pts 1) (ffi2-add pts point_t 1))
  (check-true (point_t*? (point_t*-ref pts 1))))

(let ()
  (define p (ffi2-malloc 1))
  (check-exn exn:fail:contract? (lambda () (point_t-x p)))
  (check-exn exn:fail:contract? (lambda () (set-point_t-x! p 0))))

(let ()
  (define pt1 (point_t 99 100))
  (check-equal? (point_t-x pt1) 99)
  (check-equal? (point_t-y pt1) 100))

(let ()
  (define r (ffi2-malloc rect_t))
  (ffi2-memset r 255 (ffi2-sizeof rect_t))
  (check-equal? (point_t-x (rect_t-topleft r)) -1)
  (check-equal? (point_t-y (rect_t-topleft r)) -1)

  (ffi2-set! r int_t 1 22)
  (check-equal? (point_t-x (rect_t-topleft r)) -1)
  (check-equal? (point_t-y (rect_t-topleft r)) 22)

  (check-equal? (ffi2-ref r int_t 0) -1)
  (check-equal? (ffi2-ref r int_t 1) 22)
  (check-equal? (ffi2-ref r int_t 4 #:bytes) 22)

  (set-point_t-y! (rect_t-topleft r) 77)
  (check-equal? (point_t-x (rect_t-topleft r)) -1)
  (check-equal? (point_t-y (rect_t-topleft r)) 77)

  (set-rect_t-topleft! r pt)
  (check-equal? (point_t-x (rect_t-topleft r)) 10)
  (check-equal? (point_t-y (rect_t-topleft r)) 11)

  (set-rect_t-size! r (dimen_t 101.0 102.0))
  (check-equal? (point_t-x (rect_t-topleft r)) 10)
  (check-equal? (point_t-y (rect_t-topleft r)) 11)
  (check-equal? (dimen_t-width (rect_t-size r)) 101.0)
  (check-equal? (dimen_t-height (rect_t-size r)) 102.0)

  (void))

(let ()
  (define r (rect_t (point_t 0 1) (dimen_t 3.0 4.0)))
  (check-equal? (point_t-x (rect_t-topleft r)) 0)
  (check-equal? (point_t-y (rect_t-topleft r)) 1)
  (check-equal? (dimen_t-width (rect_t-size r)) 3.0)
  (check-equal? (dimen_t-height (rect_t-size r)) 4.0)
  (void))

(let ()
  (define r (rect_shape_t (point_t 0 1) (dimen_t 3.0 4.0)))
  (check-exn exn:fail:contract? (lambda () (point_t-x (rect_shape_t-topleft r)) 0))
  (check-equal? (point_t-x (ffi2-cast (rect_shape_t-topleft r) #:to point_t*)) 0)
  (void))

(let ()
  (check-exn exn:fail:contract? (lambda () (picky_rect_shape_t (point_t 0 1) (dimen_t 3.0 4.0))))
  (define r (picky_rect_shape_t (ffi2-cast (point_t 0 1) #:to a_point_t*)
                                (ffi2-cast (dimen_t 3.0 4.0) #:to a_dimen_t*)))
  (check-exn exn:fail:contract? (lambda () (point_t-x (picky_rect_shape_t-topleft r)) 0))
  (check-equal? (point_t-x (ffi2-cast (picky_rect_shape_t-topleft r) #:to point_t*)) 0)
  (void))
