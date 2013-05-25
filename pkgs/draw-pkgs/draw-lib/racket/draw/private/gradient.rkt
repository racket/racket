#lang racket/base
(require racket/class
         "syntax.rkt"
         "color.rkt")

(provide linear-gradient%
         radial-gradient%)

(define (check-reals name lst)
  (for ([x (in-list lst)])
    (unless (real? x)
      (raise-type-error (init-name name) "real number" x))))

(define (check-radius name lst)
  (for ([x (in-list lst)])
    (unless (and (real? x) (not (negative? x)))
      (raise-type-error (init-name name) "non-negative real number" x))))

(define (check-stops name stops)
  (unless (and (list? stops)
               (for/and ([x (in-list stops)])
                 (and (list? x)
                      (= (length x) 2)
                      (real? (car x))
                      (<= 0.0 (car x) 1.0)
                      (is-a? (cadr x) color%))))
    (raise-type-error (init-name name) 
                      "list of (list x c) where x is a real in [0,1] and c is a color%"
                      stops)))
    
(define linear-gradient% 
  (class object%
    (init x0 y0 x1 y1 stops)
    (define _x0 x0)
    (define _y0 y0)
    (define _x1 x1)
    (define _y1 y1)
    (define _stops stops)

    (check-reals 'linear-gradient% (list x0 y0 x1 y1))
    (check-stops 'linear-gradient% stops)

    (super-new)

    (define/public (get-line) (values _x0 _y0 _x1 _y1))
    (define/public (get-stops) _stops)))

(define radial-gradient% 
  (class object%
    (init x0 y0 r0 x1 y1 r1 stops)
    (define _x0 x0)
    (define _y0 y0)
    (define _r0 r0)
    (define _x1 x1)
    (define _y1 y1)
    (define _r1 r1)
    (define _stops stops)

    (check-reals  'radial-gradient% (list _x0 _y0 _x1 _y1))
    (check-radius 'radial-gradient% (list _r0 _r1))
    (check-stops  'radial-gradient% stops)

    (super-new)

    (define/public (get-circles) (values _x0 _y0 _r0 _x1 _y1 _r1))
    (define/public (get-stops) _stops)))
