#lang racket/base
(require racket/class
         "syntax.rkt"
         "color.rkt")

(provide linear-gradient%
         radial-gradient%
         make-linear-gradient
         make-radial-gradient)

;(define gradient<%> 
;  (interface ()
;    get-stops))

(define (check-reals name lst)
  (for ([x lst])
   (unless (real? x)
     (raise-type-error (init-name name) "Coordinate must be a real? ~a" x))))

(define (check-radius name lst)
  (for ([x lst])
   (unless (and (real? x) (not (negative? x)))
     (raise-type-error (init-name name) "Radius must be a real? ~a" x))))

(define (check-stops name stops)
  (unless (list? stops) (error name "Stops must be a list ~a" stops))
  (for ([x stops])
    (unless (list? x) (error name "A stop must be a list ~a" x))
    (unless (= (length x) 2) (error  name "A stop must be a element list ~a" x))
    (unless (and (real? (car x)) (<= 0.0 (car x) 1.0)) 
      (error  name "First element of a stop must be a real between 0.0 and 1.0 ~a" (car x)))
    (unless (is-a? (cadr x) color%) 
      (error  name "Second element of a stop must be a color% ~a" (cdr x)))))
    
;(define linear-gradient% (class* object% (gradient<%>)
(define linear-gradient% (class object%
  (init-field [x0 0]
              [y0 0]
              [x1 0]
              [y1 0]
              [stops null])

  (check-reals 'linear-gradient% (list x0 y0 x1 y1))
  (check-stops 'linear-gradient% stops)

  (super-new)

  (define/public (get-line) (values x0 y0 x1 y1))
  (define/public (get-stops) stops)))

;(define radial-gradient% (class* object% (gradient<%>)
(define radial-gradient% (class object%
  (init-field [x0 0]
              [y0 0]
              [r0 0]
              [x1 0]
              [y1 0]
              [r1 0]
              [stops null])

  (check-reals  'radial-gradient% (list x0 y0 x1 y1))
  (check-radius 'radial-gradient% (list r0 r1))
  (check-stops  'radial-gradient% stops)

  (super-new)

  (define/public (get-circles) (values x0 y0 r0 x1 y1 r1))
  (define/public (get-stops) stops)))

(define (make-linear-gradient x0 y0 x1 y1 stopslst) 
  (make-object linear-gradient% x0 y0 x1 y1 stopslst))

(define (make-radial-gradient x0 y0 r0 x1 y1 r1 stopslst)
  (make-object radial-gradient% x0 y0 r0 x1 y1 r1 stopslst))

