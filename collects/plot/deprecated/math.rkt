(module math mzscheme

  ; Usefull mathy functions for plots
  
  (require mzlib/list mzlib/etc mzlib/math)
  
  ; using vectors for n-dimensional points
  
  ; vector-magnitude : (vectorof number) -> number
  ; computes the magnituded of the vector by using pythegorean theorem
  (define (vector-magnitude vec)
    (sqrt (foldl (lambda (item total) (+ (sqr item) total)) 0 (vector->list vec))))
  
  ; shortcuts to avoid writing ugly vector-ref code
  (define (vector-x vec)
    (vector-ref vec 0))
  
  (define (vector-y vec)
    (vector-ref vec 1))
  
  (define (vector-z vec)
    (vector-ref vec 2))
          
  ;  make-vec : (number number -> number) (number number -> number) -> (vector -> vector)
  (define (make-vec func1 func2)
    (lambda (point) (vector (func1 (vector-x point) (vector-y point)) (func2 (vector-x point) (vector-y point)))))
  
  ; derivative : (number -> number) [number] -> (number -> number)
  (define derivative
    (opt-lambda (func [h .00000001])
      (lambda (x) (/ (- (func (+ x h)) (func x)) h))))
  
  ; gradient : (number number -> number) [number] -> (vector -> vector)
  (define gradient
    (opt-lambda (func-3d [h .00000001])
      (lambda (point) (vector ((derivative (lambda (x) (func-3d x (vector-y point))) h) (vector-x point))
                              ((derivative (lambda (y) (func-3d (vector-x point) y)) h) (vector-y point))))))
  
  (provide (all-defined)))
