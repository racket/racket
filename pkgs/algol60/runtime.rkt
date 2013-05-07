(module runtime mzscheme
  (require racket/undefined)
  
  (provide (struct a60:array (vec dimens))
           (struct a60:switch (choices))
           undefined
           check-boolean
           goto
           get-value
           set-target!
           make-array
           array-ref
           array-set!
           make-switch
           switch-ref
           coerce)
           
  (define-struct a60:array (vec dimens))
  (define-struct a60:switch (choices))
  
  (define (check-boolean b) b)
  (define (goto f) (f))
  (define (get-value v) (v))
  (define (set-target! t name v)
    (unless (procedure-arity-includes? t 1)
      (error 'assignment "formal-argument variable ~a is assigned, but actual argument was not assignable"
             name))
    (t v))
  
  (define (bad what v)
    (error '|bad value| "expected a ~a, got ~e" what v))
  (define (coerce type v)
    (cond
      [(eq? type 'integer) 
       (if (number? v)
           (inexact->exact (floor v))
           (bad 'number v))]
      [(eq? type 'real) 
       (if (number? v)
           (exact->inexact v)
           (bad 'number v))]
      [(eq? type 'boolean) 
       (if (boolean? v)
           v
           (bad 'boolean v))]
      [else v]))
  
  (define (make-array . dimens)
    (make-a60:array
     ((let loop ([dimens dimens])
        (if (null? dimens)
            (lambda () undefined)
            (let ([start (car dimens)]
                  [end (cadr dimens)])
              (let ([build (loop (cddr dimens))])
                (lambda () 
                  (let ([len (add1 (- end start))])
                    (let ([v (make-vector len)])
                      (let loop ([len len])
                        (unless (zero? len)
                          (vector-set! v (sub1 len) (build))
                          (loop (sub1 len))))
                      v))))))))
     dimens))

  (define (check-array a is who)
    (unless (a60:array? a)
      (error who "not an array: ~e" a))
    (unless (= (length is) (/ (length (a60:array-dimens a)) 2))
      (error who "array dimension ~a doesn't match the number of provided indices ~a"
             (length is) (/ (length (a60:array-dimens a)) 2))))
  
  (define (check-index who dimens indices)
    (unless (and (number? (car indices))
                 (exact? (car indices))
                 (integer? (car indices)))
      (error who "index is not an integer: ~e"
             (car indices)))
    (unless (<= (car dimens) (car indices) (cadr dimens))
      (error who "index ~a out of range ~a:~a"
             (car indices) (car dimens) (cadr dimens))))
  
  (define (array-ref a . indices)
    (check-array a indices 'array-reference)
    (let loop ([v (a60:array-vec a)][indices indices][dimens (a60:array-dimens a)])
      (check-index 'array-reference dimens indices)
      (let ([i (vector-ref v (- (car indices) (car dimens)))])
        (if (null? (cdr indices))
            i
            (loop i (cdr indices) (cddr dimens))))))
  
  (define (array-set! a val . indices)
    (check-array a indices 'array-assignment)
    (let loop ([v (a60:array-vec a)][indices indices][dimens (a60:array-dimens a)])
      (check-index 'array-assignment dimens indices)
      (if (null? (cdr indices))
          (vector-set! v (- (car indices) (car dimens)) val)
          (loop (vector-ref v (- (car indices) (car dimens))) (cdr indices) (cddr dimens)))))
  
  (define (make-switch . choices)
    (make-a60:switch (list->vector choices)))
  (define (switch-ref sw index)
    (unless (and (number? index)
                 (integer? index)
                 (exact? index)
                 (<= 1 index (vector-length (a60:switch-choices sw))))
      (error "bad switch index: " index))
    ((vector-ref (a60:switch-choices sw) (sub1 index)))))
