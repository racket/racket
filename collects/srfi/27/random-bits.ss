(module random-bits mzscheme
  
  (require (lib "contract.ss"))
  
  (provide random-real
           default-random-source
           (rename s:make-random-source make-random-source)
           random-source?
           random-source-state-ref
           random-source-state-set!
           random-source-randomize!
           random-source-pseudo-randomize!)
  
  (define positive-integer/c
    (and/c integer? positive? exact?))
  
  (define-struct random-source (generator))
  
  (provide/contract
   (random-integer (-> random-source? any))
   (random-source-make-integers (-> random-source? any))
   (random-source-make-reals (case-> (-> random-source? any)
                                     (-> random-source? (and/c (>/c 0) (</c 1)) any))))
  
  (define (random-integer n)
    (if (<= n 2147483647)
        (random n)
        (+ (* (random-integer (quotient n 1073741824)) 1073741824)
           (random 1073741824))))
  
  (define (random-real)
    (random))
  
  (define-syntax default-random-source
    (syntax-id-rules (set!)
      ((set! default-random-source expr)
       (current-pseudo-random-generator (random-source-generator expr)))
      ((default-random-source expr ...)
       ((make-random-source (current-pseudo-random-generator)) expr ...))
      (default-random-source
        (make-random-source (current-pseudo-random-generator)))))
  
  (define (s:make-random-source)
    (let ((old (current-pseudo-random-generator))
          (new (make-pseudo-random-generator)))
      (current-pseudo-random-generator new)
      (random-seed 0)
      (begin0 (make-random-source new)
              (current-pseudo-random-generator old))))
  
  (define (random-source-state-ref s)
    (pseudo-random-generator->vector (random-source-generator s)))
  
  (define-syntax random-source-state-set!
    (syntax-rules (default-random-source)
      ((_ default-random-source state)
       (current-pseudo-random-generator (vector->pseudo-random-generator state)))
      ((_ s state)
       (set-random-source-generator! s
                                     (vector->pseudo-random-generator state)))))
  
  (define-syntax random-source-randomize!
    (syntax-rules (default-random-source)
      ((_ default-random-source)
       (current-pseudo-random-generator (make-pseudo-random-generator)))
      ((_ s)
       (set-random-source-generator! s
                                     (make-pseudo-random-generator)))))
  
  (define-syntax random-source-pseudo-randomize!
    (syntax-rules (default-random-source)
      ((_ default-random-source ij ...)
       (random-seed (modulo (equal-hash-code (list ij ...)) 2147483648)))
      ((_ s ij ...)
       (parameterize ((current-pseudo-random-generator
                       (random-source-generator s)))
         (random-seed (modulo (equal-hash-code (list ij ...)) 2147483648))))))
  
  (define (random-source-make-integers s)
    (lambda (n)
      (parameterize ((current-pseudo-random-generator
                      (random-source-generator s)))
        (random-integer n))))
  
  (define random-source-make-reals
    (case-lambda
      ((s)
       (lambda ()
         (parameterize ((current-pseudo-random-generator
                         (random-source-generator s)))
           (random))))
      ((s unit)
       (let ((n (inexact->exact (floor (/ unit)))))
         (lambda ()
           (parameterize ((current-pseudo-random-generator
                           (random-source-generator s)))
             (* (add1 (random n)) unit)))))))
  
  (random-seed 0))
