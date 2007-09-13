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
  
  (define integer/c
    (and/c integer? exact? positive?))
  
  (define-struct random-source (generator))
  
  (provide/contract (random-integer
                     (-> integer/c any))
                    (random-source-make-integers
                     (-> random-source? (-> integer/c any)))
                    (random-source-make-reals
                     (case->
                      (-> random-source? any)
                      (-> random-source? (and/c (>/c 0) (</c 1)) any))))
  
  (define (s:make-random-source)
    (let ((old (current-pseudo-random-generator))
          (new (make-pseudo-random-generator)))
      (current-pseudo-random-generator new)
      (random-seed 0)
      (begin0 (make-random-source new)
              (current-pseudo-random-generator old))))
  
  (define my-default-random-source
    (s:make-random-source))
  
  (define-syntax default-random-source
    (syntax-id-rules (set!)
      ((set! default-random-source expr)
       (set-random-source-generator! default-random-source
                                     (random-source-generator expr)))
      ((default-random-source expr ...)
       (my-default-random-source expr ...))
      (default-random-source
        my-default-random-source)))
  
  (define (random-source-state-ref s)
    (pseudo-random-generator->vector (random-source-generator s)))
  
  (define (random-source-state-set! s state)
    (set-random-source-generator! s (vector->pseudo-random-generator state)))
  
  (define (random-source-randomize! s)
    (set-random-source-generator! s (make-pseudo-random-generator)))
  
  (define (random-source-pseudo-randomize! s . ij)
    (parameterize ((current-pseudo-random-generator
                    (random-source-generator s)))
      (random-seed (modulo (equal-hash-code ij) 2147483648))))
  
  (define (my-random-integer n)
    (if (<= n 2147483647)
        (random n)
        (+ (* (my-random-integer (quotient n 1073741824)) 1073741824)
           (random 1073741824))))
  
  (define (random-source-make-integers s)
    (lambda (n)
      (parameterize ((current-pseudo-random-generator
                      (random-source-generator s)))
        (my-random-integer n))))
  
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
             (* (add1 (my-random-integer n)) unit)))))))
  
  (define random-integer
    (random-source-make-integers my-default-random-source))
  
  (define random-real
    (random-source-make-reals my-default-random-source))
  )
