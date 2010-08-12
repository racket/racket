(module sequence "pre-base.rkt"
  (require "list.rkt")
  
  (define empty-seqn
    (make-do-sequence
     (λ ()
       (values
        void
        void
        void
        (λ (pos) #f)
        (λ (val) #t)
        (λ (pos val) #t)))))
  
  (define (seqn->list s)
    (for/list ([v s]) v))
  
  (define-syntax-rule (-seqn-cons vs s)
    (make-do-sequence
     (λ ()
       (define-values (more? next) (sequence-generate s))
       (values
        (λ (pos)
          (if (zero? pos)
              vs
              (next)))
        (λ (pos) (if (zero? pos) 1 pos))
        0
        (λ (pos)
          (or (zero? pos) (more?)))
        (λ _ #t)
        (λ _ #t)))))
  (define seqn-cons
    (case-lambda
      [()
       (error 'seqn-cons "expects a sequence to extend, but received no arguments")]
      [(s)
       (-seqn-cons (values) s)]
      [(v s)
       (-seqn-cons (values v) s)]
      [vs*s
       ; XXX double reverse is bad but moving split-at causes a problem I can't figure
       (define s*vs (reverse vs*s))
       (-seqn-cons (apply values (reverse (cdr s*vs))) (car s*vs))]))
  
  (define (seqn-first s)
    (define-values (more? next) (sequence-generate s))
    (unless (more?)
      (error 'seqn-first "expects a sequence with at least one element"))
    (next))
  
  (define (seqn-rest s)
    (make-do-sequence
     (λ ()
       (define-values (more? next) (sequence-generate s))
       (unless (more?)
         (error 'seqn-rest "expects a sequence with at least one element"))
       (next)
       (values
        (λ (pos) (next))
        (λ (x) x)
        0
        (λ (pos) (more?))
        (λ _ #t)
        (λ _ #t)))))
  
  (define (seqn-length s)
    (define-values (more? next) (sequence-generate s))
    (let loop ([i 0])
      (if (more?)
          (begin (next) (loop (add1 i)))
          i)))
  
  (define (seqn-ref s i)
    (unless (and (exact-integer? i) (i . >= . 0))
      (error 'seqn-ref "expects an exact non-negative index, but got ~e" i))
    (define-values (more? next) (sequence-generate s))
    (let loop ([n i])
      (cond
        [(zero? n)
         (next)]
        [(more?)
         (next)
         (loop (sub1 n))]
        [else
         (error 'seqn-ref "expects a sequence with at least ~e element(s)" i)])))
  
  (define (seqn-tail s i)
    (unless (and (exact-integer? i) (i . >= . 0))
      (error 'seqn-tail "expects an exact non-negative index, but got ~e" i))
    (make-do-sequence
     (λ ()
       (define-values (more? next) (sequence-generate s))
       (let loop ([n i])
         (unless (zero? n)
           (unless (more?)
             (error 'seqn-tail "expects a sequence with at least ~e element(s)" i))
           (next)
           (loop (sub1 n))))
       (values
        (λ (pos) (next))
        (λ (x) x)
        0
        (λ (pos) (more?))
        (λ _ #t)
        (λ _ #t)))))
  
  (define (-seqn-append s0 l)
    (if (null? l)
        s0
        (make-do-sequence
         (λ ()
           (define remaining l)
           (define (next-pos pos)
             (cond
               [(more?)
                #t]
               [(null? remaining)
                #f]
               [else
                (let*-values ([(s1) (car remaining)]
                              [(next-more? next-next) (sequence-generate s1)])
                  (set! more? next-more?)
                  (set! next next-next)
                  (set! remaining (cdr remaining))
                  (next-pos pos))]))
           (define-values (more? next) (sequence-generate s0))
           (values
            (λ (pos) (next))
            (λ (x) x)
            0
            next-pos
            (λ _ #t)
            (λ _ #t))))))
  
  (define (seqn-append . l)
    (unless (andmap sequence? l)
      (error 'seqn-append "expects only sequence arguments, given ~e" l))
    (-seqn-append empty-seqn l))
  
  (define (seqn-map f s)
    (unless (procedure? f)
      (error 'seqn-map "expects a procedure as the first argument, given ~e" f))
    (make-do-sequence
     (λ ()
       (define-values (more? next) (sequence-generate s))
       (values
        (λ (pos) (call-with-values next f))
        (λ (x) x)
        0
        (λ (pos) (more?))
        (λ _ #t)
        (λ _ #t)))))
  
  (define (seqn-andmap f s)
    (define-values (more? next) (sequence-generate s))
    (let loop ()
      (if (more?)
          (and (call-with-values next f) (loop))
          #t)))
  
  (define (seqn-ormap f s)
    (define-values (more? next) (sequence-generate s))
    (let loop ()
      (if (more?)
          (or (call-with-values next f) (loop))
          #f)))
  
  (define (seqn-for-each f s)
    (define-values (more? next) (sequence-generate s))
    (let loop ()
      (when (more?)
        (call-with-values next f) 
        (loop))))
  
  (define (seqn-fold f i s)
    (define-values (more? next) (sequence-generate s))
    (let loop ([i i])
      (if (more?)
          (loop (call-with-values next (λ e (apply f i e))))
          i)))
  
  (define (seqn-filter f s)
    (unless (procedure? f)
      (error 'seqn-filter "expects a procedure as the first argument, given ~e" f))
    (make-do-sequence
     (λ ()
       (define-values (more? next) (sequence-generate s))
       (define next-vs #f)
       (define (next-pos pos)
         (if (more?)
             (call-with-values next
                               (λ vs
                                 (if (apply f vs)
                                     (begin (set! next-vs vs)
                                            #t)
                                     (next-pos pos))))
             #f))
       (values
        (λ (pos) (apply values next-vs))
        (λ (x) x)
        0
        next-pos
        (λ _ #t)
        (λ _ #t)))))
  
  (define (seqn-add-between s e)
    (make-do-sequence
     (λ ()
       (define-values (more? next) (sequence-generate s))
       (values
        (λ (pos) 
          (if pos
              (next)
              e))
        not
        #t
        (λ (pos) 
          (if pos
              (more?)
              #t))
        (λ _ #t)
        (λ _ #t)))))
  
  (define (seqn-count s)
    (define-values (more? next) (sequence-generate s))
    (let loop ([n 0])
      (if (more?)
          (begin (next) (loop (add1 n)))
          n)))
  
  (provide empty-seqn
           seqn->list
           seqn-cons
           seqn-first
           seqn-rest
           seqn-length
           seqn-ref
           seqn-tail
           seqn-append
           seqn-map
           seqn-andmap
           seqn-ormap
           seqn-for-each
           seqn-fold
           seqn-filter
           seqn-add-between
           seqn-count))