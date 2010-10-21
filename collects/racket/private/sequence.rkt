(module sequence "pre-base.rkt"
  (require "list.rkt")
  
  (define empty-stream
    (make-do-sequence
     (λ ()
       (values
        void
        void
        void
        (λ (pos) #f)
        (λ (val) #t)
        (λ (pos val) #t)))))
  
  (define (stream->list s)
    (for/list ([v s]) v))
  
  (define-syntax-rule (-stream-cons vs s)
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
  (define stream-cons
    (case-lambda
      [()
       (error 'stream-cons "expects a sequence to extend, but received no arguments")]
      [(s)
       (-stream-cons (values) s)]
      [(v s)
       (-stream-cons (values v) s)]
      [vs*s
       ; XXX double reverse is bad but moving split-at causes a problem I can't figure
       (define s*vs (reverse vs*s))
       (-stream-cons (apply values (reverse (cdr s*vs))) (car s*vs))]))
  
  (define (stream-first s)
    (define-values (more? next) (sequence-generate s))
    (unless (more?)
      (error 'stream-first "expects a sequence with at least one element"))
    (next))
  
  (define (stream-rest s)
    (make-do-sequence
     (λ ()
       (define-values (more? next) (sequence-generate s))
       (unless (more?)
         (error 'stream-rest "expects a sequence with at least one element"))
       (next)
       (values
        (λ (pos) (next))
        (λ (x) x)
        0
        (λ (pos) (more?))
        (λ _ #t)
        (λ _ #t)))))
  
  (define (stream-length s)
    (define-values (more? next) (sequence-generate s))
    (let loop ([i 0])
      (if (more?)
          (begin (next) (loop (add1 i)))
          i)))
  
  (define (stream-ref s i)
    (unless (and (exact-integer? i) (i . >= . 0))
      (error 'stream-ref "expects an exact non-negative index, but got ~e" i))
    (define-values (more? next) (sequence-generate s))
    (let loop ([n i])
      (cond
        [(zero? n)
         (next)]
        [(more?)
         (next)
         (loop (sub1 n))]
        [else
         (error 'stream-ref "expects a sequence with at least ~e element(s)" i)])))
  
  (define (stream-tail s i)
    (unless (and (exact-integer? i) (i . >= . 0))
      (error 'stream-tail "expects an exact non-negative index, but got ~e" i))
    (make-do-sequence
     (λ ()
       (define-values (more? next) (sequence-generate s))
       (let loop ([n i])
         (unless (zero? n)
           (unless (more?)
             (error 'stream-tail "expects a sequence with at least ~e element(s)" i))
           (next)
           (loop (sub1 n))))
       (values
        (λ (pos) (next))
        (λ (x) x)
        0
        (λ (pos) (more?))
        (λ _ #t)
        (λ _ #t)))))
  
  (define (-stream-append s0 l)
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
  
  (define (stream-append . l)
    (unless (andmap sequence? l)
      (error 'stream-append "expects only sequence arguments, given ~e" l))
    (-stream-append empty-stream l))
  
  (define (stream-map f s)
    (unless (procedure? f)
      (error 'stream-map "expects a procedure as the first argument, given ~e" f))
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
  
  (define (stream-andmap f s)
    (define-values (more? next) (sequence-generate s))
    (let loop ()
      (if (more?)
          (and (call-with-values next f) (loop))
          #t)))
  
  (define (stream-ormap f s)
    (define-values (more? next) (sequence-generate s))
    (let loop ()
      (if (more?)
          (or (call-with-values next f) (loop))
          #f)))
  
  (define (stream-for-each f s)
    (define-values (more? next) (sequence-generate s))
    (let loop ()
      (when (more?)
        (call-with-values next f) 
        (loop))))
  
  (define (stream-fold f i s)
    (define-values (more? next) (sequence-generate s))
    (let loop ([i i])
      (if (more?)
          (loop (call-with-values next (λ e (apply f i e))))
          i)))
  
  (define (stream-filter f s)
    (unless (procedure? f)
      (error 'stream-filter "expects a procedure as the first argument, given ~e" f))
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
  
  (define (stream-add-between s e)
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
  
  (define (stream-count f s)
    (unless (procedure? f)
      (error 'stream-count "expects a procedure as the first argument, given ~e" f))
    (define-values (more? next) (sequence-generate s))
    (let loop ([n 0])
      (if (more?)
          (if (call-with-values next f)
              (loop (add1 n))
              (loop n))
          n)))
  
  (provide empty-stream
           stream->list
           stream-cons
           stream-first
           stream-rest
           stream-length
           stream-ref
           stream-tail
           stream-append
           stream-map
           stream-andmap
           stream-ormap
           stream-for-each
           stream-fold
           stream-filter
           stream-add-between
           stream-count))
