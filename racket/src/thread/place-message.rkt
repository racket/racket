#lang racket/base
(require '#%flfxnum
         (only-in '#%foreign cpointer? ptr-add)
         (only-in '#%place place-shared?)
         racket/prefab)

(provide place-message-allowed?
         place-message-allowed-direct?

         message-ize
         un-message-ize
         
         prop:place-message)

;; A prop:place-message value is a procedure that takes self
;; and returns either #f [=> not a place mesage after all] or
;; (-> (-> v))
(define-values (prop:place-message place-message? place-message-ref)
  (make-struct-type-property 'place-message))

(struct message-ized (unmessage)
  #:authentic)

(define (allowed? v #:direct? direct?)
  (let loop ([v v] [graph #hasheq()])
    (or (number? v)
        (char? v)
        (boolean? v)
        (keyword? v)
        (void? v)
        (symbol? v)
        (and (or (string? v)
                 (bytes? v))
             (or (not direct?) (immutable? v)))
        (null? v)
        (and (pair? v)
             (or (hash-ref graph v #f)
                 (let ([graph (hash-set graph v #t)])
                   (and (loop (car v) graph)
                        (loop (cdr v) graph)))))
        (and (vector? v)
             (or (not direct?)
                 (and (immutable? v)
                      (not (impersonator? v))))
             (let ([graph (hash-set graph v #t)])
               (for/and ([e (in-vector v)])
                 (loop e graph))))
        (and (immutable-prefab-struct-key v)
             (let ([graph (hash-set graph v #t)])
               (for/and ([e (in-vector (struct->vector v))])
                 (loop e graph))))
        (and (hash? v)
             (or (not direct?)
                 (and (immutable? v)
                      (not (impersonator? v))))
             (let ([graph (hash-set graph v #t)])
               (for/and ([(k v) (in-hash v)])
                 (and (loop k graph)
                      (loop v graph)))))
        (and (not direct?)
             (or (cpointer? v)
                 (and (or (fxvector? v)
                          (flvector? v))
                      (place-shared? v))
                 (and (place-message? v)
                      ((place-message-ref v) v)
                      #t))))))

(define (place-message-allowed-direct? v)
  (allowed? v #:direct? #t))
  
(define (place-message-allowed? v)
  (allowed? v #:direct? #f))

;; Convert a message to a form suitable to keep in a channel, but
;; simultaneously check whether the message is ok, since a message
;; might start out with mutable elements that are changed while
;; the conversion is in progress (but we convert enough to avoid
;; problems afterward)
(define (message-ize v fail)
  (define graph #f)
  (define used #f)
  (define (maybe-ph ph v)
    (if (and used (hash-ref used ph #f))
        (begin
          (placeholder-set! ph v)
          ph)
        v))
  (define new-v
    (let loop ([v v])
      (cond
        [(or (number? v)
             (char? v)
             (boolean? v)
             (keyword? v)
             (void? v)
             (symbol? v)
             (null? v))
         v]
        [(string? v)
         (string->immutable-string v)]
        [(bytes? v)
         (bytes->immutable-bytes v)]
        [else
         (unless graph (set! graph (make-hasheq)))
         (cond
           [(hash-ref graph v #f)
            => (lambda (v)
                 (unless used (set! used (make-hasheq)))
                 (hash-set! used v #t)
                 v)]
           [(pair? v)
            (define ph (make-placeholder #f))
            (hash-set! graph v ph)
            (maybe-ph ph (cons (loop (car v))
                               (loop (cdr v))))]
           [(vector? v)
            (define ph (make-placeholder #f))
            (hash-set! graph v ph)
            (maybe-ph ph (for/vector #:length (vector-length v) ([e (in-vector v)])
                           (loop e)))]
           [(immutable-prefab-struct-key v)
            => (lambda (k)
                 (define ph (make-placeholder #f))
                 (hash-set! graph v ph)
                 (maybe-ph
                  ph
                  (apply make-prefab-struct
                         k
                         (for/list ([e (in-vector (struct->vector v) 1)])
                           (loop e)))))]
           [(hash? v)
            (define ph (make-placeholder #f))
            (hash-set! graph v ph)
            (cond
              [(hash-eq? v)
               (for/hasheq ([(k v) (in-hash v)])
                 (values (loop k) (loop v)))]
              [(hash-eqv? v)
               (for/hasheqv ([(k v) (in-hash v)])
                 (values (loop k) (loop v)))]
              [else
               (for/hash ([(k v) (in-hash v)])
                 (values (loop k) (loop v)))])]
           [(cpointer? v)
            (ptr-add v 0)]
           [(and (or (fxvector? v)
                     (flvector? v))
                 (place-shared? v))
            v]
           [(place-message? v)
            (define make-unmessager ((place-message-ref v) v))
            (if make-unmessager
                (message-ized (make-unmessager))
                (fail))]
           [else (fail)])])))
  (message-ized new-v))

(define (un-message-ize v)
  (if (message-ized? v)
      (make-reader-graph (do-un-message-ize (message-ized-unmessage v)))
      v))

(define (do-un-message-ize v)
  (define graph #f)
  (let loop ([v v])
    (cond
      [(placeholder? v)
       (define ph (make-placeholder #f))
       (unless graph (set! graph (make-hasheq)))
       (cond
         [(hash-ref graph v #f) => (lambda (ph) ph)]
         [else
          (hash-set! graph v ph)
          (placeholder-set! ph (loop (placeholder-get v)))
          ph])]
      [(pair? v)
       (cons (loop (car v)) (loop (cdr v)))]
      [(vector? v)
       (vector->immutable-vector
        (for/vector #:length (vector-length v) ([e (in-vector v)])
          (loop e)))]
      [(immutable-prefab-struct-key v)
       => (lambda (k)
            (apply make-prefab-struct
                   k
                   (for/list ([e (in-vector (struct->vector v) 1)])
                     (loop e))))]
      [(hash? v)
       (cond
         [(hash-eq? v)
          (for/hasheq ([(k v) (in-hash v)])
            (values (loop k) (loop v)))]
         [(hash-eqv? v)
          (for/hasheqv ([(k v) (in-hash v)])
            (values (loop k) (loop v)))]
         [else
          (for/hash ([(k v) (in-hash v)])
            (values (loop k) (loop v)))])]
      [(and (cpointer? v)
            v ; not #f
            (not (bytes? v)))
       (ptr-add v 0)]
      [(message-ized? v)
       ((message-ized-unmessage v))]
      [else v])))

(module+ test
  (define-syntax-rule (test expect actual)
    (let ([v actual])
      (unless (equal? expect v)
        (error 'test "failed: ~s = ~s" 'actual v))))

  (struct external (a)
    #:property prop:place-message (lambda (self)
                                    (lambda ()
                                      (define a (external-a self))
                                      (lambda () (external a)))))

  (test #t (place-message-allowed? "apple"))
  (test #t (place-message-allowed-direct? "apple"))
  (test #f (place-message-allowed-direct? (string-copy "apple")))
  (test #f (place-message-allowed-direct? (cons 1 (string-copy "apple"))))

  (test #t (place-message-allowed-direct? '(a . b)))
  (test #t (place-message-allowed-direct? '#(a b)))
  (test #t (place-message-allowed-direct? '#hasheq((a . b))))
  (test #t (place-message-allowed-direct? '#s(pre 1 2 3)))

  (define direct-cyclic (read (open-input-string "#0=(1 #0# 2)")))
  (test #t (place-message-allowed-direct? direct-cyclic))

  (define stateful-cyclic (make-reader-graph
                           (let ([ph (make-placeholder #f)]
                                 [ph2 (make-placeholder #f)]
                                 [ph3 (make-placeholder #f)])
                             (define (as ph v) (placeholder-set! ph v) v)
                             (as ph2 (vector (as ph (cons ph (string-copy "apple")))
                                             ph2
                                             (as ph3 (hasheq 'a 1 'b ph3))
                                             '#s(pre 4 5)))
                             ph)))
  (test #f (place-message-allowed-direct? stateful-cyclic))
  (test #t (place-message-allowed? stateful-cyclic))
  (test stateful-cyclic (un-message-ize (message-ize stateful-cyclic)))

  (define ext (external 'x))
  (test #t (place-message-allowed? ext))
  (test #f (place-message-allowed-direct? ext))
  (define ext2 (un-message-ize (message-ize ext)))
  (test #t (external? ext2)) 
  (test #f (eq? ext ext2))
  (test 'x (external-a ext2))

  (void))
