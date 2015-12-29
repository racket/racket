#lang racket/base

(require racket/private/generic
         racket/generic
         racket/contract/base
         racket/contract/combinator
         racket/generator
         (rename-in "private/for.rkt"
                    [stream-ref stream-get-generics])
         "private/sequence.rkt"
         (only-in "private/stream-cons.rkt"
                  stream-cons)
         "private/generic-methods.rkt"
         (for-syntax racket/base))

(provide empty-stream
         stream-cons
         stream?
         gen:stream
         ;; we don't need the generics versions of these because
         ;; the original sequence functions will work fine
         ;; for the dispatch. (the method table layout is
         ;; identical)
         stream-empty?
         stream-first
         stream-rest
         prop:stream
         in-stream

         stream
         stream*
         stream->list
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
         stream-count
         
         stream/c

         for/stream
         for*/stream)

(define-syntax gen:stream
  (make-generic-info (quote-syntax gen:stream)
                     (quote-syntax prop:stream)
                     (quote-syntax stream-via-prop?)
                     (quote-syntax stream-get-generics)
                     (list (quote-syntax stream-empty?)
                           (quote-syntax stream-first)
                           (quote-syntax stream-rest))
                     (list (quote-syntax stream-empty?)
                           (quote-syntax stream-first)
                           (quote-syntax stream-rest))))

(define-syntax stream
  (syntax-rules ()
    ((_)
     empty-stream)
    ((_ hd tl ...)
     (stream-cons hd (stream tl ...)))))

(define-syntax stream*
  (syntax-rules ()
    [(_ hd tl)
     (stream-cons hd tl)]
    [(_ hd tl ...)
     (stream-cons hd (stream* tl ...))]))

(define (stream->list s)
  (for/list ([v (in-stream s)]) v))

(define (stream-length s)
  (unless (stream? s) (raise-argument-error 'stream-length "stream?" s))
  (let loop ([s s] [len 0])
    (if (stream-empty? s)
        len
        (loop (stream-rest s) (add1 len)))))

(define (stream-ref st i)
  (unless (stream? st) (raise-argument-error 'stream-ref "stream?" st))
  (unless (exact-nonnegative-integer? i)
    (raise-argument-error 'stream-ref "exact-nonnegative-integer?" i))
  (let loop ([n i] [s st])
    (cond
     [(stream-empty? s)
      (raise-arguments-error 'stream-ref
                             "stream ended before index"
                             "index" i
                             "stream" st)]
     [(zero? n)
      (stream-first s)]
     [else
      (loop (sub1 n) (stream-rest s))])))

(define (stream-tail st i)
  (unless (stream? st) (raise-argument-error 'stream-tail "stream?" st))
  (unless (exact-nonnegative-integer? i)
    (raise-argument-error 'stream-tail "exact-nonnegative-integer?" i))
  (let loop ([n i] [s st])
    (cond
     [(zero? n) s]
     [(stream-empty? s)
      (raise-arguments-error 'stream-tail
                             "stream ended before index"
                             "index" i
                             "stream" st)]
     [else
      (loop (sub1 n) (stream-rest s))])))

(define (stream-append . l)
  (for ([s (in-list l)])
    (unless (stream? s) (raise-argument-error 'stream-append "stream?" s)))
  (streams-append l))

(define (streams-append l)
  (cond
   [(null? l) empty-stream]
   [(null? (cdr l)) (car l)]
   [(stream-empty? (car l)) (streams-append (cdr l))]
   [else
    (make-do-stream (lambda () #f)
                    (lambda () (stream-first (car l)))
                    (lambda () (streams-append (cons (stream-rest (car l)) (cdr l)))))]))

(define (stream-map f s)
  (unless (procedure? f) (raise-argument-error 'stream-map "procedure?" f))
  (unless (stream? s) (raise-argument-error 'stream-map "stream?" s))
  (let loop ([s s])
    (if (stream-empty? s)
      empty-stream
      (stream-cons (f (stream-first s)) (loop (stream-rest s))))))

(define (stream-andmap f s)
  (unless (procedure? f) (raise-argument-error 'stream-andmap "procedure?" f))
  (unless (stream? s) (raise-argument-error 'stream-andmap "stream?" s))
  (sequence-andmap f s))

(define (stream-ormap f s)
  (unless (procedure? f) (raise-argument-error 'stream-ormap "procedure?" f))
  (unless (stream? s) (raise-argument-error 'stream-ormap "stream?" s))
  (sequence-ormap f s))

(define (stream-for-each f s)
  (unless (procedure? f) (raise-argument-error 'stream-for-each "procedure?" f))
  (unless (stream? s) (raise-argument-error 'stream-for-each "stream?" s))
  (sequence-for-each f s))

(define (stream-fold f i s)
  (unless (procedure? f) (raise-argument-error 'stream-fold "procedure?" f))
  (unless (stream? s) (raise-argument-error 'stream-fold "stream?" s))
  (sequence-fold f i s))

(define (stream-count f s)
  (unless (procedure? f) (raise-argument-error 'stream-count "procedure?" f))
  (unless (stream? s) (raise-argument-error 'stream-count "stream?" s))
  (sequence-count f s))

(define (stream-filter f s)
  (unless (procedure? f) (raise-argument-error 'stream-filter "procedure?" f))
  (unless (stream? s) (raise-argument-error 'stream-filter "stream?" s))
  (cond
   [(stream-empty? s) empty-stream]
   [else
    (let ([done? #f]
          [empty? #f]
          [fst #f]
          [rst #f])
      (define (force!)
        (unless done?
          (let loop ([s s])
            (cond
             [(stream-empty? s)
              (set! done? #t)
              (set! empty? #t)]
             [(f (stream-first s))
              (set! fst (stream-first s))
              (set! rst (stream-filter f (stream-rest s)))]
             [else (loop (stream-rest s))]))
          (set! done? #t)))
      (make-do-stream (lambda () (force!) empty?)
                      (lambda () (force!) fst)
                      (lambda () (force!) rst)))]))

(define (stream-add-between s e)
  (unless (stream? s)
    (raise-argument-error 'stream-add-between "stream?" s))
  (if (stream-empty? s)
      empty-stream
      (stream-cons
       (stream-first s)
       (let loop ([s (stream-rest s)])
         (cond [(stream-empty? s) empty-stream]
               [else (stream-cons e (stream-cons (stream-first s)
                                                 (loop (stream-rest s))))])))))

;; Impersonators and Chaperones ----------------------------------------------------------------------
;; (these are private because they would fail on lists, which satisfy `stream?`)

(define (impersonate-stream s first-proc rest-proc . props)
  (impersonate-generics
   gen:stream
   s
   [stream-first
    (λ (stream-first)
      (impersonate-procedure stream-first
                             (λ (s) (values (λ (v) (first-proc v)) s))))]
   [stream-rest
    (λ (stream-rest)
      (impersonate-procedure stream-rest
                             (λ (s) (values (λ (v) (rest-proc v)) s))))]
   #:properties props))

(define (chaperone-stream s first-proc rest-proc . props)
  (chaperone-generics
   gen:stream
   s
   [stream-first
    (λ (stream-first)
      (chaperone-procedure stream-first
                           (λ (s) (values (λ (v) (first-proc v)) s))))]
   [stream-rest
    (λ (stream-rest)
      (chaperone-procedure stream-rest
                           (λ (s) (values (λ (v) (rest-proc v)) s))))]
   #:properties props))

;; Stream contracts ----------------------------------------------------------------------------------

(define (stream/c-name ctc)
  (define elem-name (contract-name (base-stream/c-content ctc)))
  (apply build-compound-type-name
         'stream/c
         elem-name
         '()))

(define (add-stream-context blame)
  (blame-add-context blame "a value generated by"))

(define (stream/c-stronger? a b)
  (contract-stronger? (base-stream/c-content a) (base-stream/c-content b)))

(define ((late-neg-projection impersonate/chaperone-stream) ctc)
  (define elem-ctc (base-stream/c-content ctc))
  (define listof-elem-ctc (listof elem-ctc))
  (define elem-ctc-late-neg (get/build-late-neg-projection elem-ctc))
  (define listof-elem-ctc-late-neg (get/build-late-neg-projection listof-elem-ctc))
  (λ (blame)
    (define stream-blame (add-stream-context blame))
    (define elem-ctc-late-neg-acceptor (elem-ctc-late-neg stream-blame))
    (define listof-elem-ctc-neg-acceptor (listof-elem-ctc-late-neg stream-blame))
    (define (stream/c-late-neg-proj-val-acceptor val neg-party)
      (unless (stream? val)
        (raise-blame-error blame #:missing-party neg-party
                           val '(expected "a stream" given: "~e") val))
      (define blame+neg-party (cons blame neg-party))
      (if (list? val)
          (listof-elem-ctc-neg-acceptor val neg-party)
          (impersonate/chaperone-stream
           val
           (λ (v) (with-contract-continuation-mark
                   blame+neg-party
                   (elem-ctc-late-neg-acceptor v neg-party)))
           (λ (v)
             (with-contract-continuation-mark
              blame+neg-party
              (if (list? v)
                  (listof-elem-ctc-neg-acceptor v neg-party)
                  (stream/c-late-neg-proj-val-acceptor v neg-party))))
           impersonator-prop:contracted ctc
           impersonator-prop:blame stream-blame)))
    stream/c-late-neg-proj-val-acceptor))

(struct base-stream/c (content))

(struct chaperone-stream/c base-stream/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:name stream/c-name
     #:first-order stream?
     #:stronger stream/c-stronger?
     #:late-neg-projection (late-neg-projection chaperone-stream))))

(struct impersonator-stream/c base-stream/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name stream/c-name
   #:first-order stream?
   #:stronger stream/c-stronger?
   #:late-neg-projection (late-neg-projection impersonate-stream)))

(define (stream/c elem)
  (define ctc (coerce-contract 'stream/c elem))
  (if (chaperone-contract? ctc)
      (chaperone-stream/c ctc)
      (impersonator-stream/c ctc)))

;; Stream comprehensions -----------------------------------------------------------------------------

(define-syntaxes (for/stream for*/stream)
  (let ()
    (define ((make-for/stream derived-stx) stx)
      (syntax-case stx ()
        [(_ clauses . body)
         (begin
           (when (null? (syntax->list #'body))
             (raise-syntax-error (syntax-e #'derived-stx)
                                 "missing body expression after sequence bindings"
                                 stx #'body))
           #`(sequence->stream
              (in-generator
               (#,derived-stx #,stx () clauses
                (yield (let () . body))
                (values)))))]))
    (values (make-for/stream #'for/fold/derived)
            (make-for/stream #'for*/fold/derived))))
