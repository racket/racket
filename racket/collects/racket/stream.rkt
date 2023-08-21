#lang racket/base

(require racket/private/generic
         racket/generic
         racket/contract/base
         racket/contract/combinator
         racket/function
         racket/generator
         racket/match
         (rename-in "private/for.rkt"
                    [stream-ref stream-get-generics])
         "private/sequence.rkt"
         (only-in "private/stream-cons.rkt"
                  stream-cons
                  stream-lazy
                  stream-force
                  unpack-multivalue
                  thunk->multivalue)
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
         stream-lazy
         stream-force

         stream
         stream*
         stream->list
         stream-length
         stream-ref
         stream-tail
         stream-take
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
                           (quote-syntax stream-rest))
                     (list #t #t #t)))

(define-match-expander stream
  (syntax-rules (values)
    [(_) (? stream-empty?)]
    [(_ (values hd ...) tl ...)
     (? stream-cons?
        (app stream-first hd ...)
        (app stream-rest (stream tl ...)))]
    [(_ hd tl ...)
     (? stream-cons?
        (app stream-first hd)
        (app stream-rest (stream tl ...)))])
  (syntax-rules ()
    ((_)
     empty-stream)
    ((_ tl)
     ;; shortcut:
     (stream-cons tl #:eager empty-stream))
    ((_ hd tl ...)
     (stream-cons hd (stream tl ...)))))

(define-match-expander stream*
  (syntax-rules (values)
    [(_ tl) (? stream? tl)]
    [(_ (values hd ...) tl ...)
     (? stream-cons?
        (app stream-first hd ...)
        (app stream-rest (stream* tl ...)))]
    [(_ hd tl ...)
     (? stream-cons?
        (app stream-first hd)
        (app stream-rest (stream* tl ...)))])
  (syntax-rules ()
    [(_ tl)
     (stream-lazy #:who 'stream* tl)]
    [(_ hd tl ...)
     (stream-cons hd (stream* tl ...))]))

(define (stream-cons? st)
  (and (stream? st) (not (stream-empty? st))))

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
                             ;; Why `"stream" st` is omitted:
                             ;; including `st` in the error message
                             ;; means that it has to be kept live;
                             ;; that's not so great for a stream, where
                             ;; lazy construction could otherwise allow
                             ;; a element to be reached without consuming
                             ;; proportional memory
                             #;"stream" #;st)]
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
                              ;; See "Why `"stream" st` is omitted" above
                              #;"stream" #;st)]
      [else
       (loop (sub1 n) (stream-rest s))])))

(define (stream-take st i)
  (unless (stream? st) (raise-argument-error 'stream-take "stream?" st))
  (unless (exact-nonnegative-integer? i)
    (raise-argument-error 'stream-take "exact-nonnegative-integer?" i))
  (stream-lazy
   (let loop ([n i] [s st])
     (cond
       [(zero? n) empty-stream]
       [(stream-empty? s)
        (raise-arguments-error 'stream-take
                               "stream ended before index"
                               "index" i
                               ;; See "Why `"stream" st` is omitted" above
                               #;"stream" #;st)]
       [else
        (stream-cons (stream-first s)
                     (loop (sub1 n) (stream-rest s)))]))))

(define (stream-append . l)
  (for ([s (in-list l)])
    (unless (stream? s) (raise-argument-error 'stream-append "stream?" s)))
  (stream-lazy (streams-append l)))

(define (streams-append l)
  (cond
   [(null? l) empty-stream]
   [(null? (cdr l)) (car l)]
   [(stream-empty? (car l)) (streams-append (cdr l))]
   [else
    (stream-cons (stream-first (car l))
                 (streams-append (cons (stream-rest (car l)) (cdr l))))]))

(define (stream-map f s)
  (unless (procedure? f) (raise-argument-error 'stream-map "procedure?" f))
  (unless (stream? s) (raise-argument-error 'stream-map "stream?" s))
  (stream-lazy
   (let loop ([s s])
     (cond
       [(stream-empty? s) empty-stream]
       [else (stream-cons (call-with-values (λ () (stream-first s)) f)
                          (loop (stream-rest s)))]))))

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
  (stream-lazy
   (let loop ([s s])
     (cond
       [(stream-empty? s) empty-stream]
       [(call-with-values (λ () (stream-first s)) f)
        (define v (thunk->multivalue (λ () (stream-first s))))
        (stream-cons (unpack-multivalue v)
                     (loop (stream-rest s)))]
       [else (loop (stream-rest s))]))))

(define (stream-add-between s e)
  (unless (stream? s)
    (raise-argument-error 'stream-add-between "stream?" s))
  (stream-lazy
   (cond
     [(stream-empty? s) empty-stream]
     [else
      (stream-cons
       (stream-first s)
       (let loop ([s (stream-rest s)])
         (cond
           [(stream-empty? s) empty-stream]
           [else
            (stream-cons e
                         (stream-cons (stream-first s)
                                      (loop (stream-rest s))))])))])))

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
         (with-syntax ([((pre-body ...) (post-body ...)) (split-for-body stx #'body)])
           (quasisyntax/loc stx
             (stream-lazy
               (#,derived-stx #,stx
                              ([get-rest empty-stream]
                               #:delay-with thunk)
                 clauses
                 pre-body ...
                 (stream-cons (let () post-body ...) (get-rest))))))]))
    (values (make-for/stream #'for/foldr/derived)
            (make-for/stream #'for*/foldr/derived))))
