#lang racket/base

(require racket/private/generics
         (rename-in "private/for.rkt"
                    [stream-ref    stream-get-generics]
                    [stream-empty? -stream-empty]
                    [stream-first  -stream-first]
                    [stream-rest   -stream-rest])
         "private/sequence.rkt"
         (only-in "private/stream-cons.rkt"
                  stream-cons))

(provide empty-stream
         stream-cons
         stream?
         gen:stream
         ;; we don't need the generics versions of these because
         ;; the original sequence functions will work fine
         ;; for the dispatch. (the method table layout is
         ;; identical)
         (rename-out [-stream-empty stream-empty?]
                     [-stream-first stream-first]
                     [-stream-rest  stream-rest])
         prop:stream
         in-stream

         stream
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
         stream-count)

(define-generics (-stream gen:stream prop:stream stream?
                          #:defined-table defined-table
                          #:prop-defined-already? stream-get-generics)
  ;; These three are never used for the reasons explained above.
  ;; We still need the headers for clients who extend racket/stream.
  (stream-empty? -stream)
  (stream-first -stream)
  (stream-rest -stream))

(define-syntax stream
  (syntax-rules ()
    ((_)
     empty-stream)
    ((_ hd tl ...)
     (stream-cons hd (stream tl ...)))))

(define (stream->list s)
  (for/list ([v (in-stream s)]) v))
  
(define (stream-length s)
  (unless (stream? s) (raise-type-error 'stream-length "stream" s))
  (let loop ([s s] [len 0])
    (if (stream-empty? s)
        len
        (loop (stream-rest s) (add1 len)))))

(define (stream-ref st i)
  (unless (stream? st) (raise-type-error 'stream-ref "stream" st))
  (unless (exact-nonnegative-integer? i)
    (raise-type-error 'stream-ref "nonnegative exact integer" i))
  (let loop ([n i] [s st])
    (cond
     [(stream-empty? s)
      (raise-mismatch-error 'stream-ref
                            (format "sequence ended before element ~e: "
                                    (add1 i))
                            st)]
     [(zero? n)
      (stream-first s)]
     [else
      (loop (sub1 n) (stream-rest s))])))
  
(define (stream-tail st i)
  (unless (stream? st) (raise-type-error 'stream-tail "stream" st))
  (unless (exact-nonnegative-integer? i)
    (raise-type-error 'stream-tail "nonnegative exact integer" i))
  (let loop ([n i] [s st])
    (cond
     [(zero? n) s]
     [(stream-empty? s)
      (raise-mismatch-error 
       'stream-tail
       (format "sequence ended before ~e element~a: "
               i
               (if (= i 1) "" "s"))
       st)]
     [else
      (loop (sub1 n) (stream-rest s))])))

(define (stream-append . l)
  (for ([s (in-list l)])
    (unless (stream? s) (raise-type-error 'stream-append "stream" s)))
  (streams-append l))

(define (streams-append l)
  (cond
   [(null? l) empty-stream]
   [(stream-empty? (car l)) (streams-append (cdr l))]
   [else
    (make-do-stream (lambda () #f)
                    (lambda () (stream-first (car l)))
                    (lambda () (streams-append (cons (stream-rest (car l)) (cdr l)))))]))
  
(define (stream-map f s)
  (unless (procedure? f) (raise-type-error 'stream-map "procedure" f))
  (unless (stream? s) (raise-type-error 'stream-map "stream" s))
  (let loop ([s s])
    (cond
     [(stream-empty? s) empty-stream]
     [else (stream-cons (call-with-values (lambda () (stream-first s)) f)
                        (loop (stream-rest s)))])))
  
(define (stream-andmap f s)
  (unless (procedure? f) (raise-type-error 'stream-andmap "procedure" f))
  (unless (stream? s) (raise-type-error 'stream-andmap "stream" s))
  (sequence-andmap f s))
  
(define (stream-ormap f s)
  (unless (procedure? f) (raise-type-error 'stream-ormap "procedure" f))
  (unless (stream? s) (raise-type-error 'stream-ormap "stream" s))
  (sequence-ormap f s))
  
(define (stream-for-each f s)
  (unless (procedure? f) (raise-type-error 'stream-for-each "procedure" f))
  (unless (stream? s) (raise-type-error 'stream-for-each "stream" s))
  (sequence-for-each f s))
  
(define (stream-fold f i s)
  (unless (procedure? f) (raise-type-error 'stream-fold "procedure" f))
  (unless (stream? s) (raise-type-error 'stream-fold "stream" s))
  (sequence-fold f i s))

(define (stream-count f s)
  (unless (procedure? f) (raise-type-error 'stream-count "procedure" f))
  (unless (stream? s) (raise-type-error 'stream-count "stream" s))
  (sequence-count f s))
  
(define (stream-filter f s)
  (unless (procedure? f) (raise-type-error 'stream-filter "procedure" f))
  (unless (stream? s) (raise-type-error 'stream-filter "stream" s))
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
    (raise-type-error 'stream-add-between "stream" s))
  (if (stream-empty? s)
      empty-stream
      (stream-cons
       (stream-first s)
       (let loop ([s (stream-rest s)])
         (cond [(stream-empty? s) empty-stream]
               [else (stream-cons e (stream-cons (stream-first s)
                                                 (loop (stream-rest s))))])))))
