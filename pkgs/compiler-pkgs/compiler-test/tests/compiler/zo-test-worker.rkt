#lang racket/base
(require racket/cmdline
         compiler/zo-parse
         compiler/zo-marshal
         compiler/decompile
         racket/port
         racket/bool
         racket/list
         racket/match
         "zo-test-util.rkt")

(define (bytes-gulp f)
  (with-input-from-file f 
    (λ () (port->bytes (current-input-port)))))

(define (read-compiled-bytes bs)
  (define ib (open-input-bytes bs))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([read-accept-compiled #t])
                    (read ib)))
                (lambda ()
                  (close-input-port ib))))

(define (zo-parse/bytes bs)
  (define ib (open-input-bytes bs))
  (dynamic-wind void
                (lambda ()
                  (zo-parse ib))
                (lambda ()
                  (close-input-port ib))))

(define (bytes-not-equal?-error b1 b2)
  (unless (bytes=? b1 b2)
    (error 'bytes-not-equal?-error "Not equal")))

(define (replace-file file bytes)
  (with-output-to-file file
    (λ () (write-bytes bytes))
    #:exists 'truncate))

(define (equal?/why-not v1 v2)
  (define v1->v2 (make-hasheq))
  (define (interned-symbol=? s1 s2)
    (symbol=? (hash-ref! v1->v2 s1 s2) s2))
  (define (yield p m v1 v2)
    (error 'equal?/why-not "~a in ~a: ~S ~S"
           m (reverse p) v1 v2))
  (define (inner p v1 v2)
    (unless (eq? v1 v2)
      (match v1
        [(cons car1 cdr1)
         (match v2
           [(cons car2 cdr2)
            (inner (list* 'car p) car1 car2)
            (inner (list* 'cdr p) cdr1 cdr2)]
           [_
            (yield p "Not a cons on right" v1 v2)])]
        [(? vector?)
         (match v2
           [(? vector?)
            (define v1l (vector-length v1))
            (define v2l (vector-length v2))
            (if (= v1l v2l)
                (for ([i (in-range v1l)])
                  (inner (list* `(vector-ref ,i) p)
                         (vector-ref v1 i)
                         (vector-ref v2 i)))
                (yield p "Vector lengths not equal" v1 v2))]
           [_
            (yield p "Not a vector on right" v1 v2)])]
        [(? struct?)
         (match v2
           [(? struct?)
            (define vv1 (struct->vector v1))
            (define vv2 (struct->vector v2))
            (inner (list* `(struct->vector ,(vector-ref vv1 0)) p)
                   vv1 vv2)]
           [_
            (yield p "Not a struct on right" v1 v2)])]
        [(? hash?)
         (match v2
           [(? hash?)
            (let ([p (list* 'in-hash p)])
              (for ([(k1 hv1) (in-hash v1)])
                (define hv2 
                  (hash-ref v2 k1
                            (lambda ()
                              (yield p (format "~S not in hash on right" k1) v1 v2))))
                (inner (list* `(hash-ref ,k1) p)
                       hv1 hv2)))]
           [_
            (yield p "Not a hash on right" v1 v2)])]
        [(? module-path-index?)
         (match v2
           [(? module-path-index?)
            (define-values (mp1 bmpi1) (module-path-index-split v1))
            (define-values (mp2 bmpi2) (module-path-index-split v2))
            (inner (list* 'module-path-index-split_0 p) mp1 mp2)
            (inner (list* 'module-path-index-split_1 p) bmpi1 bmpi2)]
           [_
            (yield p "Not a module path index on right" v1 v2)])]
        [(? string?)
         (match v2
           [(? string?)
            (unless (string=? v1 v2)
              (yield p "Unequal strings" v1 v2))]
           [_
            (yield p "Not a string on right" v1 v2)])]
        [(? bytes?)
         (match v2
           [(? bytes?)
            (unless (bytes=? v1 v2)
              (yield p "Unequal bytes" v1 v2))]
           [_
            (yield p "Not a bytes on right" v1 v2)])]
        [(? path?)
         (match v2
           [(? path?)
            (unless (equal? v1 v2)
              (yield p "Unequal paths" v1 v2))]
           [_
            (yield p "Not a path on right" v1 v2)])]
        [(? number?)
         (match v2
           [(? number?)
            (unless (equal? v1 v2)
              (yield p "Unequal numbers" v1 v2))]
           [_
            (yield p "Not a number on right" v1 v2)])]
        [(? regexp?)
         (match v2
           [(? regexp?)
            (unless (string=? (object-name v1) (object-name v2))
              (yield p "Unequal regexp" v1 v2))]
           [_
            (yield p "Not a regexp on right" v1 v2)])]
        [(? byte-regexp?)
         (match v2
           [(? byte-regexp?)
            (unless (bytes=? (object-name v1) (object-name v2))
              (yield p "Unequal byte-regexp" v1 v2))]
           [_
            (yield p "Not a byte-regexp on right" v1 v2)])]
        [(? box?)
         (match v2
           [(? box?)
            (inner (list* 'unbox) (unbox v1) (unbox v2))]
           [_
            (yield p "Not a box on right" v1 v2)])]
        [(? symbol?)
         (match v2
           [(? symbol?)
            (unless (symbol=? v1 v2)
              (cond
                [(and (symbol-interned? v1) (not (symbol-interned? v1)))
                 (yield p "Not interned symbol on right" v1 v2)]
                [(and (symbol-unreadable? v1) (not (symbol-unreadable? v1)))
                 (yield p "Not unreadable symbol on right" v1 v2)]
                [(and (symbol-uninterned? v1) (not (symbol-uninterned? v1)))
                 (yield p "Not uninterned symbol on right" v1 v2)]
                [(and (symbol-uninterned? v1) (symbol-uninterned? v2))
                 (unless (interned-symbol=? v1 v2)
                   (yield p "Uninterned symbols don't align" v1 v2))]
                [else
                 (yield p "Other symbol-related problem" v1 v2)]))]
           [_
            (yield p "Not a symbol on right" v1 v2)])]
        [(? empty?)
         (yield p "Not empty on right" v1 v2)]
        [_
         (yield p "Cannot inspect values deeper" v1 v2)])))
  (inner empty v1 v2))  

(define (symbol-uninterned? s)
  (not (or (symbol-interned? s) (symbol-unreadable? s))))

(define (run-with-limit file k thnk)
  (define file-custodian (make-custodian))
  (define ch (make-channel))
  (custodian-limit-memory file-custodian k)
  (define worker-thread
    (parameterize ([current-custodian file-custodian])
      (thread
       (lambda ()
         (define r (thnk))
         (channel-put ch r)
         (channel-get ch)))))
  (begin0
    (sync
     (handle-evt ch
                 (lambda (v)
                   (when (exn? v) (raise v))
                   v))
     (handle-evt worker-thread
                 (lambda _
                   (record! (failure 'memory #f "Over memory limit")))))
    (custodian-shutdown-all file-custodian)))

(define-syntax run/stages*
  (syntax-rules ()
    [(_ file) 
     (record! (success 'everything))]
    [(_ file [step1 serious? e] . rst)
     (let/ec esc
       (let ([step1 (with-handlers ([exn:fail? 
                                     (lambda (x)
                                       (record! (failure 'step1 serious? 
                                                         (exn-message x)))
                                       (if serious?
                                           (esc #f)
                                           #f))])
                      (begin0 e
                              (record! (success 'step1))))])
         (run/stages* file . rst)))]))

(define-syntax-rule (define-stages (run! file)
                      [stage serious? e] ...)
  (define (run! file)
    (run/stages* file [stage serious? e] ...)))

(define-stages (run! file)
  [read-orig 
   #t
   (bytes-gulp file)]
  [parse-orig 
   #t
   (zo-parse/bytes read-orig)]
  [marshal-parsed 
   #t
   (zo-marshal parse-orig)]
  [parse-marshalled 
   #t
   (zo-parse/bytes marshal-parsed)]
  #;[compare-parsed-to-parsed-marshalled 
   #f
   (equal?/why-not parse-orig parse-marshalled)]
  #;[marshal-marshalled 
   #t
   (zo-marshal parse-marshalled)]
  #;[compare-marshalled-to-marshalled-marshalled 
   #f
   (bytes-not-equal?-error marshal-parsed marshal-marshalled)]
  #;[replace-with-marshalled 
     #t
     (replace-file file marshal-marshalled)]
  #;[decompile-parsed 
   #t
   (decompile parse-orig)]
  [c-parse-marshalled
   #t
   (read-compiled-bytes marshal-parsed)]
  #;[compare-orig-to-marshalled 
   #f
   (bytes-not-equal?-error read-orig marshal-parsed)])

(define RESULTS empty)
(define (record! v)
  (set! RESULTS (list* v RESULTS)))
(define (run-test file)
  (run-with-limit 
   file
   (* 1024 1024 512)
   (lambda ()
     (run! file)))
  (write (reverse RESULTS)))

(command-line #:program "zo-test-worker" 
              #:args (file)
              (run-test file))

(module test racket/base)
