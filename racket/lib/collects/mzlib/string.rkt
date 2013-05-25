#lang racket/base

(provide string-lowercase!
         string-uppercase!
         eval-string
         read-from-string
         read-from-string-all
         expr->string
         
         real->decimal-string
         regexp-quote
         regexp-replace-quote
         regexp-match*
         regexp-match-positions*
         regexp-match-peek-positions*
         regexp-split
         regexp-match-exact?
         (rename-out [regexp-try-match regexp-match/fail-without-reading])

         glob->regexp)

(define ((make-string-do! translate who) s)
  (if (and (string? s) (not (immutable? s)))
    (let loop ([n (sub1 (string-length s))])
      (unless (negative? n)
        (string-set! s n (translate (string-ref s n)))
        (loop (sub1 n))))
    (raise-type-error who "mutable string" s)))
(define string-lowercase! (make-string-do! char-downcase 'string-lowercase!))
(define string-uppercase! (make-string-do! char-upcase   'string-uppercase!))

;; helpers for eval-string and read-from-string-one-or-all
(define-syntax wrap-errors
  (syntax-rules ()
    [(wrap-errors who error-handler body ...)
     (if error-handler
       (with-handlers
           ([void
             (cond [(not (procedure? error-handler))
                    (error who "bad error handler: ~e" error-handler)]
                   [(procedure-arity-includes? error-handler 1)
                    error-handler]
                   [(procedure-arity-includes? error-handler 0)
                    (lambda (exn) (error-handler))]
                   [else (error who "bad error handler: ~e" error-handler)])])
         body ...)
       (begin body ...))]))
(define (open-input-bstring s)
  (if (bytes? s) (open-input-bytes s) (open-input-string s)))

(define (eval-string str [error-handler #f])
  (wrap-errors 'eval-string error-handler
    (let ([p (open-input-bstring str)])
      (apply values
             (let loop ()
               (let ([e (read p)])
                 (if (eof-object? e)
                   '()
                   (call-with-values
                       (lambda () (eval e))
                       (lambda vals (append vals (loop)))))))))))

(define (read-from-string str [error-handler #f])
  (wrap-errors 'read-from-string error-handler
    (read (open-input-bstring str))))

(define (read-from-string-all str [error-handler #f])
  (let ([p (open-input-bstring str)])
    (wrap-errors 'read-from-string-all error-handler
      (let loop ([r '()])
        (let ([v (read p)])
          (if (eof-object? v) (reverse r) (loop (cons v r))))))))

(define (expr->string v)
  (let ([port (open-output-string)])
    (write v port)
    (get-output-string port)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define glob->regexp
  (let-values
      ([(def-case-sens) (eq? (system-path-convention-type) 'unix)]
       [(item:s item:b simple-item:s simple-item:b)
        (let ([rx (lambda (s)
                    (string-append
                     "(?:"
                     "[\\]." ; escaped item
                     "|"
                     "[*?]"  ; wildcards -- the only 1-character match
                     s       ; [*] more stuff here
                     ")"
                     ))]
              [range "|\\[(?:\\^?\\]|\\^?[^]^])[^]]*\\]"]) ; goes in [*]
          (values (regexp (rx range))
                  (byte-regexp (string->bytes/utf-8 (rx range)))
                  (regexp (rx ""))
                  (byte-regexp (string->bytes/utf-8 (rx "")))))])
    (lambda (glob [hide-dots? #t] [case-sens? def-case-sens] [simple? #f])
      (let*-values ([(b?) (cond [(bytes? glob) #t]
                                [(string? glob) #f]
                                [else (raise-type-error
                                       'glob->regexp
                                       "string or byte string" glob)])]
                    [(app sub ref rx item star any one)
                     (if b?
                       (values bytes-append subbytes bytes-ref byte-regexp
                               (if simple? simple-item:b item:b)
                               (char->integer #\*) #".*" #".")
                       (values string-append substring string-ref regexp
                               (if simple? simple-item:s item:s)
                               #\* ".*" "."))]
                    [(pfx sfx) (if case-sens?
                                 (if b? (values #"^" #"$")
                                        (values  "^"  "$"))
                                 (if b? (values #"^(?i:" #")$")
                                        (values  "^(?i:"  ")$")))]
                    [(pfx) (if hide-dots?
                             (app pfx (if b? #"(?![.])" "(?![.])"))
                             pfx)]
                    [(subq) (lambda xs (regexp-quote (apply sub xs)))])
        (let loop ([i 0] [ps (regexp-match-positions* item glob)] [r '()])
          (if (null? ps)
            (let ([r (apply app (reverse (cons (subq glob i) r)))])
              (rx (app pfx r sfx)))
            (loop (cdar ps) (cdr ps)
                  ;; length=1 is only for `*' or `?'
                  (cons (if (= 1 (- (cdar ps) (caar ps)))
                          (if (equal? star (ref glob (caar ps))) any one)
                          (sub glob (caar ps) (cdar ps)))
                        (if (= i (caar ps))
                          r (cons (subq glob i (caar ps)) r))))))))))
