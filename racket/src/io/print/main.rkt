#lang racket/base
(require racket/flonum
         racket/fixnum
         racket/symbol
         racket/keyword
         racket/unsafe/undefined
         "../common/check.rkt"
         "../port/output-port.rkt"
         "../port/input-port.rkt"
         "../port/string-output.rkt"
         "../port/bytes-output.rkt"
         "../port/bytes-port.rkt"
         "../port/parameter.rkt"
         "../error/message.rkt"
         "custom-write.rkt"
         "write-with-max.rkt"
         "string.rkt"
         "bytes.rkt"
         "symbol.rkt"
         "char.rkt"
         "list.rkt"
         "mlist.rkt"
         "vector.rkt"
         "hash.rkt"
         "named.rkt"
         "parameter.rkt"
         "mode.rkt"
         "graph.rkt"
         "config.rkt"
         "regexp.rkt"
         "recur-handler.rkt")

(provide display
         write
         print

         newline
         
         prop:custom-write
         custom-write?
         custom-write-accessor

         prop:custom-print-quotable
         custom-print-quotable?
         custom-print-quotable-accessor

         set-printable-regexp?!

         (all-from-out "parameter.rkt"))

(module+ internal
  (provide display-via-handler
           write-via-handler
           print-via-handler

           do-display
           do-write
           do-print
           do-global-print

           install-do-global-print!))

(define/who (display v [o (current-output-port)])
  (display-via-handler who v o))

(define (display-via-handler who v o)
  (let ([co (->core-output-port o who)])
    (define display-handler (core-output-port-display-handler co))
    (if display-handler
        (display-handler v o)
        (do-display who v co))
    (void)))

(define (do-display who v o [max-length #f])
  (cond
    [(and (bytes? v) (not max-length))
     (unsafe-write-bytes who v o)
     (void)]
    [(and (string? v) (not max-length))
     (write-string v o)
     (void)]
    [else
     (define config (make-print-config))
     (dots (p who v DISPLAY-MODE o (sub3 max-length) (detect-graph v DISPLAY-MODE config) config) o)
     (void)]))

(define/who (write v [o (current-output-port)])
  (write-via-handler who v o))

(define (write-via-handler who v o)
  (let ([co (->core-output-port o who)])
    (define write-handler (core-output-port-write-handler co))
    (if write-handler
        (write-handler v o)
        (do-write who v co))
    (void)))

(define (do-write who v o [max-length #f])
  (define config (make-print-config))
  (dots (p who v WRITE-MODE o (sub3 max-length) (detect-graph v WRITE-MODE config) config) o)
  (void))

(define/who (print v [o (current-output-port)] [quote-depth PRINT-MODE/UNQUOTED])
  (print-via-handler who v o quote-depth))

(define/who (print-via-handler who v o quote-depth)
  (let ([co (->core-output-port o who)])
    (check who print-mode? #:contract "(or/c 0 1)" quote-depth)
    (define print-handler (core-output-port-print-handler co))
    (if print-handler
        (print-handler v o quote-depth)
        (do-global-print who v co quote-depth))
    (void)))

(define (do-print who v o [quote-depth PRINT-MODE/UNQUOTED] [max-length #f])
  (define config (make-print-config))
  (dots (p who v quote-depth o (sub3 max-length) (detect-graph v quote-depth config) config) o)
  (void))

(define do-global-print void)

(define (install-do-global-print! param default-value)
  (set! do-global-print
        (lambda (who v o [quote-depth-in PRINT-MODE/UNQUOTED] [max-length #f])
          (define global-print (param))
          (cond
            [(eq? global-print default-value)
             (define quote-depth (if (print-as-expression) quote-depth-in WRITE-MODE))
             (do-print who v o quote-depth max-length)]
            [(not max-length)
             (global-print v o quote-depth-in)]
            [else
             ;; There's currently no way to communicate `max-length`
             ;; to the `global-print` function, but we should only get
             ;; here when `o` is a string port for errors, so write to
             ;; a fresh string port and truncate as needed.
             (define o2 (open-output-bytes))
             (global-print v o2 quote-depth-in)
             (define bstr (get-output-bytes o2))
             (if ((bytes-length bstr) . <= . max-length)
                 (unsafe-write-bytes who bstr o)
                 (begin
                   (unsafe-write-bytes who (subbytes bstr 0 (sub3 max-length)) o)
                   (unsafe-write-bytes who #"..." o)))])
          (void))))

(define/who (newline [o (current-output-port)])
  (unsafe-write-bytes 'newline #"\n" (->core-output-port o who))
  (void))

;; ----------------------------------------

(define (max-length? v)
  (or (not v)
      (and (exact-nonnegative-integer? v)
           (v . >= . 3))))

(define max-length-contract "(or/c #f (and/c exact-integer? (>=/c 3)))")

(define (sub3 n) (and n (- n 3)))

(define (dots max-length o)
  (cond
    [(eq? max-length 'full)
     (write-string "..." o)]
    [(pair? max-length)
     ;; pending bytes fit after all
     (write-bytes (cdr max-length) o)]
    [else (void)]))

;; ----------------------------------------

;; Returns the max length that is still available, where 'full
;; means that more than three items would otherwise have been
;; written, and a pair indicates that some bytes/characters are
;; pending until the rest of the writes are determined
(define (p who v mode o max-length graph config)
  (cond
    [(and graph (hash-ref graph v #f))
     => (lambda (g)
          (cond
            [(and (as-constructor? g)
                  (not (as-constructor-tag g)))
             (p/no-graph-no-quote who v mode o max-length graph config)]
            [(string? g)
             (let* ([max-length (write-string/max "#" o max-length)]
                    [max-length (write-string/max g o max-length)])
               (write-string/max "#" o max-length))]
            [else
             (let* ([gs (number->string (if (as-constructor? g)
                                            (as-constructor-tag g)
                                            g))]
                    [max-length (write-string/max "#" o max-length)]
                    [max-length (write-string/max gs o max-length)]
                    [max-length (write-string/max "=" o max-length)])
               (hash-set! graph v gs)
               (if (as-constructor? g)
                   (p/no-graph-no-quote who v mode o max-length graph config)
                   (p/no-graph who v mode o max-length graph config)))]))]
    [else
     (p/no-graph who v mode o max-length graph config)]))

(define (p/no-graph who v mode o max-length graph config)
  (cond
    [(and (eq? mode PRINT-MODE/UNQUOTED)
          (or (null? v)
              (symbol? v)
              (keyword? v)
              (pair? v)
              (vector? v)
              (box? v)
              (hash? v)
              (prefab-struct-key v)
              (and (custom-write? v)
                   (not (struct-type? v))
                   (not (printable-regexp? v))
                   (not (eq? 'self (custom-print-quotable-accessor v 'self))))))
     ;; Since this value is not marked for constructor mode,
     ;; transition to quote mode:
     (let ([max-length (write-string/max "'" o max-length)])
       (p/no-graph-no-quote who v PRINT-MODE/QUOTED o max-length graph config))]
    [else
     (p/no-graph-no-quote who v mode o max-length graph config)]))

(define (p/no-graph-no-quote who v mode o max-length graph config)
  (cond
    [(eq? max-length 'full) 'full]
    [(null? v)
     (write-string/max "()" o max-length)]
    [(number? v)
     (write-string/max (number->string v) o max-length)]
    [(string? v)
     (cond
       [(eq? mode DISPLAY-MODE) (write-string/max v o max-length)]
       [else (print-string v o max-length)])]
    [(bytes? v)
     (cond
       [(eq? mode DISPLAY-MODE) (write-bytes/max v o max-length)]
       [else (print-bytes v o max-length)])]
    [(symbol? v)
     (cond
       [(eq? mode DISPLAY-MODE) (write-string/max (symbol->immutable-string v) o max-length)]
       [else (print-symbol v o max-length config)])]
    [(keyword? v)
     (let ([max-length (write-string/max "#:" o max-length)])
       (cond
         [(eq? mode DISPLAY-MODE) (write-string/max (keyword->immutable-string v) o max-length)]
         [else
          (print-symbol (string->symbol (keyword->immutable-string v)) o max-length config
                        #:for-keyword? #t)]))]
    [(char? v)
     (cond
       [(eq? mode DISPLAY-MODE) (write-string/max (string v) o max-length)]
       [else (print-char v o max-length)])]
    [(not v)
     (if (config-get config print-boolean-long-form)
         (write-string/max "#false" o max-length)
         (write-string/max "#f" o max-length))]
    [(eq? v #t)
     (if (config-get config print-boolean-long-form)
         (write-string/max "#true" o max-length)
         (write-string/max "#t" o max-length))]
    [(pair? v)
     (print-list p who v mode o max-length graph config #f #f)]
    [(vector? v)
     (print-vector p who v mode o max-length graph config "" vector-length vector-ref eq?)]
    [(flvector? v)
     (print-vector p who v mode o max-length graph config "fl" flvector-length flvector-ref equal?)]
    [(fxvector? v)
     (print-vector p who v mode o max-length graph config "fx" fxvector-length fxvector-ref eq?)]
    [(stencil-vector? v)
     (define lst (let loop ([i 0])
                   (if (= i (stencil-vector-length v))
                       '()
                       (cons (stencil-vector-ref v i) (loop (add1 i))))))
     (print-list p who lst
                 (if (eq? mode DISPLAY-MODE) DISPLAY-MODE WRITE-MODE)
                 o max-length graph config
                 (string-append "#<stencil "
                                (number->string (stencil-vector-mask v))
                                (if (eqv? 0 (stencil-vector-mask v))
                                    ""
                                    ": "))
                 #f
                 ">")]
    [(box? v)
     (cond
       [(config-get config print-box)
        (cond
          [(eq? mode PRINT-MODE/UNQUOTED)
           (let* ([max-length (write-string/max "(box " o max-length)]
                  [max-length (p who (unbox v) mode o max-length graph config)])
             (write-string/max ")" o max-length))]
          [else
           (p who (unbox v) mode o (write-string/max "#&" o max-length) graph config)])]
       [else
        (check-unreadable who config mode v)
        (write-string/max "#<box>" o max-length)])]
    [(hash? v)
     (cond
       [(and (config-get config print-hash-table)
             (not (hash-weak? v)))
        (cond
          [(eq? mode PRINT-MODE/UNQUOTED)
           (define l (apply append (hash-map v list #t)))
           (define prefix (cond
                            [(hash-eq? v) "(hasheq"]
                            [(hash-eqv? v) "(hasheqv"]
                            [(hash-equal-always? v) "(hashalw"]
                            [else "(hash"]))
           (print-list p who l mode o max-length graph config #f prefix)]
          [else
           (print-hash v o max-length p who mode graph config)])]
       [else
        (check-unreadable who config mode v)
        (write-string/max "#<hash>" o max-length)])]
    [(and (eq? mode WRITE-MODE)
          (not (config-get config print-unreadable))
          (not (prefab-struct-key v))
          ;; Regexps are a special case: custom writers that produce readable input
          (not (printable-regexp? v)))
     (fail-unreadable who v)]
    [(mpair? v)
     (print-mlist p who v mode o max-length graph config)]
    [(and (not (struct-type? v))
          (custom-write? v))
     (let ([o/m (make-output-port/max o max-length)])
       (set-port-handlers-to-recur!
        o/m
        (lambda (v o mode)
          (p who v mode o (output-port/max-max-length o/m max-length) graph config)))
       ((custom-write-accessor v) v o/m mode)
       (output-port/max-max-length o/m max-length))]
    [(and (struct? v)
          (config-get config print-struct))
     (cond
       [(eq? mode PRINT-MODE/UNQUOTED)
        (define l (vector->list (struct->vector v struct-dots)))
        (define alt-list-constructor
          ;; strip "struct:" from the first element of `l`:
          (string-append "(" (substring (symbol->immutable-string (car l)) 7)))
        (print-list p who (cdr l) mode o max-length graph config #f alt-list-constructor)]
       [(prefab-struct-key v)
        => (lambda (key)
             (define l (cons key (cdr (vector->list (struct->vector v)))))
             (print-list p who l mode o max-length graph config "#s(" #f))]
       [else
        (p who (struct->vector v) mode o max-length graph config)])]
    [(procedure? v)
     (print-named "procedure" v mode o max-length)]
    [(struct-type? v)
     (print-named "struct-type" v mode o max-length)]
    [(struct-type-property? v)
     (print-named "struct-type-property" v mode o max-length)]
    [(thread? v)
     (print-named "thread" v mode o max-length)]
    [(eof-object? v)
     (write-string/max "#<eof>" o max-length)]
    [(core-input-port? v)
     (print-named "input-port" v mode o max-length)]
    [(core-output-port? v)
     (print-named "output-port" v mode o max-length)]
    [(continuation-prompt-tag? v)
     (print-named "continuation-prompt-tag" v mode o max-length)]
    [(unquoted-printing-string? v)
     (write-string/max (unquoted-printing-string-value v) o max-length)]
    [(eq? v unsafe-undefined)
     (write-string/max "#<unsafe-undefined>" o max-length)]
    [else
     ;; As a last resort, fall back to the host `format`:
     (write-string/max (format "~s" v) o max-length)]))

(define (fail-unreadable who v)
  (raise (exn:fail
          (error-message->string
           who
           (string-append "printing disabled for unreadable value"
                          "\n  value: "
                          (parameterize ([print-unreadable #t])
                            ((error-value->string-handler) v (error-print-width)))))
          (current-continuation-marks))))

(define (check-unreadable who config mode v)
  (when (and (eq? mode WRITE-MODE)
             (not (config-get config print-unreadable)))
    (fail-unreadable who v)))

(define struct-dots (unquoted-printing-string "..."))
