#lang racket/base
(require "../common/parameter-like.rkt"
         "config.rkt"
         "special.rkt"
         "consume.rkt"
         "error.rkt"
         "accum-string.rkt"
         "parameter.rkt"
         "wrap.rkt"
         "coerce.rkt"
         "special-comment.rkt")

(provide read-extension-reader
         read-extension-lang
         read-extension-#!)

(define (read-extension-reader read-one read-recur dispatch-c in config)
  (define extend-str (read-extension-prefix (cons dispatch-c '(#\r #\e))
                                            '(#\a #\d #\e #\r)
                                            in
                                            config))
  (unless (check-parameter read-accept-reader config)
    (reader-error in config
                  "`~a` not enabled"
                  extend-str))

  (define mod-path-wrapped (read-one #f in (next-readtable config)))
  (when (eof-object? mod-path-wrapped)
    (reader-error in config #:due-to mod-path-wrapped
                  "expected a datum after `~a`, found end-of-file"
                  extend-str))
  
  (read-extension ((read-config-coerce config) #f mod-path-wrapped #f)
                  read-recur in config
                  #:mod-path-wrapped mod-path-wrapped))

;; ----------------------------------------

(define (read-extension-lang read-recur dispatch-c in config
                             #:get-info? [get-info? #f])
  (define extend-str (read-extension-prefix (cons dispatch-c '(#\l))
                                            '(#\a #\n #\g)
                                            in
                                            config))
  (define c (read-char/special in config))
  (unless (char=? c #\space)
    (reader-error in config
                  "expected a single space after `~a`"
                  extend-str))
  
  (read-lang extend-str read-recur in config
             #:who '|#lang|
             #:get-info? get-info?))

(define (read-extension-#! read-recur dispatch-c in config
                           #:get-info? [get-info? #f])
  (define c (read-char/special in config))
  (unless (char-lang-nonsep? c)
    (bad-syntax-error in config (if (char? c)
                                    (string dispatch-c #\! c)
                                    (string dispatch-c #\!))))
  (read-lang (string dispatch-c #\!) read-recur in config
             #:init-c c
             #:who '|#!|
             #:get-info? get-info?))

;; ----------------------------------------

(define (read-lang extend-str read-recur in config
                   #:init-c [init-c #f]
                   #:get-info? [get-info? #f]
                   #:who who)
  (unless (and (check-parameter read-accept-reader config)
               (check-parameter read-accept-lang config))
    (reader-error in config
                  "`~a` not enabled"
                  extend-str))

  (define-values (line col pos) (port-next-location in))
  
  (define accum-str (accum-string-init! config))
  (when init-c
    (accum-string-add! accum-str init-c))
  (let loop ()
    (define c (peek-char/special in config))
    (cond
     [(eof-object? c) (void)]
     [(not (char? c))
      (consume-char/special in config c)
      (reader-error in config #:due-to c
                    "found non-character while reading `#~a`"
                    extend-str)]
     [(char-whitespace? c) (void)]
     [(or (char-lang-nonsep? c)
          (char=? #\/ c))
      (consume-char in c)
      (accum-string-add! accum-str c)
      (loop)]
     [else
      (consume-char in c)
      (reader-error in config
                    (string-append "expected only alphanumeric, `-`, `+`, `_`, or `/`"
                                   " characters for `~a`, found `~a`")
                    extend-str
                    c)]))

  (define lang-str (accum-string-get! accum-str config))
  (when (equal? lang-str "")
    (reader-error in config
                  "expected a non-empty sequence of alphanumeric, `-`, `+`, `_`, or `/` after `~a`"
                  extend-str))

  (when (char=? #\/ (string-ref lang-str 0))
    (reader-error in config
                  "expected a name that does not start `/` after `~a`"
                  extend-str))

  (when (char=? #\/ (string-ref lang-str (sub1 (string-length lang-str))))
    (reader-error in config
                  "expected a name that does not end `/` after `~a`"
                  extend-str))
  
  (define submod-path `(submod ,(string->symbol lang-str) reader))
  (define reader-path (string->symbol (string-append lang-str "/lang/reader")))

  (read-extension #:try-first-mod-path submod-path
                  reader-path read-recur in (reading-at config line col pos)
                  #:get-info? get-info?
                  #:who who))

(define (char-lang-nonsep? c)
  (and ((char->integer c) . < . 128)
       (or (char-alphabetic? c)
           (char-numeric? c)
           (char=? #\- c)
           (char=? #\+ c)
           (char=? #\_ c))))

;; ----------------------------------------

(define (read-extension-prefix already wanted in config)
  (define accum-str (accum-string-init! config))
  (for ([c (in-list already)])
    (accum-string-add! accum-str c))
  (let loop ([wanted wanted])
    (unless (null? wanted)
      (define c (read-char/special in config))
      (when (char? c)
        (accum-string-add! accum-str c))
      (unless (eqv? c (car wanted))
        (bad-syntax-error in config (accum-string-get! accum-str config)
                          #:due-to c))
      (loop (cdr wanted))))
  (accum-string-get! accum-str config))

;; ----------------------------------------

(define (read-extension #:try-first-mod-path [try-first-mod-path #f]
                        mod-path-datum read-recur in config
                        #:mod-path-wrapped [mod-path-wrapped
                                            ((read-config-coerce config)
                                             #t
                                             mod-path-datum
                                             (port+config->srcloc in config))]
                        #:get-info? [get-info? #f]
                        #:who [who '|#reader|])
  (force-parameters! config)
  (define guard (current-reader-guard))
  (define mod-path
    (or (and try-first-mod-path
             (let ([mod-path (guard try-first-mod-path)])
               (and ((read-config-module-declared? config) try-first-mod-path)
                    mod-path)))
        (guard mod-path-datum)))
  
  ((read-config-call-with-root-namespace config)
   (lambda ()
     (define for-syntax? (read-config-for-syntax? config))

     (define dynamic-require (read-config-dynamic-require config))

     (define no-value (gensym))

     (define extension
       (cond
         [get-info?
          (dynamic-require mod-path 'get-info (lambda () no-value))]
         [else
          (dynamic-require mod-path (if for-syntax? 'read-syntax 'read))]))

     (cond
       [(eq? extension no-value)
        ;; Only for `get-info?` mode:
        #f]
       [else
        (define result-v
          (cond
            [(and for-syntax? (not get-info?))
             (cond
               [(procedure-arity-includes? extension 6)
                (parameterize-like
                 #:with ([current-read-config config])
                 (extension (read-config-source config)
                            in
                            mod-path-wrapped
                            (read-config-line config)
                            (read-config-col config)
                            (read-config-pos config)))]
               [(procedure-arity-includes? extension 2)
                (parameterize-like
                 #:with ([current-read-config config])
                 (extension (read-config-source config) in))]
               [else
                (raise-argument-error who
                                      "(or/c (procedure-arity-includes?/c 2) (procedure-arity-includes?/c 6))"
                                      extension)])]
            [else
             (cond
               [(procedure-arity-includes? extension 5)
                (parameterize-like
                 #:with ([current-read-config config])
                 (extension in
                            mod-path-wrapped
                            (read-config-line config)
                            (read-config-col config)
                            (read-config-pos config)))]
               [get-info?
                (raise-argument-error who
                                      "(procedure-arity-includes?/c 5)"
                                      extension)]
               [(procedure-arity-includes? extension 1)
                (parameterize-like
                 #:with ([current-read-config config])
                 (extension in))]
               [else
                (raise-argument-error who
                                      "(or/c (procedure-arity-includes?/c 1) (procedure-arity-includes?/c 5))"
                                      extension)])]))

        (cond
          [get-info?
           (unless (and (procedure? result-v) (procedure-arity-includes? result-v 2))
             (raise-result-error 'read-language "(procedure-arity-includes?/c 2)" result-v))
           result-v]
          [(special-comment? result-v)
           (read-recur #f in config)]
          [else
           (coerce result-v in config)])]))))
