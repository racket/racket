#lang racket/base
(require "config.rkt"
         "special.rkt"
         "readtable.rkt"
         "whitespace.rkt"
         "delimiter.rkt"
         "consume.rkt"
         "location.rkt"
         "error.rkt"
         "accum-string.rkt"
         "indentation.rkt"
         "closer.rkt"
         "parameter.rkt"
         "coerce-key.rkt"
         "wrap.rkt"
         "sequence.rkt"
         "special-comment.rkt")

(provide read-hash)

;; `#` and `h` or `H` have been read
(define (read-hash read-one dispatch-c init-c in config)
  (define accum-str (accum-string-init! config))
  (accum-string-add! accum-str dispatch-c)
  (accum-string-add! accum-str init-c)
  
  (define (get-next! expect-c expect-alt-c)
    (define c (read-char/special in config))
    (unless (or (eqv? c expect-c) (eqv? c expect-alt-c))
      (reader-error in config #:due-to c
                    "expected `~a` after `~a`"
                    expect-c (accum-string-get! accum-str config)))
    (accum-string-add! accum-str c))
  
  (get-next! #\a #\A)
  (get-next! #\s #\S)
  (get-next! #\h #\H)

  (define-values (content opener mode)
    (let loop ([mode 'equal])
      (define c (read-char/special in config))
      (define ec (effective-char c config))
      (case ec
        [(#\()
         (define-values (open-end-line open-end-col open-end-pos) (port-next-location in))
         (define read-one-key+value (make-read-one-key+value read-one c #\) open-end-pos))
         (values (read-unwrapped-sequence read-one-key+value c #\( #\) in config
                                          #:elem-config config
                                          #:dot-mode #f)
                 ec
                 mode)]
        [(#\[)
         (cond
           [(check-parameter read-square-bracket-as-paren config)
            (define-values (open-end-line open-end-col open-end-pos) (port-next-location in))
            (define read-one-key+value (make-read-one-key+value read-one c #\] open-end-pos))
            (values (read-unwrapped-sequence read-one-key+value c #\[ #\] in config
                                             #:elem-config config
                                             #:dot-mode #f)
                    ec
                    mode)]
           [else
            (reader-error in config "illegal use of `~a`" c)])]
        [(#\{)
         (cond
           [(check-parameter read-curly-brace-as-paren config)
            (define-values (open-end-line open-end-col open-end-pos) (port-next-location in))
            (define read-one-key+value (make-read-one-key+value read-one c #\} open-end-pos))
            (values (read-unwrapped-sequence read-one-key+value c #\{ #\} in config
                                             #:elem-config config
                                             #:dot-mode #f)
                    ec
                    mode)]
           [else
            (reader-error in config "illegal use of `~a`" c)])]
        [(#\e #\E)
         (accum-string-add! accum-str c)
         (get-next! #\q #\Q)
         (loop 'eq)]
        [(#\v #\V)
         (accum-string-add! accum-str c)
         (if (eq? mode 'eq)
             (loop 'eqv)
             (reader-error in config
                           "bad syntax `~a`"
                           (accum-string-get! accum-str config)))]
        [(#\a #\A)
         (accum-string-add! accum-str c)
         (get-next! #\l #\L)
         (get-next! #\w #\W)
         (if (eq? mode 'equal)
             (loop 'equal-always)
             (reader-error in config
                           "bad syntax `~a`"
                           (accum-string-get! accum-str config)))]
        [else
         (when (char? c)
           (accum-string-add! accum-str c))
         (reader-error in config #:due-to c
                       "bad syntax `~a`"
                       (accum-string-get! accum-str config))])))
  
  (define graph? (and (read-config-state-graph
                       (read-config-st config))
                      #t))
  
  (wrap (case mode
          [(equal)
           (if graph?
               (make-hash-placeholder content)
               (make-immutable-hash content))]
          [(eq)
           (if graph?
               (make-hasheq-placeholder content)
               (make-immutable-hasheq content))]
          [(eqv)
           (if graph?
               (make-hasheqv-placeholder content)
               (make-immutable-hasheqv content))]
          [(equal-always)
           (if graph?
               (make-hashalw-placeholder content)
               (make-immutable-hashalw content))])
        in
        config
        opener))

;; ----------------------------------------

(define ((make-read-one-key+value read-one overall-opener-c overall-closer-ec prefix-end-pos) init-c in config/maybe-keep-comment)
  (define config (discard-comment config/maybe-keep-comment))
  (define c (read-char/skip-whitespace-and-comments init-c read-one in config))
  (define-values (open-line open-col open-pos) (port-next-location* in c))
  (define ec (effective-char c config))
  (define elem-config (next-readtable config))
  
  (define closer
    (case ec
      [(#\() #\)]
      [(#\[) (and (check-parameter read-square-bracket-as-paren config)
                  #\])]
      [(#\{) (and (check-parameter read-curly-brace-as-paren config)
                  #\})]
      [else #f]))
  
  (cond
   [(not closer)
    (cond
     [(eof-object? c)
      (reader-error in config
                    #:due-to c #:end-pos prefix-end-pos
                    "expected ~a to close `~a`"
                    (closer-name overall-closer-ec config) overall-opener-c)]
     [(char-closer? ec config)
      (reader-error in (reading-at config open-line open-col open-pos)
                    "~a"
                    (indentation-unexpected-closer-message ec c config))]
     [(special-comment-via-readtable? c read-one in elem-config)
      => (lambda (v)
           (if (read-config-keep-comment? config/maybe-keep-comment)
               v
               ;; Try again
               ((make-read-one-key+value read-one overall-opener-c overall-closer-ec prefix-end-pos) #f in config)))]
     [else
      (reader-error in (reading-at config open-line open-col open-pos)
                    "expected ~a to start a hash pair"
                    (all-openers-str config))])]
   [else
    (define k (read-one #f in (disable-wrapping elem-config)))

    (let dot-loop ()
      (define dot-c (read-char/skip-whitespace-and-comments #f read-one in config))
      (define-values (dot-line dot-col dot-pos) (port-next-location* in dot-c))
      (define dot-ec (effective-char dot-c config))
      
      (cond
        [(and (eqv? dot-ec #\.)
              (char-delimiter? (peek-char/special in config) config))
         (define v (read-one #f in elem-config))

         (let closer-loop ()
           (define closer-c (read-char/skip-whitespace-and-comments #f read-one in config))
           (define-values (closer-line closer-col closer-pos) (port-next-location* in closer-c))
           (define closer-ec (effective-char closer-c config))

           (cond
             [(eqv? closer-ec closer)
              (cons (coerce-key k elem-config) v)]
             [(special-comment-via-readtable? closer-c read-one in config)
              (closer-loop)]
             [else
              (reader-error in (reading-at config closer-line closer-col closer-pos)
                            #:due-to closer-c
                            "expected ~a after value within a hash"
                            (closer-name closer config))]))]
        [(special-comment-via-readtable? dot-c read-one in config)
         (dot-loop)]
        [else
         (reader-error in (reading-at config dot-line dot-col dot-pos)
                       #:due-to dot-c
                       "expected ~a and value for hash"
                       (dot-name config))]))]))
