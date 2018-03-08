#lang racket/base
(require "config.rkt"
         "special.rkt"
         "wrap.rkt"
         "readtable.rkt"
         "consume.rkt"
         "accum-string.rkt"
         "error.rkt"
         "digit.rkt")

(provide read-string
         read-here-string)

(define (read-string in config #:mode [mode 'string])
  (define source (read-config-source config))
  (define-values (open-end-line open-end-col open-end-pos) (port-next-location in))
  (define accum-str (accum-string-init! config))
  (define (bad-end c)
    (cond
     [(eof-object? c)
      (reader-error in config #:due-to c #:end-pos open-end-pos "expected a closing `\"`")]
     [else
      (reader-error in config #:due-to c
                    "found non-character while reading a ~a"
                    mode)]))
  (let loop ()
    (define c (read-char/special in config source))
    ;; Note: readtable is not used for a closing " or other string decisions
    (cond
     [(not (char? c))
      (bad-end c)]
     [(char=? #\\ c)
      (define escaping-c c)
      (define escaped-c (read-char/special in config source))
      (when (not (char? escaped-c))
        (bad-end escaped-c))
      (define (unknown-error)
        (reader-error in config
                      "unknown escape sequence `~a~a` in ~a"
                      escaping-c escaped-c
                      mode))
      (case escaped-c
        [(#\\ #\" #\')
         (accum-string-add! accum-str escaped-c)]
        [(#\a) (accum-string-add! accum-str #\u7)]
        [(#\b) (accum-string-add! accum-str #\backspace)]
        [(#\t) (accum-string-add! accum-str #\tab)]
        [(#\n) (accum-string-add! accum-str #\newline)]
        [(#\v) (accum-string-add! accum-str #\vtab)]
        [(#\f) (accum-string-add! accum-str #\page)]
        [(#\r) (accum-string-add! accum-str #\return)]
        [(#\e) (accum-string-add! accum-str #\u1B)]
        [(#\newline) (void)]
        [(#\return)
         (define maybe-newline-c (peek-char/special in config 0 source))
         (when (eqv? maybe-newline-c #\newline)
           (consume-char in maybe-newline-c))
         (void)]
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
         ;; Octal (valid if <= 255)
         (define pos (accum-string-count accum-str))
         (accum-string-add! accum-str escaped-c)
         (define init-v (digit->number escaped-c))
         (define v (read-digits in config accum-str #:base 8 #:max-count 2
                                #:init init-v
                                #:zero-digits-result init-v))
         (unless (v . <= . 255)
           (reader-error in config
                         "escape sequence `~a~a` is out of range in ~a"
                         escaping-c (accum-string-get! accum-str config #:start-pos pos)
                         mode))
         (set-accum-string-count! accum-str pos)
         (accum-string-add! accum-str (integer->char v))]
        [(#\x)
         ;; Hex, two characters (always valid)
         (define pos (accum-string-count accum-str))
         (define v (read-digits in config accum-str #:base 16 #:max-count 2))
         (unless (integer? v) (no-hex-digits in config v escaping-c escaped-c))
         (set-accum-string-count! accum-str pos)
         (accum-string-add! accum-str (integer->char v))]
        [(#\u)
         ;; Hex, four characters (valid if not surrogate or if surrogate pair)
         (unless (eq? mode 'string) (unknown-error))
         (define pos (accum-string-count accum-str))
         (define v (read-digits in config accum-str #:base 16 #:max-count 4))
         (unless (integer? v) (no-hex-digits in config v escaping-c escaped-c))
         (cond
          [(or (v . < . #xD800) (v . > . #xDFFF))
           ;; Normal \u escape
           (set-accum-string-count! accum-str pos)
           (accum-string-add! accum-str (integer->char v))]
          [else
           ;; Maybe a surrogate-pair encoding
           (define (next!)
             (define next-c (read-char/special in config source))
             (when (char? next-c)
               (accum-string-add! accum-str next-c))
             next-c)
           (define v2
             (let ([next-c (next!)])
               (cond
                [(char=? next-c #\\)
                 (define next-c (next!))
                 (cond
                  [(char=? next-c #\u)
                   (define v2 (read-digits in config accum-str #:base 16 #:max-count 4))
                   (cond
                    [(integer? v2)
                     (and (v2 . >= . #xDC00)
                          (v2 . <= . #xDFFF)
                          v2)]
                    [else v2])]    ; maybe EOF
                  [else next-c])]  ; maybe EOF
                [else next-c]))) ; maybe EOF
           (cond
            [(integer? v2)
             (define combined-v (+ (arithmetic-shift (- v #xD800) 10)
                                   (- v2 #xDC00)
                                   #x10000))
             (cond
              [(combined-v . > . #x10FFFF)
               (reader-error in config
                             "escape sequence `~au~a` is out of range in string"
                             escaping-c (accum-string-get! accum-str config #:start-pos pos))]
              [else
               (set-accum-string-count! accum-str pos)
               (accum-string-add! accum-str (integer->char combined-v))])]
            [else
             (reader-error in config
                           #:due-to v2
                           "bad or incomplete surrogate-style encoding at `~au~a`"
                           escaping-c (accum-string-get! accum-str config #:start-pos pos))])])]
        [(#\U)
         (unless (eq? mode 'string) (unknown-error))
         (define pos (accum-string-count accum-str))
         (define v (read-digits in config accum-str #:base 16 #:max-count 8))
         (unless (integer? v) (no-hex-digits in config v escaping-c escaped-c))
         (cond
          [(and (or (v . < . #xD800) (v . > . #xDFFF))
                (v . <= . #x10FFFF))
           (set-accum-string-count! accum-str pos)
           (accum-string-add! accum-str (integer->char v))]
          [else
           (reader-error in config
                         "escape sequence `~aU~a` is out of range in string"
                         escaping-c (accum-string-get! accum-str config #:start-pos pos))])]
        [else (unknown-error)])
      (loop)]
     [(char=? #\" c)
      null]
     [else
      (when (eq? mode '|byte string|)
        (unless (byte? (char->integer c))
          (reader-error in config
                        "character `~a` is out of range in byte string"
                        c)))
      (accum-string-add! accum-str c)
      (loop)]))
  (define str (if (eq? mode '|byte string|)
                  (accum-string-get-bytes! accum-str config)
                  (accum-string-get! accum-str config)))
  (wrap str
        in
        config
        str))

;; ----------------------------------------

(define (read-here-string in config)
  (define source (read-config-source config))
  (define-values (open-end-line open-end-col open-end-pos) (port-next-location in))
  (define accum-str (accum-string-init! config))
  
  ;; Parse terminator
  (define full-terminator
    (cons
     #\newline ;; assumption below that this character is first
     (let loop ()
       (define c (read-char/special in config source))
       (cond
         [(eof-object? c)
          (reader-error in config #:due-to c
                        "found end-of-file after `#<<` and before a newline")]
         [(not (char? c))
          (reader-error in config #:due-to c
                        "found non-character while reading `#<<`")]
         [(char=? c #\newline) null]
         [else (cons c (loop))]))))
  
  ;; Get string content.
  ;; We just saw a newline that could be considered the start of an
  ;; immediate `full-terminator`.
  (let loop ([terminator (cdr full-terminator)] [terminator-accum null])
    (define c (read-char/special in config source))
    (cond
     [(eof-object? c)
      (unless (null? terminator)
        (reader-error in config #:due-to c #:end-pos open-end-pos
                      "found end-of-file before terminating `~a`"
                      (list->string (cdr full-terminator))))]
     [(not (char? c))
      (reader-error in config #:due-to c
                    "found non-character while reading `#<<`")]
     [(and (pair? terminator)
           (char=? c (car terminator)))
      (loop (cdr terminator) (cons (car terminator) terminator-accum))]
     [(and (null? terminator)
           (char=? c #\newline))
      (void)]
     [else
      (unless (null? terminator-accum)
        (for ([c (in-list (reverse terminator-accum))])
          (accum-string-add! accum-str c)))
      (cond
        [(char=? c #\newline)
         ;; assume `full-terminator` starts with #\newline
         (loop (cdr full-terminator) (list #\newline))]
        [else
         (accum-string-add! accum-str c)
         (loop full-terminator null)])]))

  ;; Done
  (define str (accum-string-get! accum-str config))
  (wrap str
        in
        config
        str))

;; ----------------------------------------

(define (no-hex-digits in config c escaping-c escaped-c)
  (reader-error in config
                #:due-to c
                "no hex digit following `~a~a`"
                escaping-c escaped-c))
