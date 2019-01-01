#lang racket/base
(require "../port/string-output.rkt"
         "../string/number.rkt"
         "write-with-max.rkt"
         "parameter.rkt"
         "config.rkt")

(provide print-symbol
         symbol->print-string)

(define (print-symbol sym o max-length config
                      #:for-keyword? [for-keyword? #f])
  (define str (symbol->print-string sym #:config config #:for-keyword? for-keyword?))
  (write-string/max str o max-length))

(define (symbol->print-string sym
                              #:config [config #f]
                              #:for-type? [for-type? #f]
                              #:case-sensitive? [case-sensitive? (if config
                                                                     (config-get config read-case-sensitive)
                                                                     #t)]
                              #:for-keyword? [for-keyword? #f])
  (define str (symbol->string sym))
  (define (is-simple? ch i)
    (not (or (char=? ch #\()
             (char=? ch #\[)
             (char=? ch #\{)
             (char=? ch #\))
             (char=? ch #\])
             (char=? ch #\})
             (char=? ch #\")
             (char=? ch #\\)
             (char=? ch #\')
             (char=? ch #\,)
             (and (char=? ch #\|)
                  (or (not config) (config-get config read-accept-bar-quote)))
             (and for-type?
                  (or (char=? ch #\<)
                      (char=? ch #\>)))
             (and (char-whitespace? ch)
                  (or (not for-type?)
                      (not (char=? ch #\space))))
             (and (char=? ch #\#)
                  (zero? i)
                  (or ((string-length str) . < . 2)
                      (not (char=? (string-ref str 1) #\%))))
             (and (char=? ch #\.)
                  (zero? i)
                  (= (string-length str) 1))
             (and (not case-sensitive?)
                  (not (char=? ch (char-foldcase ch)))))))
  (cond
    [(for/and ([ch (in-string str)]
               [i (in-naturals)])
       (is-simple? ch i))
     (cond
       [(or for-keyword?
            for-type?
            (and (not (string->number? str))
                 (not (string=? str ""))))
        str]
       ;; Remaining two cases add some form of quoting to
       ;; protect against a symbol looking like a number
       [(and config (not (config-get config read-accept-bar-quote)))
        (cond
          [(string=? str "")
           ;; There's no good answer in this case. Traditionally, Racket
           ;; just prints an empty string, anyway.
           str]
          [else
           (string-append "\\" str)])]
       [else
        (string-append "|" str "|")])]
    [(or (and config (not (config-get config read-accept-bar-quote)))
         (for/or ([ch (in-string str)])
           (char=? ch #\|)))
     ;; Need to use backslashes for quoting
     (define len (string-length str))
     (apply
      string-append
      (let loop ([start 0] [i 0])
        (cond
          [(= i len) (list (substring str start len))]
          [(is-simple? (string-ref str i) i) (loop start (add1 i))]
          [else
           (list* (substring str start i)
                  "\\"
                  (substring str i (add1 i))
                  (loop (add1 i) (add1 i)))])))]
    [else
     ;; Can use bars for quoting:
     (string-append "|" str "|")]))
