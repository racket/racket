#lang racket/base

#| Based on the PLaneT package by Dave Herman,
   Originally released under MIT license.
|#

(require (only-in scheme/base [read scheme:read] [write scheme:write]))
(provide read-json write-json jsexpr->json json->jsexpr jsexpr?)

(define (write-json json [port (current-output-port)])
  (cond
    [(hash? json)
     (display "{" port)
     (for ([(key value) json]
           [i (in-naturals)])
       (when (> i 0)
         (display ", " port))
       (fprintf port "\"~a\"" key)
       (display ": " port)
       (write-json value port))
     (display "}" port)]
    [(list? json)
     (display "[" port)
     (for ([(value i) (in-indexed json)])
       (when (> i 0)
         (display ", " port))
       (write-json value port))
     (display "]" port)]
    [(or (string? json) (and (number? json) (or (integer? json) (inexact? json))))
     (scheme:write json port)]
    [(boolean? json) (scheme:write (if json 'true 'false) port)]
    [(null-jsexpr? json) (scheme:write 'null port)]
    [else (error 'json "bad json value: ~v" json)]))

(define (read-json [port (current-input-port)])
  (skip-whitespace port)
  (case (peek-char port)
    [(#\{) (read/hash port)]
    [(#\[) (read/list port)]
    [(#\") (read/string port)]
    [(#\t) (read/true port)]
    [(#\f) (read/false port)]
    [(#\n) (read/null port)]
    [else (read/number port)]))

(define (expect ch . expected)
  (unless (memq ch expected)
    (error 'read "expected: ~v, got: ~a" expected ch))
  ch)

(define (expect-string port expected)
  (list->string (for/list ([ch expected])
                  (expect (read-char port) ch))))

(define (skip-whitespace port)
  (let ([ch (peek-char port)])
    (when (char-whitespace? ch)
      (read-char port)
      (skip-whitespace port))))

(define (in-port-until port reader done?)
  (make-do-sequence (lambda ()
                      (values reader
                              (lambda (port) port)
                              port
                              (lambda (port)
                                (not (done? port)))
                              (lambda values #t)
                              (lambda (port . values) #t)))))

(define (read/hash port)
  (expect (read-char port) #\{)
  (skip-whitespace port)
  (begin0 (for/hasheq ([(key value)
                        (in-port-until port
                                       (lambda (port)
                                         (let ([key (read/string port)])
                                           (unless (string? key)
                                             (error 'read "expected: string, got: ~v" key))
                                           (skip-whitespace port)
                                           (expect (read-char port) #\:)
                                           (skip-whitespace port)
                                           (let ([value (read-json port)])
                                             (skip-whitespace port)
                                             (expect (peek-char port) #\, #\})
                                             (values (string->symbol key) value))))
                                       (lambda (port)
                                         (eq? (peek-char port) #\})))])
            (when (eq? (peek-char port) #\,)
              (read-char port))
            (skip-whitespace port)
            (values key value))
          (expect (read-char port) #\})))

(define (read/list port)
  (expect (read-char port) #\[)
  (begin0 (for/list ([value
                      (in-port-until port
                                     (lambda (port)
                                       (skip-whitespace port)
                                       (begin0 (read-json port)
                                               (skip-whitespace port)
                                               (expect (peek-char port) #\, #\])))
                                     (lambda (port)
                                       (eq? (peek-char port) #\])))])
            (when (eq? (peek-char port) #\,)
              (read-char port))
            value)
          (expect (read-char port) #\])))

(define (read/string port)
  (expect (read-char port) #\")
  (begin0 (list->string
           (for/list ([ch (in-port-until port
                                         (lambda (port)
                                           (let ([ch (read-char port)])
                                             (when (eof-object? ch)
                                               (error 'read "unexpected EOF"))
                                             (if (eq? ch #\\)
                                                 (let ([esc (read-char port)])
                                                   (when (eof-object? ch)
                                                     (error 'read "unexpected EOF"))
                                                   (case esc
                                                     [(#\b) #\backspace]
                                                     [(#\n) #\newline]
                                                     [(#\r) #\return]
                                                     [(#\f) #\page]
                                                     [(#\t) #\tab]
                                                     [(#\\) #\\]
                                                     [(#\") #\"]
                                                     [(#\/) #\/]
                                                     [(#\u) (unescape (read-string 4 port))]
                                                     [else esc]))
                                                 ch)))
                                         (lambda (port)
                                           (eq? (peek-char port) #\")))])
             ch))
          (expect (read-char port) #\")))

(define (unescape str)
  (unless (regexp-match #px"[a-fA-F0-9]{4}" str)
    (error 'read "bad unicode escape sequence: \"\\u~a\"" str))
  (integer->char (string->number str 16)))

(define (read/true port)
  (expect-string port "true")
  #t)

(define (read/false port)
  (expect-string port "false")
  #f)

(define (read/null port)
  (expect-string port "null")
  null-jsexpr)

(define (read/digits port)
  (let ([digits (for/list ([digit (in-port-until port
                                                 read-char
                                                 (lambda (port)
                                                   (let ([ch (peek-char port)])
                                                     (or (eof-object? ch)
                                                         (not (char-numeric? ch))))))])
                  digit)])
    (when (and (null? digits) (eof-object? (peek-char port)))
      (error 'read "unexpected EOF"))
    (when (null? digits)
      (error 'read "expected: digits, got: ~a" (peek-char port)))
    digits))

(define (read/exponent port)
  (let ([sign (case (peek-char port)
                [(#\- #\+) (list (read-char port))]
                [else '()])])
    (append sign (read/digits port))))

(define (read/number port)
  (let* ([sign (if (eq? (peek-char port) #\-) (list (read-char port)) '())]
         [digits (read/digits port)]
         [frac (if (eq? (peek-char port) #\.)
                   (list* (read-char port) (read/digits port))
                   '())]
         [exp (if (memq (peek-char port) '(#\e #\E))
                  (list* (read-char port) (read/exponent port))
                  '())])
    (string->number
     (list->string
      (append sign digits frac exp)))))

(define (jsexpr? x)
  (or (integer? x)
      (and (number? x) (inexact? x))
      (null-jsexpr? x)
      (boolean? x)
      (string? x)
      (null? x)
      (array-jsexpr? x)
      (object-jsexpr? x)))

(define (array-jsexpr? x)
  (or (null? x)
      (and (pair? x)
           (jsexpr? (car x))
           (array-jsexpr? (cdr x)))))

(define (object-jsexpr? x)
  (let/ec return
    (and (hash? x)
         (for ([(key value) x])
           (unless (and (symbol? key) (jsexpr? value))
             (return #f)))
         #t)))

(define (null-jsexpr? x)
  (eqv? x #\null))

(define null-jsexpr #\null)

(define (jsexpr->json x)
  (let ([out (open-output-string)])
    (write-json x out)
    (get-output-string out)))

(define (json->jsexpr s)
  (let ([in (open-input-string s)])
    (read-json in)))
