#lang racket/base
(require "../common/check.rkt"
         "input-port.rkt"
         "bytes-input.rkt"
         "string-input.rkt"
         "parameter.rkt"
         "flush-output.rkt")

(provide read-bytes-line
         read-line)

(define (ok-mode? v)
  (memq v '(linefeed return return-linefeed any any-one)))
(define ok-mode-str "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)")

(define-syntax-rule (define-read-line read-line
                      make-string string-length string-set! 
                      string-copy! substring
                      read-char peek-char
                      as-char)
  (define/who (read-line [in (current-input-port)] [mode 'linefeed])
    (check who input-port? in)
    (check who ok-mode? #:contract ok-mode-str mode)
    (maybe-flush-stdout in)
    (define cr? (memq mode '(return any any-one)))
    (define lf? (memq mode '(linefeed any any-one)))
    (define crlf? (memq mode '(return-linefeed any)))
    (let loop ([str (make-string 32)] [pos 0])
      (define ch (read-char in))
      (define (keep-char)
        (if (pos . < . (string-length str))
            (begin
              (string-set! str pos ch)
              (loop str (add1 pos)))
            (let ([new-str (make-string (* (string-length str) 2))])
              (string-copy! new-str 0 str 0)
              (string-set! new-str pos ch)
              (loop new-str (add1 pos)))))
      (cond
       [(eof-object? ch)
        (if (zero? pos)
            eof
            (substring str 0 pos))]
       [(and (or cr? crlf?)
             (eqv? ch (as-char #\return)))
        (cond
         [(and crlf?
               (eqv? (peek-char in) (as-char #\linefeed)))
          (read-char in)
          (substring str 0 pos)]
         [cr?
          (substring str 0 pos)]
         [else (keep-char)])]
       [(and lf?
             (eqv? ch (as-char #\newline)))
        (substring str 0 pos)]
       [else (keep-char)]))))

(define-read-line read-line
  make-string string-length string-set! 
  string-copy! substring
  read-char peek-char
  values)

 (define-read-line read-bytes-line
   make-bytes bytes-length bytes-set! 
   bytes-copy! subbytes
   read-byte peek-byte
   char->integer)
