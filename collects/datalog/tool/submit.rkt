#lang racket/base

(define (delimiter-pair? x y)
  (and (char=? x #\() (char=? y #\))))

(define (repl-submit? ip has-white-space?)
  (let loop ([blank? #t]
             [string-char #f]
             [delimiter-stack null]
             [closed? #f])
    (let ([c (read-char ip)])
      (if (eof-object? c)
          (and closed?
               (not blank?)
               (not string-char)
               (null? delimiter-stack))
          (case c
            [(#\. #\? #\~)
             (if string-char
                 (loop #f string-char delimiter-stack #f)
                 (loop #f #f delimiter-stack #t))]
            [(#\()
             (if string-char
                 (loop #f string-char delimiter-stack #f)
                 (loop #f #f (cons c delimiter-stack) #f))]
            [(#\))
             (cond
               [string-char
                (loop #f string-char delimiter-stack #f)]
               [(and (pair? delimiter-stack)
                     (delimiter-pair? (car delimiter-stack) c))
                (loop #f #f (cdr delimiter-stack) #f)]
               [else
                (loop #f #f delimiter-stack #f)])]
            [(#\")
             (cond
               [(and string-char (char=? c string-char))
                (loop #f #f delimiter-stack #f)]
               [string-char
                (loop #f string-char delimiter-stack #f)]
               [else
                (loop #f c delimiter-stack #f)])]
            [(#\\)
             (if string-char
                 (begin (read-char ip)
                        (loop #f string-char delimiter-stack #f))
                 (loop #f string-char delimiter-stack #f))]
            [else
             (loop (and blank? (char-whitespace? c))
                   string-char
                   delimiter-stack
                   closed?)])))))

(provide repl-submit?)
