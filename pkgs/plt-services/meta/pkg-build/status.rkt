#lang racket/base
(require racket/list)

(provide status
         substatus
         show-list)

(define (substatus fmt . args)
  (apply printf fmt args)
  (flush-output))

(define (status fmt . args)
  (printf ">> ")
  (apply substatus fmt args))

(define (show-list nested-strs #:indent [indent ""])
  (define strs (let loop ([strs nested-strs])
                 (cond
                  [(null? strs) null]
                  [(pair? (car strs))
                   (define l (car strs))
                   (define len (length l))
                   (loop (append
                          (list (string-append "(" (car l)))
                          (take (cdr l) (- len 2))
                          (list (string-append (last l) ")"))
                          (cdr strs)))]
                  [else (cons (car strs) (loop (cdr strs)))])))
  (substatus "~a\n"
             (for/fold ([a indent]) ([s (in-list strs)])
               (if ((+ (string-length a) 1 (string-length s)) . > . 72)
                   (begin
                     (substatus "~a\n" a)
                     (string-append indent " " s))
                   (string-append a " " s)))))
