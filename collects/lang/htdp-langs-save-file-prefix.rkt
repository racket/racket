#lang racket/base

;; ---------------------------------------------------------------------------------------------------
;; exports the header for a file saved from a drracket buffer in a menu-selected teaching language 

(require racket/contract 
         racket/port)

(provide/contract
 [htdp-save-file-prefix (listof string?)]
 [htdp-file-prefix? (-> input-port? boolean?)])


(define htdp-save-file-prefix
  (list ";; The first three lines of this file were inserted by DrRacket. They record metadata"
        ";; about the language level of this file in a form that our tools can easily process."))

(define (htdp-file-prefix? port)
  (define pp (peeking-input-port port))
  (define ans (htdp-file-prefix/raw? pp))
  (when ans
    (htdp-file-prefix/raw? port))
  ans)
  
;; htdp-file-prefix/raw? : input-port? -> boolean
;; SIDE EFFECT: consumes input from the port;
;;  in the case the it returns #t, then it consumes precisely
;;  the prefix that DrRacket saves
;;  in the case that it returns #f, it consumes some random amount
(define (htdp-file-prefix/raw? port)
  (let loop ([prefix htdp-save-file-prefix])
    (cond
      [(null? prefix)
       (define l (read-line port 'any))
       (and (string? l) (pair? (regexp-match #rx"^#reader" l)))]
      [else
       (define l (read-line port 'any))
       (and (string? l)
            (equal? l (car prefix))
            (loop (cdr prefix)))])))
