#lang scheme
(provide snippet-at-position
         snippet-at-line
         occurs-at-lines
         occurs-at-positions)

(require "config.scm")

(define (snippet-at-position file pos size)
  (with-input-from-file file
    (λ ()
      (file-position (current-input-port)
                     (max 0 (- pos (quotient size 2))))
      (read-string size))))

(define (snippet-at-line file line . extra-lines)
  (let ([e (if (null? extra-lines) 0 (car extra-lines))])
    (with-input-from-file file
      (λ ()
        (let loop ([n line])
          (cond [(= (max 1 (- n e)) 1)
                 (read-lines (+ 1 (* 2 e)))]
                [else
                 (let ([s (read-line)])
                   (cond [(eof-object? s) '()]
                         [else (loop (- n 1))]))]))))))

(define (occurs-at-lines file term)
  (with-input-from-file file
    (λ ()
      (port-count-lines! (current-input-port))
      (let ([term-reg (regexp-quote term)])
        (let loop ([lines '()])
          (cond [(regexp-match term-reg (current-input-port))
                 => (λ (pos)
                      (let-values ([(l c p) (port-next-location (current-input-port))])
                        (loop (cons l lines))))]
                [else (reverse lines)]))))))

(define (occurs-at-positions file term)
  (with-input-from-file file
    (λ ()
      (map car (regexp-match-positions* term (current-input-port))))))

(define (read-lines n)
  (cond [(<= n 0) '()]
        [(read-line) 
         => (λ (l) 
              (if (eof-object? l)
                  '()
                  (cons l (read-lines (- n 1)))))]
        [else '()]))



;(display (snippet-at-position test-file 17800 80))
;(newline)
;(display "--\n")
;(require "intersperse.scm")
;(display (apply string-append
;                (intersperse "\n" (snippet-at-line test-file 1 2))))
