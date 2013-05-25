#lang unstable/2d racket/base
(require unstable/2d/cond
         rackunit)

(define (basic a b c d)
  #2dcond
  ╔═══╦═══╦═══╗
  ║   ║ a ║ b ║
  ╠═══╬═══╬═══╣
  ║ c ║ 1 ║ 2 ║
  ╠═══╬═══╬═══╣
  ║ d ║ 3 ║ 4 ║
  ╚═══╩═══╩═══╝)

(define ((matches reg) exn) (regexp-match? reg (exn-message exn)))

(check-equal? (basic #t #t #t #t) 1)
(check-equal? (basic #t #f #t #f) 1)
(check-equal? (basic #f #t #t #t) 2)
(check-equal? (basic #f #t #t #f) 2)
(check-equal? (basic #t #t #f #t) 3)
(check-equal? (basic #t #f #f #t) 3)
(check-equal? (basic #f #t #f #t) 4)
(check-exn (matches #rx"x-direction questions") 
           (λ () (basic #f #f #f #f)))
(check-exn (matches #rx"y-direction questions.*x coordinate 1")
           (λ () (basic #t #f #f #f)))
(check-exn (matches #rx"y-direction questions.*x coordinate 2")
           (λ () (basic #f #t #f #f)))

(define (bot-right-cell a b c d)
  #2dcond
  ╔═══╦═══╦═══╗
  ║   ║ a ║ b ║
  ╠═══╬═══╩═══╣
  ║ c ║ 1     ║
  ╠═══╣   ╔═══╣
  ║ d ║   ║ 2 ║
  ╚═══╩═══╩═══╝)

(check-equal? (bot-right-cell #t #t #t #t) 1)
(check-equal? (bot-right-cell #t #f #t #f) 1)
(check-equal? (bot-right-cell #f #t #t #t) 1)
(check-equal? (bot-right-cell #f #t #t #f) 1)
(check-equal? (bot-right-cell #t #t #f #t) 1)
(check-equal? (bot-right-cell #t #f #f #t) 1)
(check-equal? (bot-right-cell #f #t #f #t) 2)

(define (top-left-cell a b c d)
  #2dcond
  ╔═══╦═══╦═══╗
  ║   ║ a ║ b ║
  ╠═══╬═══╬═══╣
  ║ c ║ 1 ║   ║
  ╠═══╬═══╝   ║
  ║ d ║     2 ║
  ╚═══╩═══════╝)

(check-equal? (top-left-cell #t #t #t #t) 1)
(check-equal? (top-left-cell #t #f #t #f) 1)
(check-equal? (top-left-cell #f #t #t #t) 2)
(check-equal? (top-left-cell #f #t #t #f) 2)
(check-equal? (top-left-cell #t #t #f #t) 2)
(check-equal? (top-left-cell #t #f #f #t) 2)
(check-equal? (top-left-cell #f #t #f #t) 2)

(let ([sp (open-output-string)])
  (define (f x) (printf "~a\n" x) #f)
  (parameterize ([current-output-port sp])
    #2dcond
    ╔═════╦═══════╦═══════╦════╗
    ║     ║ (f 1) ║ (f 2) ║ #t ║
    ╠═════╬═══════╩═══════╩════╣
    ║(f 3)║                    ║
    ╠═════╣                    ║
    ║(f 4)║        222         ║
    ╠═════╣                    ║
    ║  #t ║                    ║
    ╚═════╩════════════════════╝)
  (check-equal? (get-output-string sp)
                "1\n2\n3\n4\n"))


(define (try-else a b c)
  #2dcond
  ╔════╦════╦════╗
  ║    ║ a  ║else║
  ╠════╬════╬════╣
  ║ b  ║ 1  ║  2 ║
  ╠════╬════╬════╣
  ║ c  ║ 3  ║  4 ║
  ╠════╬════╬════╣
  ║else║ 5  ║  6 ║
  ╚════╩════╩════╝)

(require rackunit)
(check-equal? (try-else #t #t #f) 1)
(check-equal? (try-else #f #t #f) 2)
(check-equal? (try-else #t #f #f) 5)
(check-equal? (try-else #f #f #f) 6)




