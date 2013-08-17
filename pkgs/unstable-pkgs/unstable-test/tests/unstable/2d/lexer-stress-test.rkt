#lang racket/gui

(require framework/private/color-local-member-name
         syntax-color/racket-lexer
         unstable/2d/lexer
         framework)

(define f (new frame% [label ""] [width 400] [height 600]))
(define t (new (class racket:text%
                 (define/override (tokenizing-give-up-early) 
                   (when (zero? (random 2))
                     (do-something))
                   #t)
                 (super-new))))
(define ec (new editor-canvas% [parent f] [editor t]))

(define count 0)

(define (do-something)
  (queue-callback (λ () 
                    (set! count (+ count 1))
                    (cond
                      [(< count 100)
                       (cond
                         [(send t find-string "-" 'forward 0)
                          =>
                          (λ (x)
                            (send t delete x (+ x 1)))]
                         [else
                          ;; these two numbers are dependent
                          ;; on the string constant below
                          (define n (+ 36 (random 448)))
                          (define howmany (+ 1 (random 2)))
                          (for ([x (in-range howmany)])
                            (send t insert "-" n n))])]
                      [else
                       (send tmr stop)
                       (send f show #f)]))))

(define tmr (new timer% [notify-callback do-something] [interval 100]))

(send f show #t)

(send t insert
      #<<---
#lang unstable/2d racket/base

#2dx
╔═══╦═══╦═══╦═══╗
║ 1 ║ 2 ║ 3 ║ 4 ║
╠═══╬═══╩═══╩═══╣
║ 5 ║("abcdef") ║
╠═══╣(|zz zzz|) ║
║ 6 ║(31415926) ║
╠═══╬═══╦═══╦═══╣
║ 7 ║ 8 ║ 9 ║ 0 ║
╠═══╬═══╬═══╬═══╣
║ A ║ B ║ C ║ D ║
╠═══╬═══╩═══╩═══╣
║ E ║("ghijkl") ║
╠═══╣(|xx xxx|) ║
║ F ║(27182818) ║
╠═══╬═══╦═══╦═══╣
║ G ║ H ║ I ║ J ║
╠═══╬═══╬═══╬═══╣
║ K ║ L ║ M ║ N ║
╠═══╬═══╩═══╩═══╣
║ O ║("mnopqs") ║
╠═══╣(|yy yyy|) ║
║ P ║(whatever) ║
╠═══╬═══╦═══╦═══╣
║ Q ║ R ║ S ║ T ║
╚═══╩═══╩═══╩═══╝

---
)

