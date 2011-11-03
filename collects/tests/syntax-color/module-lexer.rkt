#lang racket/base
(require syntax-color/module-lexer
         rackunit)

(define (lex str)
  (define p (open-input-string str))
  (port-count-lines! p)
  (let loop ([mode #f]
             [n 0])
    (define-values (lexeme type data token-start token-end backup new-mode)
      (module-lexer p
                    0
                    mode))
    (define one (list lexeme 
                      type token-start token-end 
                      (cond
                        [(procedure? mode)
                         `(proc ,(object-name mode))]
                        [(and (pair? mode)
                              (procedure? (car mode)))
                         (cons `(proc ,(object-name (car mode)))
                               (cdr mode))]
                        [else mode])))
    (cond
      [(eof-object? lexeme) (list one)]
      [(= n 1000) '()] ;; watch out for loops
      [else (cons one (loop new-mode (+ n 1)))])))

(define (same? a b)
  (cond
    [(eq? a 'dont-care) #t]
    [(eq? b 'dont-care) #t]
    [(and (pair? a) (pair? b))
     (and (same? (car a) (car b))
          (same? (cdr a) (cdr b)))]
    [else (equal? a b)]))

(check-equal? (lex "#lang racket/base")
              `(("#lang" other 1 18 #f)
                (,eof eof #f #f (proc scheme-lexer))))
(check-equal? (lex "#lang racket/base\n1")
              `(("#lang" other 1 18 #f)
                ("\n" white-space 18 19 (proc scheme-lexer)) 
                ("1" constant 19 20 (proc scheme-lexer))
                (,eof eof #f #f (proc scheme-lexer))))
(check-equal? (lex ";; αα\n")
              `(("; αα" comment 1 6 #f) 
                ("\n" white-space 6 7 before-lang-line) 
                (,eof eof #f #f before-lang-line)))
(check-equal? (lex ";; ααα\n;; aaa\n")
              `(("; ααα" comment 1 7 #f) 
                ("\n" white-space 7 8 before-lang-line) 
                ("; aaa" comment 8 14 before-lang-line) 
                ("\n" white-space 14 15 before-lang-line)
                (,eof eof #f #f before-lang-line)))
(check-equal? (lex ";; a\n#lang racket/base")
              `(("; a" comment 1 5 #f) 
                ("\n" white-space 5 6 before-lang-line) 
                ("#lang" other 6 23 before-lang-line)
                (,eof eof #f #f (proc scheme-lexer))))
(check-equal? (lex "#lang at-exp racket/base")
              `(("#lang" other 1 25 #f)
                (,eof eof 25 25 ((proc scribble-lexer) . #f))))
(check-equal? (lex "#lang at-exp racket/baseBOGUS")
              `(("#lang at-exp" error 1 31 #f)
                (,eof eof #f #f no-lang-line)))
(check same?
       (lex "#lang at-exp racket/base\n1\n")
       `(("#lang" other 1 25 #f)
         ("\n" white-space 25 26 ((proc scribble-lexer) . #f)) 
         ("1" constant 26 27 ((proc scribble-lexer) . dont-care))
         ("\n" white-space 27 28 ((proc scribble-lexer) . dont-care))
         (,eof eof 28 28 ((proc scribble-lexer) . dont-care))))
