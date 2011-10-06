#lang racket/base
(require syntax-color/module-lexer
         rackunit)

(define (lex str)
  (define p (open-input-string str))
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

(check-equal? (lex "#lang racket/base")
              `(("#lang" other 1 18 #f)
                (,eof eof #f #f (proc scheme-lexer))))
(check-equal? (lex "#lang racket/base\n1")
              `(("#lang" other 1 18 #f)
                ("\n" white-space 18 19 (proc scheme-lexer)) 
                ("1" constant 19 20 (proc scheme-lexer))
                (,eof eof #f #f (proc scheme-lexer))))
(check-equal? (lex ";; a\n#lang racket/base")
              `(("; a" comment 1 5 #f) 
                ("\n" white-space 5 6 before-lang-line) 
                ("#lang" other 1 18 before-lang-line)
                (,eof eof #f #f (proc scheme-lexer))))
(check-equal? (lex "#lang at-exp racket/base")
              `(("#lang" other 1 25 #f)
                (,eof eof 25 25 ((proc scribble-lexer) . #f))))
(check-equal? (lex "#lang at-exp racket/baseBOGUS")
              `(("#lang at-exp" error 1 30 #f)
                (,eof eof #f #f no-lang-line)))
