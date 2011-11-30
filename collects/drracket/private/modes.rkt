#lang racket/unit
  (require string-constants
           racket/class
	   racket/list
           framework
           "drsig.rkt")
  
  (import)
  (export drracket:modes^)
  
  (define-struct mode (name surrogate repl-submit matches-language))
  (define modes (list))
  
  (define (get-modes) modes)
  
  (define (add-mode name surrogate repl-submit matches-language)
    (let ([new-mode (make-mode name 
                               surrogate
                               repl-submit
                               matches-language)])
      (set! modes (cons new-mode modes))
      new-mode))
  
  (define (not-a-language-language? l)
    (and (not (null? l))
         (equal? (last l)
                 (string-constant no-language-chosen))))
  
  (define (add-initial-modes)
    
    ;; must be added first, to make it last in mode list,
    ;; since predicate matches everything
    (add-mode 
     (string-constant scheme-mode)
     (new racket:text-mode%)
     (λ (text prompt-position) (racket:text-balanced? text prompt-position))
     (λ (l) #t))
    
    (add-mode 
     (string-constant text-mode)
     #f
     (λ (text prompt-position) #t)
     (λ (l) 
       (and l
            (or (not-a-language-language? l)
                (ormap (λ (x) (regexp-match #rx"Algol" x)) l))))))
