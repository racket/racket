#lang racket/unit
(require string-constants
         racket/class
         racket/list
         framework
         racket/gui/base
         drracket/private/drsig
         "local-member-names.rkt")

(import [prefix drracket:module-language: drracket:module-language/int^])
(export drracket:modes/int^)

(define mode? drracket:modes:mode?)
(define mode-name drracket:modes:mode-name)
(define mode-surrogate drracket:modes:mode-surrogate)
(define mode-repl-submit drracket:modes:mode-repl-submit)
(define mode-matches-language drracket:modes:mode-matches-language)
(define struct:mode struct:drracket:modes:mode)


(define modes (list))

(define (get-modes) modes)

(define (add-mode name surrogate repl-submit matches-language)
  (let ([new-mode (drracket:modes:mode name 
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
   (string-constant racket-mode)
   (new drracket:module-language:default-surrogate%)
   (λ (text prompt-position) (racket:text-balanced? text prompt-position))
   (λ (l) (member (string-constant module-language-name) l)))
  
  (add-mode 
   (string-constant text-mode)
   #f
   (λ (text prompt-position) #t)
   (λ (l) 
     (and l
          (or (not-a-language-language? l)
              (ormap (λ (x) (regexp-match #rx"Algol" x)) l))))))
