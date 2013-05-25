#lang racket/base
(require racket/contract
         racket/list
         "structures.rkt")
(provide/contract
 [eliminate-whitespace (() ((listof symbol?) (boolean? . -> . boolean?)) . ->* . (element? . -> . element?))])

;; eliminate-whitespace : (listof Symbol) (Bool -> Bool) -> Element -> Element
(define (eliminate-whitespace [special empty]
                              [eliminate-special? (λ (x) x)])
  (letrec ([blank-it
            (lambda (el)
              (let ([name (element-name el)]
                    [content (map (lambda (x)
                                    (if (element? x) (blank-it x) x))
                                  (element-content el))])
                (make-element
                 (source-start el)
                 (source-stop el)
                 name
                 (element-attributes el)
                 (cond
                   [(eliminate-special? (and (memq (element-name el) special) #t))
                    (filter (lambda (s)
                              (not (and (pcdata? s)
                                        (or (all-blank (pcdata-string s))
                                            (error 'eliminate-blanks "Element <~a> is not allowed to contain text ~e" name (pcdata-string s))))))
                            content)]
                   [else content]))))])
    blank-it))

;; all-blank : String -> Bool
(define (all-blank s) (andmap char-whitespace? (string->list s)))
