#lang racket/base

(define-syntax-rule (require-syntax stuff ...)
                    (require (for-syntax stuff ...)))

;; phase 0
(require ;; "macro2.rkt"
         "literals.rkt"
         "debug.rkt"
         (prefix-in transformer: "transformer.rkt")
         syntax/stx
         syntax/parse)
;; phase 1
(require-syntax racket/base)

;; phase -1
(require (for-template racket/base
                       racket/splicing))

(provide parse)

#;
(define-literal-set literals
                    [honu-macro])

(define (get-value what)
  (syntax-local-value what (lambda () #f)))

#;
(define (get-value what)
  (debug "what is ~a\n" what)
  (with-syntax ([what what])
    (let-syntax ([v (lambda (stx)
                      (debug "get ~a\n" #'what)
                      (with-syntax ([x (syntax-local-value #'what (lambda () #f))])
                        #'x))])
      (v))))

#;
(define (get-value check)
  (eval-syntax
    (with-syntax ([check check])
      #'(syntax-local-value check #'check (lambda () #f))))) 

(define (bound-to-operator? check)
  (let ([value (get-value check)])
    (debug "operator? ~a ~a\n" check value)
    (transformer:honu-operator? value)))

(define (bound-to-macro? check)
  (let ([value (get-value check)])
    (debug "macro? ~a ~a\n" check value)
    (transformer:honu-transformer? value))
  #;
  (let ([value (syntax-local-value check (lambda () #f))])
    (transformer:honu-transformer? value)))

(define (honu-macro? something)
  (and (identifier? something)
       (bound-to-macro? something)))

(define (honu-operator? something)
  (and (identifier? something)
       (bound-to-operator? something)))

(define (semicolon? what)
  (define-literal-set check (semicolon))
  (and (identifier? what)
       ((literal-set->predicate check) what)))

;; 1 + 1
;; ^
;;  left: identity
;;  current: 1
;; 1 + 1
;;   ^
;;  left: (lambda (x) (+ 1 x))
;;  current: #f
;; 1 + 1
;;     ^
;;  left: (lambda (x) (+ 1 x))
;;  current: 1
;;
;; 1 + 1 * 2
;;       ^
;;  left: (lambda (x) (left (* 1 x)))
;;  current: #f
;;
;; 1 + 1 * 2
;;         ^
;;  left: (lambda (x) (left (* 1 x)))
;;  current: 2

(define (parse input)
  (define (do-parse stream precedence left current)
    (debug "parse ~a precedence ~a left ~a current ~a\n" stream precedence left current)
    (syntax-parse stream
      [() (left current)]
      [(head rest ...)
       (cond
         [(honu-macro? #'head)
          (begin
            (debug "Honu macro ~a\n" #'head)
            (let-values ([(parsed unparsed)
                          ((syntax-local-value #'head) #'(head rest ...) #f)])
              (with-syntax ([parsed parsed]
                            [rest unparsed])
                (do-parse #'rest precedence (lambda (x)
                                              (with-syntax ([x x])
                                                #'(begin parsed x)))
                          (left current))
                #;
                #'(splicing-let-syntax ([more-parsing (lambda (stx)
                                                        (do-parse (stx-cdr stx)
                                                                  precedence left
                                                                  current))])
                                       parsed
                                       (more-parsing . rest)))))]
         [(honu-operator? #'head)
          (define new-precedence (transformer:honu-operator-ref (syntax-local-value #'head) 0))
          (define operator-transformer (transformer:honu-operator-ref (syntax-local-value #'head) 1))
          (define association 'left)
          (define higher
            (case association
              [(left) >]
              [(right) >=]))
          (printf "new precedence ~a\n" new-precedence)
          (if (higher new-precedence precedence)
            (do-parse #'(rest ...) new-precedence
                      (lambda (stuff)
                        (left (operator-transformer current stuff)))
                      #f)
            (do-parse #'(head rest ...)
                      0
                      (lambda (x) x)
                      (left current)))]
         [(semicolon? #'head)
          (with-syntax ([so-far (left current)])
            #'(splicing-let-syntax ([more (lambda (stx)
                                            (parse #'(rest ...)))])
                so-far (more)))]
         [(identifier? #'head)
          (do-parse #'(rest ...) precedence left #'head)]
         [else (syntax-parse #'head
                 #:literal-sets (cruft)
                 [x:number (do-parse #'(rest ...)
                                     precedence left #'x)]
                 [(#%parens args ...)
                  (debug "function call ~a\n" left)
                  (do-parse #'(rest ...)
                            0
                            (lambda (x) x)
                            (left (with-syntax ([current current])
                                    #'(current args ...))))
                  #;
                  (error 'parse "function call")]
                 [else (error 'what "dont know ~a" #'head)])]

         )]))

  (do-parse input 0 (lambda (x) x) #'(void)))

(define (parse2 forms)
  (debug "parse forms ~a\n" forms)
  (when (stx-pair? forms)
    (define head (stx-car forms))
    (if (honu-macro? head)
      (begin
        (debug "honu macro ~a\n" head)
        (let-values ([(parsed rest)
                      ((syntax-local-value head) forms #f)])
          (with-syntax ([parsed parsed]
                        [rest rest])
          #'(splicing-let-syntax ([more-parsing (lambda (stx)
                                         (debug "more parsing!!\n")
                                         (parse stx))])
              parsed
              (more-parsing . rest)))))
      #'(debug "regular parsing\n"))))
