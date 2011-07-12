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

(define (semicolon? what)
  (define-literal-set check (semicolon))
  (and (identifier? what)
       ((literal-set->predicate check) what)))

(define (parse input)
  (define (do-parse stream precedence left current)
    (debug "parse ~a precedence ~a left ~a current ~a\n" stream precedence left current)
    (syntax-parse stream
      [() #'(void)]
      [(head rest ...)
       (cond
         [(honu-macro? #'head)
          (begin
            (debug "Honu macro ~a\n" #'head)
            (let-values ([(parsed unparsed)
                          ((syntax-local-value #'head) #'(head rest ...) #f)])
              (with-syntax ([parsed parsed]
                            [rest unparsed])
                (do-parse #'rest precedence #'parsed current)
                #;
                #'(splicing-let-syntax ([more-parsing (lambda (stx)
                                                        (do-parse (stx-cdr stx)
                                                                  precedence left
                                                                  current))])
                                       parsed
                                       (more-parsing . rest)))))]
         [(semicolon? #'head)
          (with-syntax ([so-far left])
            #'(splicing-let-syntax ([more (lambda (stx)
                                            (parse #'(rest ...)))])
                so-far (more)))]

         [(identifier? #'head)
          (do-parse #'(rest ...) precedence #'head current)]
         [else (syntax-parse #'head
                 #:literal-sets (cruft)
                 [(#%parens args ...)
                  (debug "function call ~a\n" left)
                  (with-syntax ([left left])
                    #'(left args ...))
                  #;
                  (error 'parse "function call")]
                 [else (error 'what "dont know ~a" #'head)])]

         )]))

  (do-parse input 0 #f #f))

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
