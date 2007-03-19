(module match-internal-func mzscheme
  
  (provide (all-defined))
  
  (require-for-syntax "gen-match.ss"                      
                      "match-helper.ss"
                      "match-error.ss") 
  
  (require (lib "etc.ss")           
           (lib "list.ss")
           "match-expander.ss"
           "match-error.ss")
  
  
  (define-syntax (match stx)
    (syntax-case stx ()
      [(_ exp . clauses)       
       (with-syntax ([body (gen-match #'x #'clauses stx)])
         #`(let ([x exp]) body))]))
  
  (define-syntax (match-lambda stx)
    (syntax-case stx ()
      [(k . clauses)
       #'(lambda (exp) (match exp . clauses))]))
  
  (define-syntax (match-lambda* stx)
    (syntax-case stx ()
      [(k . clauses)
       #'(lambda exp (match exp . clauses))]))
  
  ;; there's lots of duplication here to handle named let
  ;; some factoring out would do a lot of good
  (define-syntax (match-let stx)
    (syntax-case stx ()
      ;; an empty body is an error
      [(_ nm (clauses ...))
       (identifier? #'nm)
       (match:syntax-err stx "bad syntax (empty body)")]
      [(_ (clauses ...)) (match:syntax-err stx "bad syntax (empty body)")]
      ;; with no bindings, there's nothing to do
      [(_ name () body ...) 
       (identifier? #'name)
       #'(let name () body ...)]
      [(_ () body ...) #'(let () body ...)]  
      ;; optimize the all-variable case            
      [(_ ([pat exp]...) body ...)
       (andmap pattern-var? (syntax->list #'(pat ...)))
       #'(let name ([pat exp] ...) body ...)]           
      [(_ name ([pat exp]...) body ...)
       (and (identifier? (syntax name))
            (andmap pattern-var? (syntax->list #'(pat ...))))
       #'(let name ([pat exp] ...) body ...)]
      ;; now the real cases
      [(_ name ([pat exp] ...) . body)
       #'(letrec ([name (match-lambda* ((list pat ...) . body))])                             
           (name exp ...))]        
      [(_ ([pat exp] ...) . body)
       #'(match (list exp ...) [(list pat ...) . body])]))
  
  (define-syntax (match-let* stx)
    (syntax-case stx ()
      [(_ (clauses ...)) (match:syntax-err stx "bad syntax (empty body)")]
      ((_ () body ...)
       #'(let* () body ...))
      ((_ ([pat exp] rest ...) body ...)
       (if (pattern-var? (syntax pat))
           #'(let ([pat exp])
               (match-let* (rest ...) body ...))
           #'(match exp [pat (match-let* (rest ...) body ...)]))
       )))
  
  (define-syntax (match-letrec stx)
    (syntax-case stx ()
      [(_ (clauses ...)) (match:syntax-err stx "bad syntax (empty body)")]
      [(_ ([pat exp] ...) . body)
       (andmap pattern-var?
               (syntax->list #'(pat ...)))
       #'(letrec ([pat exp] ...) . body)]
      [(_ ([pat exp] ...) . body)
       #'(let ()
           (match-define (list pat ...) (list exp ...))
           . body)]))
  
  (define-syntax (match-define stx)
    (syntax-case stx ()
      [(_ pat exp)
       (identifier? #'pat)
       #'(define pat exp)]
      [(_ pat exp)
       (let ([**match-bound-vars** '()])
         (with-syntax ([compiled-match
                        (gen-match #'the-exp                        
                                   #'((pat never-used))
                                   stx
                                   (lambda (sf bv)
                                     (set! **match-bound-vars** bv)
                                     (with-syntax ([((vars . vals) ...) (reverse bv)])
                                       #'(values vals ...))))]
                       [(vars ...) (map car (reverse **match-bound-vars**))])
           #'(define-values (vars ...)
               (let ([the-exp exp])
                 compiled-match))))]))
  )