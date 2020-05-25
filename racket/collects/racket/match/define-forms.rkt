#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     (only-in racket/list append*)
                     racket/lazy-require))

(module define-match-imp racket/base
  (require syntax/parse/pre syntax/parse/lib/function-header
           (for-template racket/base))
  (provide define/match-impl)
  (define (define/match-impl stx derived-id)
    (syntax-parse stx
      [(_ ?header:function-header ?clause ...)
       (with-syntax ([match*/derived derived-id])
         #`(define ?header
             (match*/derived (~? ?header.params) #,stx
                             ?clause ...)))])))


(begin-for-syntax
 (lazy-require [racket/match/patterns (bound-vars pats->bound-vars)]
               [racket/match/gen-match (go go/one)]
               [(submod "." define-match-imp) (define/match-impl)]))

(provide define-forms)


(define-syntax-rule (define-forms parse-id
                      match match* match-lambda match-lambda*
		      match-lambda** match-let match-let*
                      match-let-values match-let*-values
		      match-define match-define-values
                      match-letrec match-letrec-values
		      match/values match/derived match*/derived
                      define/match)
  (...
   (begin
     (provide match match* match-lambda match-lambda* match-lambda**
	      match-let match-let* match-let-values match-let*-values
              match-define match-define-values
              match-letrec match-letrec-values
	      match/values match/derived match*/derived match-define-values
              define/match)
     (define-syntax (match* stx)
       (syntax-case stx ()
         [(_ es . clauses)
          (go parse-id stx #'es #'clauses)]))

     (define-syntax (match*/derived stx)
       (syntax-case stx ()
         [(_ es orig-stx . clauses)
          (go parse-id #'orig-stx #'es #'clauses)]))

     (define-syntax (match stx)
       (syntax-case stx ()
         [(_ arg clauses ...)
          (go/one parse-id stx #'arg #'(clauses ...))]))

     (define-syntax (match/derived stx)
       (syntax-case stx ()
         [(_ arg orig-stx clauses ...)
          (go/one parse-id #'orig-stx #'arg #'(clauses ...))]))

     (define-syntax (match/values stx)
       (syntax-case stx ()
         [(_ arg cl0 clauses ...)
          (with-syntax ([[(pats ...) rhs ...] #'cl0])
            (with-syntax ([(ids ...) (generate-temporaries #'(pats ...))])
              (quasisyntax/loc stx
                (let-values ([(ids ...) arg])
                  (match*/derived (ids ...) #,stx cl0 clauses ...)))))]))

     (define-syntax (match-lambda stx)
       (syntax-case stx ()
         [(_ . clauses)
          (with-syntax* ([arg (generate-temporary)]
                         [body #`(match/derived arg #,stx . clauses)])
            (syntax/loc stx (lambda (arg) body)))]))

     (define-syntax (match-lambda* stx)
       (syntax-case stx ()
         [(_ . clauses)
          (with-syntax* ([arg (generate-temporary)]
                         [body #`(match/derived arg #,stx . clauses)])
            (syntax/loc stx (lambda arg body)))]))

     (define-syntax (match-lambda** stx)
       (syntax-case stx ()
         [(_ [(pats ...) . rhs] ...)
          (when (null? (syntax-e #'(rhs ...)))
            (raise-syntax-error #f "expected at least one clause to match-lambda**" stx))
          (with-syntax* ([vars (generate-temporaries (car (syntax-e #'((pats ...) ...))))]
                         [body #`(match*/derived vars #,stx #,@(cdr (syntax-e stx)))])
            (syntax/loc stx (lambda vars body)))]))


     (define-syntax (match-let-values stx)
       (syntax-case stx ()
         [(_ ([(patss ...) rhss] ...) body1 body ...)
          (let ()
            (define-values (idss let-clauses)
              (for/lists (idss let-clauses)
                         ([pats (syntax->list #'((patss ...) ...))]
                          [rhs (syntax->list #'(rhss ...))])
                (define ids (generate-temporaries pats))
                (values ids #`[#,ids #,rhs])))
            (quasisyntax/loc stx
              (let-values #,let-clauses
                (match*/derived #,(append* idss) #,stx
                                [(patss ... ...) (let () body1 body ...)]))))]))

     ;; note: match-let*-values/derived is *not* provided
     (define-syntax (match-let*-values/derived stx)
       (syntax-case stx ()
         [(_ orig-stx () body1 body ...)
          (syntax/loc stx (let () body1 body ...))]
         [(_ orig-stx ([(pats ...) rhs] rest-pats ...) body1 body ...)
          (with-syntax ([(ids ...) (generate-temporaries #'(pats ...))])
            (quasisyntax/loc stx
              (let-values ([(ids ...) rhs])
                (match*/derived (ids ...) orig-stx
                  [(pats ...) #,(syntax/loc stx
                                  (match-let*-values/derived
                                   orig-stx (rest-pats ...)
                                   body1 body ...))]))))]))

     (define-syntax (match-let*-values stx)
       (syntax-case stx ()
         [(_ cl body1 body ...)
          (with-syntax ([([(pats ...) rhs] ...) #'cl])
            (quasisyntax/loc stx (match-let*-values/derived #,stx cl body1 body ...)))]))

     ;; there's lots of duplication here to handle named let
     ;; some factoring out would do a lot of good
     (define-syntax (match-let stx)
       (syntax-case stx ()
         [(_ nm ([pat init-exp] ...) body1 body ...)
          (identifier? #'nm)
          (with-syntax*
            ([vars (generate-temporaries #'(pat ...))]
             [loop-body (quasisyntax/loc stx
                          (match*/derived vars #,stx
                                          [(pat ...) (let () body1 body ...)]))])
            (syntax/loc stx
              (letrec ([nm (lambda vars loop-body)])
                (nm init-exp ...))))]
         [(_ ([pat init-exp] ...) body1 body ...)
          (quasisyntax/loc stx
            ;; use of match*/derived instead of match-let-values fixes #1431
            ;; alternatively, we could have created let-values/derived but
            ;; that is not really necessary
            (match*/derived [init-exp ...] #,stx [(pat ...) (let () body1 body ...)]))]))

     (define-syntax (match-let* stx)
       (syntax-case stx ()
         [(_ ([pat rhs] ...) body1 body ...)
          (quasisyntax/loc stx
            (match-let*-values/derived
             #,stx
             ([(pat) rhs] ...)
             body1 body ...))]))

     ;; note: match-define-values/derived is *not* provided
     ;; it may be useful enough to suggest we should provide it...
     (define-syntax (match-define-values/derived stx)
       (syntax-case stx ()
         [(_ orig-stx (pats ...) rhs)
          (with-syntax ([(ids ...) (generate-temporaries #'(pats ...))]
                        [(pb-ids ...) (pats->bound-vars parse-id (syntax->list #'(pats ...)))])
            (quasisyntax/loc stx
              (define-values (pb-ids ...)
                (let-values ([(ids ...) rhs])
                  (match*/derived (ids ...) orig-stx
                                  [(pats ...) (values pb-ids ...)])))))]))

     (define-syntax (match-letrec stx)
       (syntax-case stx ()
         [(_ (cl ...) body1 body ...)
          (with-syntax ([([pat exp] ...) #'(cl ...)])
            (quasisyntax/loc stx
              (let ()
                #,@(for/list ([c (in-list (syntax->list #'(cl ...)))]
                              [p (in-list (syntax->list #'(pat ...)))]
                              [e (in-list (syntax->list #'(exp ...)))])
                     (quasisyntax/loc c
                       (match-define-values/derived #,stx (#,p) #,e)))
                body1 body ...)))]))

     (define-syntax (match-letrec-values stx)
       (syntax-case stx ()
         [(_ (cl ...) body1 body ...)
          (with-syntax ([([(pat ...) exp] ...) #'(cl ...)])
            (quasisyntax/loc stx
              (let ()
                #,@(for/list ([c (syntax->list #'(cl ...))]
                              [ps (syntax->list #'((pat ...) ...))]
                              [e (syntax->list #'(exp ...))])
                     (quasisyntax/loc c
                       (match-define-values/derived #,stx #,ps #,e)))
                body1 body ...)))]))

     (define-syntax (match-define stx)
       (syntax-case stx ()
         [(_ pat rhs)
          (quasisyntax/loc stx
            (match-define-values/derived #,stx (pat) rhs))]))

     (define-syntax (match-define-values stx)
       (syntax-case stx ()
         [(_ (pats ...) rhs)
          (quasisyntax/loc stx
            (match-define-values/derived #,stx (pats ...) rhs))]))

     (define-syntax (define/match stx) (define/match-impl stx #'match*/derived)))))
