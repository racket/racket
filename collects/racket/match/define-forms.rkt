#lang scheme/base

(require (for-syntax scheme/base
                     racket/syntax
                     (only-in racket/list append* append-map)
                     unstable/sequence
                     syntax/parse
                     "parse.rkt"
                     "parse-helper.rkt"
                     "patterns.rkt"
                     "gen-match.rkt"))

(provide define-forms)

;; each pat matches a value in a multi-valued expression
(define-for-syntax (match-values-clause->let-clause pats rhs)
  (with-syntax ([(pats ...) pats]
                [(ids ...) (generate-temporaries pats)])
    ;; rhs evaluates to number of ids values.
    ;; patterns should match against each id.
    (values #'(ids ...)
            #`[(ids ...) #,rhs])))

(define-for-syntax (match-values-clauses->let-clauses patses rhses)
  (for/lists (idses let-clauses)
      ([pats (syntax->list patses)]
       [rhs (syntax->list rhses)])
    (match-values-clause->let-clause pats rhs)))

(define-for-syntax (all-same-length stx-listses)
  (let loop ([listses (syntax->list stx-listses)]
             [the-length #f])
    (cond [(null? listses) #t]
          [the-length
           (and (= the-length (length (syntax->list (car listses))))
                (loop (cdr listses) the-length))]
          [else (loop (cdr listses) (length (syntax->list (car listses))))])))

(define-syntax-rule (define-forms parse-id
                      match match* match-lambda match-lambda*
		      match-lambda** match-let match-let*
                      match-let-values match-let*-values
		      match-define match-define-values match-letrec
		      match/values match/derived match*/derived)
  (...
   (begin
     (provide match match* match-lambda match-lambda* match-lambda**
	      match-let match-let* match-let-values match-let*-values
              match-define match-define-values match-letrec
	      match/values match/derived match*/derived match-define-values)
     (define-syntax (match* stx)
       (syntax-parse stx
         [(_ es . clauses)
          (go parse-id stx #'es #'clauses)]))

     (define-syntax (match*/derived stx)
       (syntax-parse stx
         [(_ es orig-stx . clauses)
          (go parse-id #'orig-stx #'es #'clauses)]))

     (define-syntax (match stx)
       (syntax-parse stx
         [(_ arg:expr clauses ...)
          (go/one parse-id stx #'arg #'(clauses ...))]))

     (define-syntax (match/derived stx)
       (syntax-parse stx
         [(_ arg:expr orig-stx clauses ...)
          (go/one parse-id #'orig-stx #'arg #'(clauses ...))]))

     (define-syntax (match/values stx)
       (syntax-parse stx
         [(_ arg:expr [(pats ...) rhs:expr] [(patses ...) rhses:expr] ...)
          #:fail-unless (all-same-length #'((pats ...) (patses ...) ...))
          "All clauses must have the same number of patterns"
          (define-values (ids let-clause)
            (match-values-clause->let-clause #'(pats ...) #'rhs))
          #`(let-values ([#,ids arg])
              (match*/derived #,ids #,stx [(pats ...) rhs] [(patses ...) rhses] ...))]))

     (define-syntax (match-lambda stx)
       (syntax-parse stx
         [(_ . clauses)
          (with-syntax* ([arg (generate-temporary)]
                         [body #`(match/derived arg #,stx . clauses)])
            (syntax/loc stx (lambda (arg) body)))]))

     (define-syntax (match-lambda* stx)
       (syntax-parse stx
         [(_ . clauses)
          (with-syntax* ([arg (generate-temporary)]
                         [body #`(match/derived arg #,stx . clauses)])
            (syntax/loc stx (lambda arg body)))]))

     (define-syntax (match-lambda** stx)
       (syntax-parse stx
         [(_ (~and clauses [(pats ...) . rhs]) ...)
          (with-syntax* ([vars (generate-temporaries (car (syntax-e #'((pats ...) ...))))]
                         [body #`(match*/derived vars #,stx clauses ...)])
            (syntax/loc stx (lambda vars body)))]))


     (define-syntax (match-let-values stx)
       (syntax-parse stx
         [(_ (~and clauses ([(patses ...) rhses:expr] ...)) body1 body ...)
          (define-values (idses let-clauses)
            (match-values-clauses->let-clauses #'((patses ...) ...) #'(rhses ...)))
          #`(let-values #,let-clauses
              (match*/derived #,(append-map syntax->list idses) #,stx
                [(patses ... ...)
                 (let () body1 body ...)]))]))

     (define-syntax (match-let*-values stx)
       (syntax-parse stx
         [(_ () body1 body ...)
          #'(let () body1 body ...)]
         [(_ ([(pats ...) rhs] rest-pats ...) body1 body ...)
          (define-values (ids let-clause)
            (match-values-clause->let-clause #'(pats ...) #'rhs))
          #`(let-values (#,let-clause)
              (match*/derived #,ids #,stx
               [(pats ...) #,(syntax/loc stx (match-let*-values (rest-pats ...)
                               body1 body ...))]))]))

     ;; there's lots of duplication here to handle named let
     ;; some factoring out would do a lot of good
     (define-syntax (match-let stx)
       (syntax-parse stx
         [(_ nm:id (~and clauses ([pat init-exp:expr] ...)) body1 body ...)
          (with-syntax*
           ([vars (generate-temporaries #'(pat ...))]
            [loop-body #`(match*/derived vars #,stx
					 [(pat ...) (let () body1 body ...)])])
           #'(letrec ([nm (lambda vars loop-body)])
               (nm init-exp ...)))]
         [(_ ([pat init-exp:expr] ...) body1 body ...)
          #`(match-let-values ([(pat) init-exp] ...) body1 body ...)]))

     (define-syntax-rule (match-let* ([pat exp] ...) body1 body ...)
       (match-let*-values ([(pat) exp] ...) body1 body ...))

     (define-syntax (match-letrec stx)
       (syntax-parse stx
         [(_ ((~and cl [pat exp]) ...) body1 body ...)
          (quasisyntax/loc stx
			   (let ()
                            #,@(for/list ([c (in-syntax #'(cl ...))]
                                          [p (in-syntax #'(pat ...))]
                                          [e (in-syntax #'(exp ...))])
                                 (quasisyntax/loc c (match-define #,p #,e)))
                            body1 body ...))]))

     (define-syntax (match-define stx)
       (syntax-parse stx
         [(_ pat rhs:expr)
          (let ([p (parse-id #'pat)])
            (with-syntax ([vars (bound-vars p)])
              (quasisyntax/loc stx
                (define-values vars (match*/derived (rhs) #,stx
				      [(pat) (values . vars)])))))]))

     (define-syntax (match-define-values stx)
       (syntax-parse stx
         [(_ (pats ...) rhs:expr)
          (define ppats (map parse-id (syntax->list #'(pats ...))))
          (define bound-vars-list (map bound-vars ppats))
          (with-syntax ([(ids ...) (generate-temporaries #'(pats ...))]
                        [(pat-vars ...) bound-vars-list]
                        [vars (append* bound-vars-list)])
            (quasisyntax/loc stx
              (define-values vars
                (let-values ([(ids ...) rhs])
                  (apply values
                         (append
                          (match*/derived (ids) #,stx
                                          [(pats) (list . pat-vars)]) ...))))))])))))
