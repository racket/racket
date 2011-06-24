#lang scheme/base

(require (for-syntax scheme/base
                     racket/syntax
                     unstable/sequence
                     syntax/parse
                     "parse.rkt"
                     "parse-helper.rkt"
                     "patterns.rkt"
                     "gen-match.rkt"))

(provide define-forms)

(define-syntax-rule (define-forms parse-id
                      match match* match-lambda match-lambda* 
		      match-lambda** match-let match-let* 
		      match-define match-letrec
		      match/derived match*/derived)
  (...
   (begin
     (provide match match* match-lambda match-lambda* match-lambda**
	      match-let match-let* match-define match-letrec
	      match/derived match*/derived)
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
         [(_ (~and clauses ([pat init-exp:expr] ...)) body1 body ...)
          #`(match*/derived (init-exp ...) #,stx 
			    [(pat ...) (let () body1 body ...)])]))

     (define-syntax (match-let* stx)
       (syntax-parse stx
         [(_ () body1 body ...)
          #'(let () body1 body ...)]
         [(_ ([pat exp] rest-pats ...) body1 body ...)
          #`(match*/derived 
             (exp)
             #,stx
             [(pat) #,(syntax/loc stx (match-let* (rest-pats ...) 
					 body1 body ...))])]))

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
				      [(pat) (values . vars)])))))])))))
