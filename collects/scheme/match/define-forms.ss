#lang scheme/base

(require (for-syntax scheme/base
                     "parse.ss"
                     "parse-helper.ss"
                     "patterns.ss"
                     "gen-match.ss"))

(provide define-forms)

(define-syntax-rule (define-forms parse-id
                      match match* match-lambda match-lambda* match-lambda** match-let
                      match-let* match-define match-letrec)
  (...
   (begin
     (provide match match* match-lambda match-lambda* match-lambda** match-let match-let*
              match-define match-letrec)
     (define-syntax (match* stx)
       (syntax-case stx ()
         [(_ es . clauses)
          (go parse-id stx #'es #'clauses (syntax-local-certifier))]))

     (define-syntax (match stx)
       (syntax-case stx ()
	   [(match arg cl ...)
	    (with-syntax ([clauses
			   (for/list ([c (syntax->list #'(cl ...))])
			     (syntax-case c ()
			       [[p . es] (syntax/loc c [(p) . es])]))])			 
	      (syntax/loc stx (match* (arg) . clauses)))]))

     (define-syntax (match-lambda stx)
       (syntax-case stx ()
         [(k . clauses) (syntax/loc stx (lambda (exp) (match exp . clauses)))]))

     (define-syntax (match-lambda* stx)
       (syntax-case stx ()
         [(k . clauses) (syntax/loc stx (lambda exp (match exp . clauses)))]))

     (define-syntax (match-lambda** stx)
       (syntax-case stx ()
         [(k [pats . rhs] ...)
          (let* ([pss (syntax->list #'(pats ...))]
                 [ps1 (car pss)])
            (unless (syntax->list ps1)
              (raise-syntax-error 
               #f "expected a sequence of patterns" stx ps1))
            (let ([len (length (syntax->list ps1))])
              (for/list ([ps pss])
                (unless (= (length (syntax->list ps)) len)
                  (raise-syntax-error
                   #f "unequal number of patterns in match clauses"
                   stx ps)))
              (with-syntax ([(vars ...) (generate-temporaries (car pss))])
                (syntax/loc stx
                  (lambda (vars ...) (match* (vars ...) [pats . rhs] ...))))))]))

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
          (syntax/loc stx (let name () body ...))]
         [(_ () body ...) (syntax/loc stx (let () body ...))]
         ;; optimize the all-variable case
         [(_ ([pat exp]...) body ...)
          (andmap pattern-var? (syntax->list #'(pat ...)))
          (syntax/loc stx (let name ([pat exp] ...) body ...))]
         [(_ name ([pat exp]...) body ...)
          (and (identifier? (syntax name))
               (andmap pattern-var? (syntax->list #'(pat ...))))
          (syntax/loc stx (let name ([pat exp] ...) body ...))]
         ;; now the real cases
         [(_ name ([pat exp] ...) . body)
          (identifier? #'name)
          (syntax/loc stx (letrec ([name (match-lambda** ((pat ...) . body))])
                            (name exp ...)))]
         [(_ ([pat exp] ...) . body)
          (syntax/loc stx (match* (exp ...) [(pat ...) . body]))]))

     (define-syntax (match-let* stx)
       (syntax-case stx ()
         [(_ (clauses ...)) (match:syntax-err stx "bad syntax (empty body)")]
         [(_ () body ...)
          (syntax/loc stx (let* () body ...))]
         [(_ ([pat exp] rest ...) body ...)
          (syntax/loc stx (match exp [pat (match-let* (rest ...) body ...)]))]))

     (define-syntax (match-letrec stx)
       (syntax-case stx ()
         [(_ (clauses ...)) (match:syntax-err stx "bad syntax (empty body)")]
         [(_ ([pat exp] ...) . body)
          (andmap pattern-var?
                  (syntax->list #'(pat ...)))
          (syntax/loc stx (letrec ([pat exp] ...) . body))]
         [(_ ([pat exp] ...) . body)
          (syntax/loc stx (let () (match-define pat exp) ... . body))]))

     (define-syntax (match-define stx)
       (syntax-case stx ()
         [(_ pat exp)
          (pattern-var? #'pat)
          (syntax/loc stx (define pat exp))]
         [(_ pat rhs)
          ;; FIXME - calls parse twice
          (let ([p (parse-id #'pat (syntax-local-certifier))])
            (with-syntax ([vars (bound-vars p)])
              (syntax/loc stx
                (define-values vars (match rhs [pat (values . vars)])))))])))))
