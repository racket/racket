#lang scheme/unit

(require "signatures.ss"
         mzlib/trace
         scheme/list
         (except-in "type-rep.ss" make-arr) ;; doesn't need tests
         "type-effect-convenience.ss" ;; maybe needs tests
         "type-environments.ss" ;; doesn't need tests
         "lexical-env.ss" ;; maybe needs tests
         "type-annotation.ss" ;; has tests
         (except-in "utils.ss" extend)
         "type-utils.ss"
         "effect-rep.ss"
         "tc-utils.ss"
         "union.ss"
         (lib "plt-match.ss")
         (only-in "type-effect-convenience.ss" [make-arr* make-arr]))
(require (for-template scheme/base "internal-forms.ss"))

(import tc-expr^)
(export tc-lambda^)

(define (remove-var id thns elss)
  (let/ec exit
    (define (fail) (exit #f))
    (define (rv e)
      (match e
        [(Var-True-Effect: v) (if (free-identifier=? v id) (make-Latent-Var-True-Effect) (fail))]
        [(Var-False-Effect: v) (if (free-identifier=? v id) (make-Latent-Var-False-Effect) (fail))]
        [(or (True-Effect:) (False-Effect:)) e]
        [(Restrict-Effect: t v) (if (free-identifier=? v id) (make-Latent-Restrict-Effect t) (fail))]
        [(Remove-Effect: t v) (if (free-identifier=? v id) (make-Latent-Remove-Effect t) (fail))]))
    (cons (map rv thns) (map rv elss))))


;; typecheck a single lambda, with argument list and body
;; fixme: abstract the two cases!
;; syntax-list[id] block listof[type] type option[type] -> arr
(define (tc/lambda-clause/check args body arg-tys ret-ty rest-ty)
  (syntax-case args ()
    [(args* ...)    
     (if (ormap (lambda (e) (not (type-annotation e))) (syntax->list #'(args* ...)))
         (let* ([arg-list (syntax->list #'(args* ...))])
           (let ([arg-tys
                  (let ([arg-len (length arg-list)]
                        [tys-len (length arg-tys)])
                    (define (expected-str tys-len rest-ty arg-len)
                      (format "Expected function with ~a argument~a~a, but got function with ~a argument~a"
                              tys-len
                              (if (= tys-len 1) "" "s")
                              (if rest-ty " and a rest arg" "")
                              arg-len
                              (if (= arg-len 1) "" "s")))
                    (cond
                      [(= arg-len tys-len)
                       arg-list]
                      [(< arg-len tys-len)
                       (tc-error/expr
                        #:return (take arg-tys arg-len)
                        (expected-str tys-len rest-ty arg-len))]
                      [(> arg-len tys-len)
                       (tc-error/expr
                        #:return (append arg-tys
                                         (map (lambda _ (if rest-ty rest-ty (Un))) (drop arg-list tys-len)))
                        (expected-str tys-len rest-ty arg-len))]))])
           (for-each (lambda (a) (printf/log "Lambda Var: ~a~n" (syntax-e a))) arg-list)
           (with-lexical-env/extend 
            arg-list arg-tys
            (match (tc-exprs/check (syntax->list body) ret-ty)
              [(tc-result: t thn els)
               (cond
                 ;; this is T-AbsPred                
                 ;; if this function takes only one argument, and all the effects are about that one argument
                 [(and (= 1 (length arg-list)) (remove-var (car arg-list) thn els))
                  => (lambda (thn/els) (make-arr arg-tys t #f (car thn/els) (cdr thn/els)))]
                 ;; otherwise, the simple case
                 [else (make-arr arg-tys t)])]
              [t (int-err "bad match 1 - not a tc-result: ~a ~a" ret-ty t)]))))
         (let* ([arg-list (syntax->list #'(args* ...))]
                [arg-types (map get-type arg-list)])
           (for-each (lambda (a) (printf/log "Lambda Var: ~a~n" (syntax-e a))) arg-list)
           (with-lexical-env/extend 
            arg-list arg-types
            (match (tc-exprs/check (syntax->list body) ret-ty)
              [(tc-result: t thn els)
               (cond
                 ;; this is T-AbsPred                
                 ;; if this function takes only one argument, and all the effects are about that one argument
                 [(and (= 1 (length arg-list)) (remove-var (car arg-list) thn els))
                  => (lambda (thn/els) (make-arr arg-types t #f (car thn/els) (cdr thn/els)))]
                 ;; otherwise, the simple case
                 [else (make-arr arg-types t)])]
              [t (int-err "bad match 2 - not a tc-result: ~a ~a ~a" t ret-ty (syntax->datum body))]))))]
    [(args* ... . rest)
     (begin
       (unless rest-ty
         (tc-error "Expected function with ~a arguments and no rest argument,~nbut got function with ~a arguments and a rest argument"
                   (length arg-tys) (length (syntax->list #'(args* ...)))))
       (with-lexical-env/extend
        (list #'rest) (list (-lst rest-ty))
        (tc/lambda-clause/check #'(args* ...) body arg-tys ret-ty #f)))]))

;; syntax-list[id] block -> arr
(define (tc/lambda-clause args body)
  (syntax-case args ()
    [(args ...)       
     (let* ([arg-list (syntax->list #'(args ...))]
            [arg-types (map get-type arg-list)])
       (for-each (lambda (a) (printf/log "Lambda Var: ~a~n" (syntax-e a))) arg-list)
       (with-lexical-env/extend 
        arg-list arg-types
        (match (tc-exprs (syntax->list body))
          [(tc-result: t thn els)
           (cond
             ;; this is T-AbsPred                
             ;; if this function takes only one argument, and all the effects are about that one argument
             [(and (= 1 (length arg-list)) (remove-var (car arg-list) thn els))
              => (lambda (thn/els) (make-arr arg-types t #f (car thn/els) (cdr thn/els)))]
             ;; otherwise, the simple case
             [else (make-arr arg-types t)])]
          [t (int-err "bad match - not a tc-result: ~a no ret-ty" t)])))]
    [(args ... . rest)
     (let* ([arg-list (syntax->list #'(args ...))]
            [arg-types (map get-type arg-list)])
       (for-each (lambda (a) (printf/log "Lambda Var: ~a~n" (syntax-e a))) (cons #'rest arg-list))
       (cond 
         [(dotted? #'rest)
          =>
          (lambda (bound)
            (unless (Dotted? (lookup (current-tvars) bound
                                     (lambda _ (tc-error/stx #'rest
                                                             "Bound on ... type (~a) was not in scope" bound))))
              (tc-error "Bound on ... type (~a) is not an appropriate type variable" bound))
            (let ([rest-type (parameterize ([current-tvars 
                                             (extend-env (list bound) 
                                                         (list (make-DottedBoth (make-F bound)))
                                                         (current-tvars))])
                               (get-type #'rest))])
              (with-lexical-env/extend 
               arg-list
               arg-types
               (parameterize ([dotted-env (extend-env (list #'rest)
                                                      (list (cons rest-type bound))
                                                      (dotted-env))])
                 (match-let ([(tc-result: t thn els) (tc-exprs (syntax->list body))])
                   (make-arr-dots arg-types t rest-type bound))))))]
         [else
          (let ([rest-type (get-type #'rest)])
            (with-lexical-env/extend 
             (cons #'rest arg-list) 
             (cons (make-Listof rest-type) arg-types)
             (match-let ([(tc-result: t thn els) (tc-exprs (syntax->list body))])
               (make-arr arg-types t rest-type))))]))]))

;(trace tc-args)

;; tc/mono-lambda : syntax-list syntax-list -> Funty
;; typecheck a sequence of case-lambda clauses
(define (tc/mono-lambda formals bodies expected)
  (define (syntax-len s)
    (cond [(syntax->list s) => length]
          [else (let loop ([s s])
                  (cond
                    [(pair? s)
                     (+ 1 (loop (cdr s)))]
                    [(pair? (syntax-e s))
                     (+ 1 (loop (cdr (syntax-e s))))]
                    [else 1]))]))
  (if (and expected
           (= 1 (length (syntax->list formals))))
      ;; special case for not-case-lambda
      (let loop ([expected expected])
        (match expected          
          [(Mu: _ _) (loop (unfold expected))]
          [(Function: (list (arr: args ret rest #f _ _))) 
           (tc/lambda-clause/check (car (syntax->list formals)) (car (syntax->list bodies)) args ret rest)
           expected]
          [(Function: (list (arr: argss rets rests #f _ _) ...)) 
           (for ([args argss] [ret rets] [rest rests])
             (tc/lambda-clause/check (car (syntax->list formals)) (car (syntax->list bodies)) args ret rest))
           expected]
          [t (let ([t (tc/mono-lambda formals bodies #f)])
               (check-below t expected))]))
      (let loop ([formals (syntax->list formals)] 
                 [bodies (syntax->list bodies)]
                 [formals* null]
                 [bodies* null]
                 [nums-seen null])
        (cond 
          [(null? formals)
           (make-Function (map tc/lambda-clause (reverse formals*) (reverse bodies*)))]
          [(memv (syntax-len (car formals)) nums-seen)
           ;; we check this clause, but it doesn't contribute to the overall type
           (tc/lambda-clause (car formals) (car bodies))
           (loop (cdr formals) (cdr bodies) formals* bodies* nums-seen)]
          [else
           (loop (cdr formals) (cdr bodies) 
                 (cons (car formals) formals*)
                 (cons (car bodies) bodies*)
                 (cons (syntax-len (car formals)) nums-seen))]))))

(define (tc/lambda form formals bodies)
  (tc/lambda/internal form formals bodies #f))

;; typecheck a sequence of case-lambda clauses, which is possibly polymorphic
;; tc/lambda/internal syntax syntax-list syntax-list option[type] -> Type
(define (tc/lambda/internal form formals bodies expected)
  (if (or (syntax-property form 'typechecker:plambda) (Poly? expected) (PolyDots? expected))
      (tc/plambda form formals bodies expected)
      (ret (tc/mono-lambda formals bodies expected))))

(define (tc/lambda/check form formals bodies expected)
  (tc/lambda/internal form formals bodies expected))

;; tc/plambda syntax syntax-list syntax-list type -> Poly
;; formals and bodies must by syntax-lists
(define (tc/plambda form formals bodies expected)
  (match expected
    [(Poly-names: ns (and expected* (Function: _)))
     (let* ([tvars (let ([p (syntax-property form 'typechecker:plambda)])
                     (when (and (pair? p) (eq? '... (car (last p))))
                       (tc-error "Expected a polymorphic function without ..., but given function had ..."))
                     (or (and p (map syntax-e (syntax->list p)))
                         ns))]
            [literal-tvars tvars]
            [new-tvars (map make-F literal-tvars)]
            [ty (parameterize ([current-tvars (extend-env literal-tvars new-tvars (current-tvars))])
                  (tc/mono-lambda formals bodies expected*))])
       ;(printf "plambda: ~a ~a ~a ~n" literal-tvars new-tvars ty)
       (ret expected))]
    [(PolyDots-names: (list ns ... dvar) (and expected* (Function: _)))
     (let-values 
         ([(tvars dotted)
           (let ([p (syntax-property form 'typechecker:plambda)])
             (if p
                 (match (map syntax-e (syntax->list p))
                   [(list var ... dvar '...)
                    (values var dvar)]
                   [_ (tc-error "Expected a polymorphic function with ..., but given function had no ...")])
                 (values ns dvar)))])
       (let* ([literal-tvars tvars]
              [new-tvars (map make-F literal-tvars)]
              [ty (parameterize ([current-tvars (extend-env (cons dotted literal-tvars)
                                                            (cons (make-Dotted (make-F dotted))
                                                                  new-tvars)
                                                            (current-tvars))])
                    (tc/mono-lambda formals bodies expected*))])
         (ret expected)))]
    [#f
     (match (map syntax-e (syntax->list (syntax-property form 'typechecker:plambda)))
       [(list tvars ... dotted-var '...)
        (let* ([literal-tvars tvars]
               [new-tvars (map make-F literal-tvars)]
               [ty (parameterize ([current-tvars (extend-env (cons dotted-var literal-tvars)
                                                             (cons (make-Dotted (make-F dotted-var)) new-tvars)
                                                             (current-tvars))])
                     (tc/mono-lambda formals bodies #f))])
          (ret (make-PolyDots (append literal-tvars (list dotted-var)) ty)))]
       [tvars
        (let* ([literal-tvars tvars]
               [new-tvars (map make-F literal-tvars)]
               [ty (parameterize ([current-tvars (extend-env literal-tvars new-tvars (current-tvars))])
                     (tc/mono-lambda formals bodies #f))])
          ;(printf "plambda: ~a ~a ~a ~n" literal-tvars new-tvars ty)
          (ret (make-Poly literal-tvars ty)))])]
    [_ 
     (unless (check-below (tc/plambda form formals bodies #f) expected)
       (tc-error/expr #:return (ret expected) "Expected a value of type ~a, but got a polymorphic function." expected))
     (ret expected)]))
    

;; form : a syntax object for error reporting
;; formals : the formal arguments to the loop
;; body : a block containing the body of the loop
;; name : the name of the loop
;; args : the types of the actual arguments to the loop
;; ret : the expected return type of the whole expression
(define (tc/rec-lambda/check form formals body name args ret)
  (with-lexical-env/extend
   (syntax->list formals) args
   (let ([t (->* args ret)])
     (with-lexical-env/extend
      (list name) (list t)
      (begin (tc-exprs/check (syntax->list body) ret)
             (make-Function (list t)))))))

;(trace tc/mono-lambda)


