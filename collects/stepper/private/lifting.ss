(module lifting mzscheme
  (require (lib "etc.ss")
           (lib "contract.ss")
           (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "match.ss")
           "testing-shared.ss"
           "shared.ss"
           "my-macros.ss")

  (define-struct context-record (stx index kind))
  
  ; context-records are used to represent syntax context frames. That is,
  ; a list of context records represents a path through a syntax tree
  ; (to the highlight).

  (define-struct try-record (index try-fn expr))
  
  ; try-records are 
  (provide/contract [lift (syntax? ; syntax to perform lifiting in
                           boolean? ; lift-at-highlight?
                           . -> .
                           (listof syntax?))]) ; result
                          
  
  (define (lift stx lift-in-highlight?)
    (let*-2vals ([(context-records highlight) (find-highlight stx)])
      (lift-local-defs context-records highlight lift-in-highlight?)))
  
  ; [find-highlight (-> syntax? (listof context-record?))]

  (define (find-highlight stx)
    (let/ec success-escape
      (local
          ((define (make-try-all-subexprs stx kind context-so-far)
             (lambda (index-mangler list-of-subtries)
               (let loop ([index 0] [remaining list-of-subtries])
                 (unless (null? remaining)
                   (let* ([try (car remaining)]
                          [corrected-index (index-mangler index)])
                   ((car try) (cadr try) (cons (make-context-record stx corrected-index kind) context-so-far))
                   (loop (+ index 1) (cdr remaining)))))))
           
           (define try->offset-try
             (lambda (try)
               (lambda (offset subtries)
                 (try (lambda (index) (list (+ offset index))) subtries))))
           
           (define (top-level-expr-iterator stx context-so-far)
             (let ([try (try->offset-try (make-try-all-subexprs stx 'top-level context-so-far))])
               (kernel:kernel-syntax-case stx #f
                 [(module identifier name (#%plain-module-begin . module-level-exprs))
                  (try 3 (map (lambda (expr) `(,module-level-expr-iterator ,expr))
                              (syntax->list #'module-level-exprs)))]
                 [else-stx
                  (general-top-level-expr-iterator stx context-so-far)])))
           
           (define (module-level-expr-iterator stx context-so-far)
             (kernel:kernel-syntax-case stx #f
               [(provide . provide-specs)
                (void)]
               [else-stx
                (general-top-level-expr-iterator stx context-so-far)]))
           
           (define (general-top-level-expr-iterator stx context-so-far)
             (let ([try (try->offset-try (make-try-all-subexprs stx 'general-top-level context-so-far))])
               (kernel:kernel-syntax-case stx #f
                 [(define-values (var ...) expr)
                  (try 2 `((,expr-iterator ,#'expr)))]
                 [(define-syntaxes (var ...) expr)
                  (try 2 `((,expr-iterator ,#'expr)))]
                 [(begin . top-level-exprs)
                  (try 1 (map (lambda (expr) `(,top-level-expr-iterator ,expr))
                              (syntax->list #'exprs)))]
                 [(require . require-specs)
                  (void)]
                 [(require-for-syntax . require-specs)
                  (void)]
                 [else
                  (expr-iterator stx context-so-far)])))
           
           (define (expr-iterator stx context-so-far)
             (when (syntax-property stx 'stepper-highlight)
               (success-escape (2vals context-so-far stx)))
             (let* ([try (make-try-all-subexprs stx 'expr context-so-far)]
                    [try-exprs (lambda (index-mangler exprs) (try index-mangler (map (lambda (expr) (list expr-iterator expr)) 
                                                                                     (syntax->list exprs))))]
                    [try-exprs-offset (try->offset-try try-exprs)] 
                    [let-values-abstraction
                     (lambda (stx)
                       (kernel:kernel-syntax-case stx #f
                         [(kwd (((variable ...) rhs) ...) . bodies)
                          (begin
                            (try-exprs (lambda (index) (list 1 index 1)) #'(rhs ...))
                            (try-exprs-offset 2 #'bodies))]
                         [else
                          (error 'expr-syntax-object-iterator 
                                 "unexpected let(rec) expression: ~a"
                                 (syntax-object->datum stx))]))]) 
               (kernel:kernel-syntax-case stx #f
                 [var-stx
                  (identifier? (syntax var-stx))
                  (void)]
                 [(lambda vars . bodies)
                  (try-exprs-offset 2 #'bodies)]
                 [(case-lambda (vars . bodies) ...)
                  (let loop ([count 1] [clauses (syntax->list #'(bodies ...))])
                    (unless (null? clauses)
                      (try-exprs (lambda (index) (list count (+ index 1))) (cdar clauses))
                      (loop (+ count 1) (cdr clauses))))]
                 [(if test then)
                  (try-exprs-offset 1 #'(test then))]
                 [(if test then else)
                  (try-exprs-offset 1 #'(test then else))]
                 [(begin . bodies)
                  (try-exprs-offset 1 #'bodies)]
                 [(begin0 . bodies)
                  (try-exprs-offset 1 #'bodies)]
                 [(let-values . _)
                  (let-values-abstraction stx)]
                 [(letrec-values . _)
                  (let-values-abstraction stx)]
                 [(set! var val)
                  (try-exprs-offset 2 #'(val))]
                 [(quote _)
                  (void)]
                 [(quote-syntax _)
                  (void)]
                 [(with-continuation-mark key mark body)
                  (try-exprs-offset 1 #'(key mark body))]
                 [(#%app . exprs)
                  (try-exprs-offset 1 #'exprs)]
                 [(#%datum . _)
                  (void)]
                 [(#%top . var)
                  (void)]
                 [else
                  (error 'expr-iterator "unknown expr: ~a" 
                         (syntax-object->datum stx))]))))
        
        (begin (top-level-expr-iterator stx null)
               (error 'find-highlight "couldn't find highlight-placeholder in expression: ~v" (syntax-object->datum stx))))))

  ; TESTING:
  
  (define-syntax (test-begin stx)
    (syntax-case stx ()
      [(_ expr ...)
       ;#'(begin expr ...) ; testing version
       #'(void) ; non-testing version
       ]))
  
  (define (datum-ize-context-record cr)
     (list (syntax-object->datum (context-record-stx cr))
                          (context-record-index cr)
                          (context-record-kind cr)))
  
  (test-begin (require (lib "mz-testing.ss" "tests" "utils")))
  
  (test-begin (section 'stepper-lifting))

  (test-begin
   ; TEST OF FIND-HIGHLIGHT
   
   
   (define test-datum (expand (car (build-stx-with-highlight
                                    `((define (f x) (letrec ([a (lambda (x) (b (- x 1)))]
                                                             [b (lambda (x) ((hilite a) x))])
                                                      (a x))))))))

   (define expected (list (list `(#%app a x) '(1) 'expr)
                          (list `(lambda (x) (#%app a x)) '(2) 'expr)
                          (list `(letrec-values ([(a) (lambda (x) (#%app b (#%app (#%top . -) x (#%datum . 1))))] [(b) (lambda (x) (#%app a x))]) (#%app a x)) '(1 1 1) 'expr)
                          (list `(lambda (x) (letrec-values ([(a) (lambda (x) (#%app b (#%app (#%top . -) x (#%datum . 1))))] [(b) (lambda (x) (#%app a x))]) (#%app a x))) '(2) 'expr)                 
                          (list `(define-values (f) (lambda (x) (letrec-values ([(a) (lambda (x) (#%app b (#%app (#%top . -) x (#%datum . 1))))] [(b) (lambda (x) (#%app a x))]) (#%app a x)))) '(2)
                                               'general-top-level)))
   
   (let*-2vals ([(context-records highlight) (find-highlight test-datum)])
     (test expected map datum-ize-context-record context-records))
   
   
   (test null (lambda () 
                (let*-2vals ([(context-records dc) 
                              (find-highlight (car (build-stx-with-highlight `((hilite foo)))))])
                  context-records))))
  
  ; substitute-in-syntax takes a syntax expression (which must be a proper syntax list) and a path
  ; (represented by a list of numbers) and a syntax-expression to insert.  If the path is null, the
  ; 'to-insert' expression is returned.  Otherwise, the nth element of the syntax-list is replaced
  ; by the recursive call with the nth element, the rest of the path, and the to-insert, where n is
  ; the first number in the list.

  (define (substitute-in-syntax src path to-insert)
    (if (null? path)
        to-insert
        (let* ([opened (syntax->list src)]
               [n (car path)])
          (when (>= n (length opened))
            (error 'substitute-in-syntax "given an n (~v) which is larger than the length of the source sytax ~v" n (syntax-object->datum src)))
          (datum->syntax-object
           src
           (let loop ([n n] [left opened])
            (if (= n 0) 
                (cons (substitute-in-syntax (car left) (cdr path) to-insert)
                      (cdr left))
                (cons (car left) 
                      (loop (- n 1) (cdr left)))))
           src
           src))))
  
  (define (n-times n fn arg)
    (if (= n 0)
        arg
        (fn (n-times (- n 1) fn arg))))
  
  (test-begin
   
   (local
       ((define expected '(let-values ([(a) (lambda (x) 'bar)]) (a)))
        (define actual (syntax-object->datum (substitute-in-syntax #'(let-values ([(a) (lambda (x) 'foo)]) (a)) '(1 0 1 2 1) #'bar))))
     (printf "equal? ~v\n" (equal? expected actual))))
  
  
  ; lift-local-defs takes a list of contexts and an instruction and works its way out, reconstructing the expression.
  ; the main action of lift-local-defs is on let-values and letrec-values, where (after performing the substitution)
  ; the binding clauses are lifted into top-level defs.
  ; [lift-local-defs (->* ((listof context-record?) syntax?)
  ;                       ((listof syntax?) (listof syntax?)))]
  
  (define (lift-local-defs ctx-list highlighted lift-in-highlighted?)
    (let-values ([(highlighted-defs highlighted-body) (if lift-in-highlighted?
                                                          (lift-helper highlighted #f null)
                                                          (values null highlighted))])
      (let loop ([ctx-list ctx-list]
                 [so-far-defs (map (lambda (x) (syntax-property x 'stepper-highlight #t)) 
                                   highlighted-defs)]
                 [body (syntax-property highlighted-body 'stepper-highlight #t)])
        (if (null? ctx-list)
            (append so-far-defs (list body))
            (let*-values ([(ctx) (car ctx-list)]
                          [(index) (context-record-index ctx)]
                          [(next-defs next-body) 
                           (lift-helper (substitute-in-syntax (context-record-stx ctx) index body)
                                        index
                                        so-far-defs)])
              (loop (cdr ctx-list) next-defs next-body))))))
  
  ; lift-helper takes a syntax object and a split path and a list of syntax objects and breaks it up
  ; iff its a let/rec, wrapping those before the split and those after the split around the list of syntax
  ; objects
  ;  (->* (syntax? (union false? (listof number?)) (listof syntax?)) ((listof syntax?) syntax?))
  (define (lift-helper stx path so-far-defs)
    (let* ([lift
            (lambda ()
              (kernel:kernel-syntax-case stx #f
                [(tag ([(var ...) rhs] ...) body ...)
                 (let* ([defns (syntax->list #'((define-values (var ...) rhs) ...))]
                        [bodies-list (syntax->list #'(body ...))]
                        [body (if (= (length bodies-list) 1) ; as far as I can tell, this source info is comprehensively lost.
                                  (car bodies-list)
                                  #'(values body ...))])
                   (cond [(or (not path) (and (= (length path) 1)
                                              (> (car path) 1)))
                          (values (append defns so-far-defs) body)]
                         [(match path [`(1 ,n 1) n]) =>
                          (lambda (n)
                            (values (append (sublist 0 n defns) so-far-defs (sublist n (length defns) defns))
                                    body))]))]
                [else (error 'lift-helper "let or letrec does not have expected shape: ~v\n" (syntax-object->datum stx))]))])
    (kernel:kernel-syntax-case stx #f
      [(let-values . dc)
       (not (eq? (syntax-property stx 'user-stepper-hint) 'comes-from-or))
       (lift)]
      [(letrec-values . dc)
       (lift)]
      [else (values so-far-defs stx)])))
  
  (test-begin
   (local 
       ((define actual-stxs
          (lift-local-defs
            (list (make-context-record #'(dc 14) '(0) 'expr)
                  (make-context-record #'(letrec-values ([(a) 3] [(b) dc] [(c) 5]) (+ 3 4)) '(1 1 1) 'expr)
                  (make-context-record #'(f dc) '(1) 'expr))
            #'(let-values ([(a) 4] [(b) 9] [(c) 12]) (p q))
            #t))
        
        (define actual-sexps (map syntax-object->hilite-datum actual-stxs))
        
        (define expected-sexps
          (list '(define-values (a) 3)
                `(hilite (define-values (a) 4))
                `(hilite (define-values (b) 9))
                `(hilite (define-values (c) 12))
                `(define-values (b) ((hilite (p q)) 14))
                '(define-values (c) 5)
                '(f (+ 3 4)))))
     
     (test expected-sexps (lambda () actual-sexps))
     ;(printf "shared: ~v\n" (sexp-shared actual expected))
     )
   
   (report-errs)
   ))
 
