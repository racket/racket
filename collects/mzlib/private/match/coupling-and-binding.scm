
(module coupling-and-binding mzscheme
  ;; This library is used by match.ss
  
  (provide couple-tests meta-couple subst-bindings)
  
  (require "test-structure.scm"
	   "match-helper.ss"
	   (lib "pretty.ss")
           (lib "list.ss"))
  
  (require-for-template mzscheme)
  
  ;; a structure representing bindings of portions of the matched data
  ;; exp: the expression that is bound in s-exp form
  ;; exp-stx: the expression that is bound in syntax form
  ;; new-exp: the new symbol that will represent the expression
  (define-struct binding (exp exp-stx new-exp))
  
  ;;!(function couple-tests
  ;;          (form (couple-tests test-list ks-func kf-func let-bound)
  ;;                ->
  ;;                ((list list) -> syntax))
  ;;          (contract (list
  ;;                     ((((list list) -> syntax) list) -> 
  ;;                      ((list list) -> syntax))
  ;;                     (list -> ((list list) -> syntax))
  ;;                     list)
  ;;                    ->
  ;;                    ((list list) -> syntax)))
  ;; This is a major function of the compiler.  This function
  ;; couples a list of tests together.  Here is where state is
  ;; passed around to the various partially compiled tests so that
  ;; compilation can be completed.  This returns a function that takes a
  ;; list of tests so far and a list of bound pattern variables.
  (define (couple-tests test-list ks-func kf-func let-bound)
    ;(print-time "entering couple-tests")
    ;(printf "test-list: ~a~n" (map test-tst test-list))
    ;(printf "test-list size: ~a~n" (length test-list))
    (if (null? test-list)
        (ks-func (kf-func let-bound) let-bound)
        (let* ([cur-test (car test-list)]
               [rest-tests (cdr test-list)]
               ;; this couples together the rest of the test
               ;; it is passed a list of the already bound expressions
               ;; only used in test/rest
               [couple-rest (lambda (let-bound) 
                              (couple-tests rest-tests
                                            ks-func
                                            (if (negate-test? cur-test) 
                                                (lambda (let-bound)
                                                  (lambda (sf bv)
                                                    #`(match-failure)))
                                                kf-func)
                                            let-bound))]
               ;; this generates the current test as well as the rest of the match expression
               ;; it is passed a list of the already bound expressions
               [test/rest (lambda (let-bound)
                            ((test-comp cur-test)
                             (couple-rest let-bound)
                             (kf-func let-bound)
                             let-bound))])
          (if (and  
               ;; the expression is referenced twice
               (>= (test-bind-count cur-test) 2)
               ;; and it's not already bound to some variable
               (not (exp-already-bound?
                     (test-bind-exp cur-test)
                     let-bound)))               
              ;; then generate a new binding for this expression
              (let* ([new-exp (get-exp-var)]
                     [binding (make-binding (test-bind-exp cur-test)
                                            (test-bind-exp-stx cur-test)
                                            new-exp)]
                     [let-bound (cons binding let-bound)])
                (with-syntax (;; the new variable
                              [v new-exp]
                              ;; the expression being bound
                              ;; with appropriate substitutions for the already bound portions
                              [expr (sub-expr-subst (binding-exp-stx binding) let-bound)])
                  (lambda (sf bv)
                    #`(let ([v expr])
                        ;; the new body, using the new binding (through let-bound)
                        #,((test/rest let-bound) sf bv)))))
              
              ;; otherwise it doesn't need a binding, and we can just do the test
              (test/rest let-bound)))))
  
  ;;!(function subst-bindings
  ;;          (form (subst-bindings exp-stx let-bound) -> syntax)
  ;;          (contract (syntax list) -> syntax)
  ;;          (example (subst-bindings (syntax (car (cdr x))) 
  ;;                                   (list (list '(cdr x) 
  ;;                                                (syntax (cdr x)) 
  ;;                                                'exp5)))
  ;;                   -> (syntax (car 'exp5))))
  ;; This function substitutes let bound variables names for the
  ;; expressions that they represent.
  (define (subst-bindings exp-stx let-bound)    
    (cond [(get-bind exp-stx let-bound) => binding-new-exp]
          [else (sub-expr-subst exp-stx let-bound)]))    
  
  ;;!(function sub-exp-subst
  ;;          (form (sub-exp-subst exp-stx let-bound) -> syntax)
  ;;          (contract (syntax list) -> syntax)
  ;;          (example (subst-bindings (syntax (car (cdr x))) 
  ;;                                   (list (list '(cdr x) 
  ;;                                         (syntax (cdr x)) 
  ;;                                         'exp5)))
  ;;                   -> (syntax (car 'exp5))))
  ;; This function substitutes let bound variables names for the
  ;; expressions that they represent. This only works if a
  ;; subexpression of exp-stx is bound in the let-bound list.
  ;; This function assumes that all accessors are of the form
  ;; (acc obj other-args ...) (such as list-ref)
  (define (sub-expr-subst exp-stx let-bound)
    (syntax-case exp-stx ()
      [(access sub-exp rest ...)
       (let ([binding (get-bind #'sub-exp let-bound)])
         (if binding 
             #`(access #,(binding-new-exp binding) rest ...)
             #`(access #,(sub-expr-subst #'sub-exp let-bound) rest ...)))]
      [_ exp-stx]))
  
  ; helper for the following functions
  (define ((equal-bind-get exp) e)
    (equal? exp (binding-exp e)))
  
  ;;!(function get-bind
  ;;          (form (get-bind exp let-bound) -> binding)
  ;;          (contract (any list) -> list))
  ;; This function looks up the binding for a given expression exp
  ;; in the binding list let-bound.  If the binding is found then the
  ;; binding is returned if not then #f is returned.
  (define (get-bind exp let-bound)
    (cond [(memf (equal-bind-get (syntax-object->datum exp)) let-bound) => car]
          [else #f]))
  
  ;;!(function exp-already-bound?
  ;;          (form (exp-already-bound? exp let-bound) -> binding)
  ;;          (contract (any list) -> boolean))
  ;; This function looks up the binding for a given expression exp
  ;; in the binding list let-bound.  If the binding is found then #t
  ;; binding is returned if not then #f is returned.
  (define (exp-already-bound? exp let-bound)
    (ormap (equal-bind-get exp) let-bound))
  
  ;;!(function meta-couple
  ;;          (form (meta-couple rendered-list failure-func 
  ;;                             let-bound bvsf)
  ;;                ->
  ;;                ((list list) -> syntax))
  ;;          (contract (list ((list list) -> syntax) list list)
  ;;                    ->
  ;;                    ((list list) -> syntax)))
  ;; This function takes a list of rendered clauses which also have
  ;; success functions attached and couples the whole lot together
  ;; yeilding one function that when invoked will compile the whole
  ;; original match expression.
  (define (meta-couple rendered-list failure-func let-bound bvsf)
    #;(print-time "entering meta-couple")
    ;(printf "rendered-list ~n")
    ;(pretty-print (map (lambda (x) (map test-tst (car x))) rendered-list))
    (if (null? rendered-list)
        failure-func
        ;; here we erase the previously bound variables
        (let* ([failed 
                (lambda (let-bound)
                  (lambda (sf bv)
                    ((meta-couple (cdr rendered-list) 
                                  failure-func 
                                  let-bound 
                                  bvsf)
                     sf bvsf)))])
          (couple-tests (caar rendered-list)
                        (cdar rendered-list) ;; successfunc needs 
                        ;; failure method
                        failed ;; needs let-bound
                        let-bound ;; initial-let bindings
                        ))))      ;; fail-func
  
  (require (lib "trace.ss"))
  ;(trace meta-couple)
  ;(trace couple-tests)
  )