
(module coupling-and-binding mzscheme
  ;; This library is used by match.ss
  
  (provide couple-tests meta-couple subst-bindings)
  
  (require "test-structure.scm"
	   "match-helper.ss"
           (lib "list.ss"))
  
  (require-for-template mzscheme)
  
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
    (if (null? test-list)
        (ks-func (kf-func let-bound) let-bound)
        (let ([cur-test (car test-list)])
          (if (and (>= (test-bind-count cur-test) 2)
                   (not (exp-already-bound?
                         (test-bind-exp cur-test)
                         let-bound))) ;; if it is member of
              ;;let-bound skip it
              (let* ([new-exp (get-exp-var)]
                     [binding (list (test-bind-exp cur-test)
                                    (test-bind-exp-stx cur-test)
                                    new-exp)]
                     [let-bound (cons binding let-bound)]
                     [kf (kf-func let-bound)])
                (lambda (sf bv)
                  #`(let ((#,new-exp
                             #,(sub-expr-subst (bind-get-exp-stx binding)
                                               let-bound)))
                      #,(((test-comp (car test-list)) 
                          (couple-tests (cdr test-list)
                                        ks-func
                                        (if (negate-test? cur-test) 
                                            (lambda (let-bound)
                                              (lambda (sf bv)
                                                #`(match-failure)))
                                            kf-func)
                                        ;kf-func
                                        let-bound) 
                          kf let-bound) sf bv))))
              (let* ([kf (kf-func let-bound)])
                ((test-comp (car test-list)) 
                 (couple-tests (cdr test-list)
                               ks-func
                               (if (negate-test? cur-test) 
                                   (lambda (let-bound)
                                     (lambda (sf bv)
                                       #`(match-failure)))
                                   kf-func) 
                               ;kf-func
                               let-bound)
                 kf 
                 let-bound))))))
  
  ;;!(function bind-get-exp
  ;;          (form (bind-get-exp binding) -> exp)
  ;;          (contract binding -> exp))
  ;; This is just an accessor function for a binding.  This function
  ;; returns the expression that is bound in s-exp form.
  (define bind-get-exp car)         
  
  ;;!(function bind-get-exp-stx
  ;;          (form (bind-get-exp-stx binding) -> exp)
  ;;          (contract binding -> exp))
  ;; This is just an accessor function for a binding.  This function
  ;; returns the expression that is bound in syntax form.
  (define bind-get-exp-stx  cadr)
  
  ;;!(function bind-get-new-exp
  ;;          (form (bind-get-new-exp binding) -> exp)
  ;;          (contract binding -> exp))
  ;; This is just an accessor function for a binding.  This function
  ;; returns the new symbol that will represent the expression.
  (define bind-get-new-exp caddr)         
  
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
    (define binding (get-bind exp-stx let-bound))
    (if binding
        (bind-get-new-exp binding)
        (sub-expr-subst exp-stx let-bound)))
  
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
  (define (sub-expr-subst exp-stx let-bound)
    (syntax-case exp-stx ()
      [(access sub-exp rest ...)
       (let ([binding (get-bind #'sub-exp let-bound)])
         ;;(write (syntax sub-exp))(newline) (write binding)(newline)
         (if binding 
             #`(access #,(bind-get-new-exp binding) rest ...)
             #`(access #,(sub-expr-subst #'sub-exp let-bound) rest ...)))]
      [_ exp-stx]))
  
  ; helper for the following functions
  (define ((equal-bind-get exp) e) 
    (equal? exp (bind-get-exp e)))
  
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
    (if (null? rendered-list)
        failure-func
        ;; here we erase the previously bound variables
        (let* ([failed 
                (lambda (let-bound)
                  (lambda (sf bv)
                    ((meta-couple (cdr rendered-list) 
                                  failure-func 
                                  let-bound 
                                  bvsf) sf bvsf)))])
          (couple-tests (caar rendered-list)
                        (cdar rendered-list) ;; successfunc needs 
                        ;; failure method
                        failed ;; needs let-bound
                        let-bound ;; initial-let bindings
                        ))))      ;; fail-func
  )