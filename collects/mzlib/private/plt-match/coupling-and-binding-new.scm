;; This library is used by match.ss

(define-values (couple-tests meta-couple subst-bindings)
  (letrec 
      (
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
       (couple-tests
        (lambda (test-list ks-func kf-func let-bound)
          (if (null? test-list)
              (ks-func (kf-func let-bound) let-bound)
              (let ((cur-test (car test-list)))
                (if (and (>= (test-bind-count cur-test) 2)
                         (not (exp-already-bound?
                               (test-bind-exp cur-test)
                               let-bound))) ;; if it is member of
                    ;;let-bound skip it
                    (let* ((new-exp (get-exp-var))
                           (binding (list (test-bind-exp cur-test)
                                          (test-bind-exp-stx cur-test)
                                          new-exp))
                           (let-bound (cons binding
                                            let-bound))
                           (kf (kf-func let-bound)))
                      (lambda (sf bv)
                        (let ((coup-res
                               (((test-comp (car test-list)) 
                                 (couple-tests (cdr test-list)
                                               ks-func
                                               (if (negate-test? cur-test) 
                                                   (lambda (let-bound)
                                                     (lambda (sf bv)
                                                       (quasisyntax/loc
                                                        (test-bind-exp-stx cur-test)
                                                        (match-failure))))
                                                   kf-func)
                                        ;kf-func
                                               let-bound) 
                                 kf let-bound) sf bv)))
                          (if (equal? (syntax-object->datum coup-res)
                                      '(match-failure))
                              coup-res
                              (quasisyntax/loc
                               (test-bind-exp-stx cur-test)
                               (let ((#,new-exp
                                      #,(sub-expr-subst (bind-get-exp-stx binding)
                                                        let-bound)))
                                 #,coup-res))))))
                    (let* ((kf (kf-func let-bound)))
                      ((test-comp (car test-list)) 
                       (couple-tests (cdr test-list)
                                     ks-func
                                     (if (negate-test? cur-test) 
                                         (lambda (let-bound)
                                           (lambda (sf bv)
                                             (quasisyntax/loc
                                              (test-bind-exp-stx cur-test)
                                              (match-failure))))
                                         kf-func) 
                                        ;kf-func
                                     let-bound) 
                       kf 
                       let-bound)))))))

       ;;!(function bind-get-exp
       ;;          (form (bind-get-exp binding) -> exp)
       ;;          (contract binding -> exp))
       ;; This is just an accessor function for a binding.  This function
       ;; returns the expression that is bound in s-exp form.
       (bind-get-exp 
        (lambda (binding)
          (car binding)))

       ;;!(function bind-get-exp-stx
       ;;          (form (bind-get-exp-stx binding) -> exp)
       ;;          (contract binding -> exp))
       ;; This is just an accessor function for a binding.  This function
       ;; returns the expression that is bound in syntax form.
       (bind-get-exp-stx 
        (lambda (binding)
         (cadr binding)))

       ;;!(function bind-get-new-exp
       ;;          (form (bind-get-new-exp binding) -> exp)
       ;;          (contract binding -> exp))
       ;; This is just an accessor function for a binding.  This function
       ;; returns the new symbol that will represent the expression.
       (bind-get-new-exp 
        (lambda (binding)
         (caddr binding)))

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
       (subst-bindings 
        (lambda (exp-stx let-bound)
          (let* ((exp (syntax-object->datum exp-stx))
                 (binding (get-bind exp let-bound)))
            (if binding
                (bind-get-new-exp binding)
                (sub-expr-subst exp-stx let-bound)))))

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
       (sub-expr-subst 
        (lambda (exp-stx let-bound)
          (syntax-case exp-stx ()
            ((access sub-exp rest ...)
             (let ((binding (get-bind 
                             (syntax-object->datum (syntax sub-exp)) 
                             let-bound)))
               (if binding
                   (quasisyntax/loc 
                    exp-stx (access #,(bind-get-new-exp binding) rest ...))
                   (quasisyntax/loc 
                    exp-stx (access #,(sub-expr-subst (syntax sub-exp) 
                                                      let-bound) 
                                    rest ...)))))
            (other (syntax other)))))

        ;;!(function get-bind
        ;;          (form (get-bind exp let-bound) -> binding)
        ;;          (contract (any list) -> list))
        ;; This function looks up the binding for a given expression exp
        ;; in the binding list let-bound.  If the binding is found then the
        ;; binding is returned if not then #f is returned.
        (get-bind 
         (lambda (exp let-bound)
           (cond ((null? let-bound) #f)
                 ((equal? exp (bind-get-exp (car let-bound))) (car let-bound))
                 (else (get-bind exp (cdr let-bound))))))

        ;;!(function exp-already-bound?
        ;;          (form (exp-already-bound? exp let-bound) -> binding)
        ;;          (contract (any list) -> list))
        ;; This function looks up the binding for a given expression exp
        ;; in the binding list let-bound.  If the binding is found then #t
        ;; binding is returned if not then #f is returned.
        (exp-already-bound? 
         (lambda (exp let-bound)
           (cond ((null? let-bound) #f)
                 ((equal? exp (bind-get-exp (car let-bound))) #t)
                 (else (exp-already-bound? exp (cdr let-bound))))))

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
        (meta-couple
         (lambda (rendered-list failure-func let-bound bvsf)
         ;; here we are going to tag the rendered-list before moving on
           (let* ((count 0)
                  (rendered-list 
                   (map (lambda (x)
                          (begin
                            (set! count (add1 count))
                            (cons count x)))
                        rendered-list)))
             (meta-couple-help rendered-list failure-func let-bound bvsf))))
        ;; so the shape of the data is ((index test-list . success-func) ...)
        (tests-index (lambda (x) (car x)))
        (tests-list (lambda (x) (cadr x)))
        (tests-succ-func (lambda (x) (cddr x)))
        (no-group? (lambda (x) (<= (length (test-used-set x)) 1)))

        (meta-couple-help
         (lambda (rendered-list failure-func let-bound bvsf)
            (cond ((null? rendered-list) failure-func)
                  ;; if the top list is null
                  ;; or the first item of the top list
                  ;; has no group associated with it
                  ;; or is a negate test 
                  ;; handle list normally
                  ((let ((tlist (tests-list (car rendered-list))))
                     (or (null? tlist)
                         (or (negate-test? (car tlist))
                             (no-group? (car tlist)))))
                   ;; here we erase the previously bound (bv) variables
                   (let* ((failed 
                           (lambda (let-bound)
                             (lambda (sf bv)
                               ((meta-couple-help (cdr rendered-list) 
                                                  failure-func 
                                                  let-bound 
                                                  bvsf) sf bvsf)))))
                     (couple-tests (tests-list (car rendered-list))
                                   (tests-succ-func (car rendered-list)) ;; successfunc needs 
                                   ;; failure method
                                   failed ;; needs let-bound
                                   let-bound ;; initial-let bindings
                                   )))
                  (else
                   (let ((upper-left-test
                          (car (tests-list (car rendered-list)))))
                     (let-values (((top-group remainder-of-list)
                                   (lift-out-group (test-used-set 
                                                    upper-left-test)
                                                   rendered-list)))
                       (let* ((failed
                               (lambda (let-bound)
                                 (lambda (sf bv)
                                   ((meta-couple-help remainder-of-list
                                                      failure-func
                                                      let-bound
                                                      bvsf) sf bvsf)))))
                         (couple-tests (list upper-left-test)
                                       (lambda (fail let-bound)
                                         (lambda (sf bv)
                                           ((meta-couple-help
                                             (cons
                                              (cons (tests-index (car top-group))
                                                    (cons
                                                     (cdr (tests-list (car top-group)))
                                                     (tests-succ-func (car top-group))))
                                              (map
                                               (lambda (tests)
                                                 (cons
                                                  (tests-index tests)
                                                  (cons 
                                                   (eliminate-test-from-list 
                                                    upper-left-test
                                                    (tests-list tests)
                                                    )
                                                   (tests-succ-func tests))))
                                               (cdr top-group)))
                                             fail
                                             let-bound
                                             bv)
                                            sf bv)))
                                       failed
                                       let-bound))))))))


        ;; returns a list containing the separated group and the 
        ;; the rest of the rendered list
        (lift-out-group
         (lambda (group-list rendered-list)
           (let loop ((rl rendered-list)
                      (k (lambda (grp rest)
                           (values grp rest))))
             (when (null? group-list) (error 'null-group-list))
             (if (null? rl)
                 (k '() '())
                 (if (member (caar rl) group-list)
                     (loop (cdr rl)
                           (lambda (g r)
                             (k (cons (car rl) g) 
                                r)))
                     (loop (cdr rl)
                           (lambda (g r)
                             (k g
                                (cons (car rl) r)))))))))
        
        (eliminate-test-from-list
         (lambda (test tl)
            (filter 
             (lambda (t)
               (or (action-test? t)
                   (not (in (test-tst t) (list (test-tst test))))))
             tl)))

        )
    (values couple-tests meta-couple subst-bindings)))