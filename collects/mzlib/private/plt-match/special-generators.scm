;; This library is used by match.ss

;;!(function or-gen
;;         (form (or-gen exp orpatlist stx sf bv ks kf let-bound)
;;               ->
;;               syntax)
;;         (contract (syntax list syntax list list (list list -> syntax) 
;;                    (list list -> syntax) list)
;;                   ->
;;                   syntax))
;; The function or-gen is very similar to the function gen except
;; that it is called when an or pattern is compiled.  An or
;; pattern is essentially the same as a match pattern with several
;; clauses.  The key differences are that it exists within a
;; larger pattern and the state of compilation has information
;; that will help optimaize its compilation.  And the success of
;; any pattern results in the same outcome.
(define or-gen
  (lambda (exp orpatlist stx sf bv ks kf let-bound)
    (let ((rendered-list
           (map
            (lambda (pat) 
              (cons (render-test-list pat exp stx)
                    (lambda (fail let-bound)
                      (lambda (sf bv)
                        (let ((bv (map
                                   (lambda (bind)
                                     (cons (car bind)
                                           (subst-bindings (cdr bind) 
                                                           let-bound)))
                                   bv)))
                          (ks sf bv))))))
            orpatlist)))
      (update-counts rendered-list)
      (update-binding-counts rendered-list)
      (let* ((rendered-list 
              (reorder-all-lists rendered-list)
              )
             (output ((meta-couple rendered-list kf let-bound bv) sf bv)))
        output))))

;;!(function next-outer
;;          (form (next-outer p ae sf bv let-bound kf ks syntax bool)
;;                ->
;;                syntax)
;;          (contract (syntax syntax list list list (list list -> syntax)
;;                     (list list -> syntax) syntax bool)
;;                    ->
;;                    syntax))
;; The function next-outer is basically a throw-back to the next
;; function of the original match compiler.  It compiles a pattern
;; or sub-pattern of a clause and does not yield a list of
;; partially compiled test structs.  This function is called
;; inside of test constructs that cannot be eliminated because of
;; a related presence in the test-so-far list.  So, instead of
;; partially compiling patterns this function fully compiles patterns.
(define next-outer
  (opt-lambda (p
               ae ;; this is the actual expression
               sf
               bv
               let-bound
               kf
               ks
               [stx (syntax '())]
               [opt #f])
    (next-outer-helper p ae sf bv let-bound 
                       (lambda (x) kf) (lambda (a b) ks) stx opt)))

;;!(function next-outer-helper
;;          (form (next-outer p ae sf bv let-bound kf-func ks-func syntax bool)
;;                ->
;;                syntax)
;;          (contract (syntax syntax list list list (list list -> syntax)
;;                     (list list -> syntax) syntax bool)
;;                    ->
;;                    syntax))
;; The function next-outer-helper contains the meat of next-outer
;; and allows the programmer to pass higher order functions
;; ks-func and kf-func that will be given compile time imformation
;; about let-bindings etc. which in turn will allow the programmer
;; to take advantage of this info.
(define next-outer-helper
  (opt-lambda (p
               ae ;; this is the actual expression
               sf
               bv
               let-bound
               kf-func
               ks-func
               [stx (syntax '())]
               [opt #f])
    ;; right now this does not bind new variables
    (let ((rendered-list (render-test-list p ae stx)))
      ;; no need to reorder lists although I suspect that it may be
      ;; better to put shape tests first
      (update-binding-count rendered-list)
      ((couple-tests rendered-list ks-func kf-func let-bound) sf bv))))

;;!(function create-test-func
;;          (form (create-test-func p sf let-bound bind-map last-test)
;;                ->
;;                syntax)
;;          (contract (syntax list list a-list bool) -> syntax))
;; This function creates a runtime function that is used as an
;; individual test in a list of tests for the list-no-order
;; pattern.
;; <pre>
;; bindmap - a-list of bindings mapped to their expressions
;; last-test - a boolean value that indicates whether this function
;; is collecting one value or a list of values.</pre>
(define (create-test-func p sf let-bound bind-map last-test)
  (quasisyntax/loc
   p
   (lambda (exp)
     #,(next-outer-helper 
        p #'exp sf '() let-bound
        (lambda (let-bound)
          (lambda (sf bv)
            #'#f))
        (lambda (fail let-bound)
          (lambda (sf bv)
            #`(begin
                #,@(map (lambda (bind)
                          (let ((binding-name (get-bind-val (car bind) bind-map))
                                (exp-to-bind 
                                 (subst-bindings (cdr bind) let-bound)))
                            (if last-test
                                #`(set! #,binding-name
                                        (cons #,exp-to-bind #,binding-name))
                                #`(set! #,binding-name
                                        #,exp-to-bind))))
                        bv)
                #t)))))))

;;!(function getbindings
;;          (form (getbindings pat-syntax) -> list)
;;          (contract syntax -> list))
;; This function given a pattern returns a list of pattern
;; variable names which are found in the pattern.
(define (getbindings pat-syntax)
  (let/cc out
          (next-outer
           pat-syntax
           (quote-syntax dummy)
           '()
           '()
           '()
           (lambda (sf bv) #'(dummy-symbol))
           (lambda (sf bv) (out (map car bv))))))
