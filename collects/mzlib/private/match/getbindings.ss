(module getbindings mzscheme
  (provide getbindings@)
  
  (require "coupling-and-binding.scm"
           "update-binding-counts.scm"           
           "render-helpers.ss"
           "render-sigs.ss"
           (lib "unit.ss"))
    
  (require-for-template mzscheme)
   
  (define-unit getbindings@
    (import render-test-list^)
    (export getbindings^)
    
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
    (define/opt (next-outer
                 p
                 ae ;; this is the actual expression
                 sf
                 bv
                 let-bound
                 kf
                 ks
                 cert
                 [stx (syntax '())])
      (next-outer-helper p ae sf bv let-bound 
                         (lambda (x) kf) (lambda (a b) ks) cert stx))
    
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
    (define/opt (next-outer-helper
                 p
                 ae ;; this is the actual expression
                 sf
                 bv
                 let-bound
                 kf-func
                 ks-func
                 cert
                 [stx (syntax '())])
      ;; right now this does not bind new variables
      (let ((rendered-list (render-test-list p ae cert stx)))
        ;; no need to reorder lists although I suspect that it may be
        ;; better to put shape tests first
        (update-binding-count rendered-list)
        ((couple-tests rendered-list ks-func kf-func let-bound) sf bv)))
    
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
    (define (create-test-func p sf let-bound bind-map last-test cert)
      #`(lambda (exp)
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
                     #t)))
             cert)))
    
    ;;!(function getbindings
    ;;          (form (getbindings pat-syntax) -> list)
    ;;          (contract syntax -> list))
    ;; This function given a pattern returns a list of pattern
    ;; variable names which are found in the pattern.
    (define (getbindings pat-syntax cert)
      (let/cc out
        (next-outer
         pat-syntax
         (quote-syntax dummy)
         '()
         '()
         '()
         (lambda (sf bv) #'(dummy-symbol))
         (lambda (sf bv) (out (map car bv)))
         cert)))
    
    ;; end getbindings@
    )
  )