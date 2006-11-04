(module contract-opt mzscheme
  (require "contract-guts.ss"
           "contract-opt-guts.ss")
  (require-for-syntax "contract-opt-guts.ss"
                      (lib "list.ss"))
  
  (provide opt/c define/opter)
  
  ;; define/opter : id -> syntax
  ;;
  ;; Takes an expression which is to be expected of the following signature: 
  ;;
  ;; opter : id id syntax ->
  ;;         syntax syntax-list syntax-list (union syntax #f) (union syntax #f)
  ;;
  ;; It takes in an identifier for pos, neg, and the original syntax. An identifier
  ;; that can be used to call the opt/i function is also implicitly passed into
  ;; every opter.
  ;;
  ;; Every opter needs to return:
  ;;  - the optimized syntax
  ;;  - lifted variables: a list of (id, sexp) pairs
  ;;  - partially applied contracts: a list of (id, sexp) pairs
  ;;  - if the contract being optimized is flat,
  ;;    then an sexp that evals to bool,
  ;;    else #f
  ;;    This is used in conjunction with optimizing flat contracts into one boolean
  ;;    expression when optimizing or/c.
  ;;  - if the contract can be optimized,
  ;;    then #f (that is, it is not unknown)
  ;;    else the symbol of the lifted variable
  ;;    This is used for contracts with subcontracts (like cons) doing checks.
  (define-syntax (define/opter stx)
    (syntax-case stx ()
      [(_ (for opt/i pos neg stx) expr ...)
       (if (identifier? #'for)
           #'(begin
               (begin-for-syntax
                 (reg-opter!
                  'for
                  (λ (opt/i pos neg stx)
                    expr ...)))
               #t)
           (error 'define/opter "expected opter name to be an identifier, got ~e" (syntax-e #'for)))]))
  
  ;; opt/c : syntax -> syntax
  ;; opt/c is an optimization routine that takes in an sexp containing
  ;; contract combinators and attempts to "unroll" those combinators to save
  ;; on things such as closure allocation time.
  (define-syntax (opt/c stx)
    
    ;; opt/i : id id syntax ->
    ;;         syntax syntax-list syntax-list (union syntax #f) (union syntax #f)
    (define (opt/i pos neg stx)
      (syntax-case stx ()
        [(ctc arg ...)
         (and (identifier? #'ctc) (opter #'ctc))
         ((opter #'ctc) opt/i pos neg stx)]
        [argless-ctc
         (and (identifier? #'argless-ctc) (opter #'argless-ctc))
         ((opter #'argless-ctc) opt/i pos neg stx)]
        [else
         (if (opter 'unknown)
             ((opter 'unknown) opt/i pos neg stx)
             (error 'opt/c "opt libraries not loaded properly"))]))
    
    (syntax-case stx ()
      [(_ e)
       (let-values ([(next lifts partials _ __) (opt/i #'pos #'neg #'e)])
         (with-syntax ((next next)
                       (lifts (make-lifts lifts))
                       (partials (make-lifts partials))
                       (stx stx)) 
           (syntax (make-opt-contract
                    (λ (ctc)
                      (let* lifts
                        (λ (pos neg src-info orig-str)
                          (let partials
                            (λ (val) next)))))
                    (λ () e)))))])))