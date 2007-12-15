(module contract-opt mzscheme
  (require "contract-guts.ss"
           (lib "stxparam.ss")
           (lib "etc.ss"))
  (require-for-syntax "contract-opt-guts.ss"
                      (lib "etc.ss")
                      (lib "stxparam.ss")
                      (lib "list.ss"))
  
  (provide opt/c define-opt/c define/opter opt-stronger-vars-ref)
  
  ;; define/opter : id -> syntax
  ;;
  ;; Takes an expression which is to be expected of the following signature: 
  ;;
  ;; opter : id id syntax list-of-ids ->
  ;;         syntax syntax-list syntax-list syntax-list (union syntax #f) (union syntax #f) syntax
  ;;         
  ;;
  ;; It takes in an identifier for pos, neg, and the original syntax. An identifier
  ;; that can be used to call the opt/i function is also implicitly passed into
  ;; every opter. A list of free-variables is implicitly passed if the calling context
  ;; was define/osc otherwise it is null.
  ;;
  ;; Every opter needs to return:
  ;;  - the optimized syntax
  ;;  - lifted variables: a list of (id, sexp) pairs
  ;;  - super-lifted variables: functions or the such defined at the toplevel of the
  ;;                            calling context of the opt routine.
  ;;                            Currently this is only used for struct contracts.
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
  ;;  - a list of stronger-ribs
  (define-syntax (define/opter stx)
    (syntax-case stx ()
      [(_ (for opt/i opt/info stx) expr ...)
       (if (identifier? #'for)
           #'(begin
               (begin-for-syntax
                 (reg-opter!
                  #'for
                  (λ (opt/i opt/info stx)
                    expr ...)))
               #t)
           (error 'define/opter "expected opter name to be an identifier, got ~e" (syntax-e #'for)))]))
  
  ;;
  ;; opt/unknown : opt/i id id syntax
  ;;
  (define-for-syntax (opt/unknown opt/i opt/info uctc)
    (let* ((lift-var (car (generate-temporaries (syntax (lift)))))
           (partial-var (car (generate-temporaries (syntax (partial)))))
           (partial-flat-var (car (generate-temporaries (syntax (partial-flat))))))
      (values
       (with-syntax ((partial-var partial-var)
                     (lift-var lift-var)
                     (uctc uctc)
                     (val (opt/info-val opt/info)))
         (syntax (partial-var val)))
       (list (cons lift-var 
                   ;; FIXME needs to get the contract name somehow
                   (with-syntax ((uctc uctc))
                     (syntax (coerce-contract 'opt/c uctc)))))
       null
       (list (cons
              partial-var
              (with-syntax ((lift-var lift-var)
                            (pos (opt/info-pos opt/info))
                            (neg (opt/info-neg opt/info))
                            (src-info (opt/info-src-info opt/info))
                            (orig-str (opt/info-orig-str opt/info)))
                (syntax (((proj-get lift-var) lift-var) pos neg src-info orig-str))))
             (cons
              partial-flat-var
              (with-syntax ((lift-var lift-var))
                (syntax (if (flat-pred? lift-var)
                            ((flat-get lift-var) lift-var)
                            (lambda (x) (error 'opt/unknown "flat called on an unknown that had no flat pred ~s ~s"
                                               lift-var
                                               x)))))))
       (with-syntax ([val (opt/info-val opt/info)]
                     [partial-flat-var partial-flat-var])
         #'(partial-flat-var val))
       lift-var
       null)))
  
  ;;
  ;; opt/recursive-call
  ;;
  ;; BUG: currently does not try to optimize the arguments, this requires changing
  ;;      every opter to keep track of bound variables.
  ;;
  (define-for-syntax (opt/recursive-call opt/info stx)
    (values
     (with-syntax ((stx stx)
                   (val (opt/info-val opt/info))
                   (pos (opt/info-pos opt/info))
                   (neg (opt/info-neg opt/info))
                   (src-info (opt/info-src-info opt/info))
                   (orig-str (opt/info-orig-str opt/info)))
       (syntax (let ((ctc stx))
                 ((((proj-get ctc) ctc) pos neg src-info orig-str) val))))
     null
     null
     null
     #f
     #f
     null
     null))
  
  ;; make-stronger : list-of-(union syntax #f) -> syntax
  (define-for-syntax (make-stronger strongers)
    (let ((filtered (filter (λ (x) (not (eq? x #f))) strongers)))
      (if (null? filtered)
          #t
          (with-syntax (((stronger ...) strongers))
            (syntax (and stronger ...))))))

  ;; opt/c : syntax -> syntax
  ;; opt/c is an optimization routine that takes in an sexp containing
  ;; contract combinators and attempts to "unroll" those combinators to save
  ;; on things such as closure allocation time.
  (define-syntax (opt/c stx)
    
    ;; opt/i : id opt/info syntax ->
    ;;         syntax syntax-list syntax-list (union syntax #f) (union syntax #f)
    (define (opt/i opt/info stx)
      (syntax-case stx (if)
        [(ctc arg ...)
         (and (identifier? #'ctc) (opter #'ctc))
         ((opter #'ctc) opt/i opt/info stx)]
        [argless-ctc
         (and (identifier? #'argless-ctc) (opter #'argless-ctc))
         ((opter #'argless-ctc) opt/i opt/info stx)]
        [(f arg ...)
         (and (identifier? #'f) 
              (syntax-parameter-value #'define/opt-recursive-fn)
              (module-identifier=? (syntax-parameter-value #'define/opt-recursive-fn)
                                   #'f))
         (values
          #`(#,(syntax-parameter-value #'define/opt-recursive-fn) #,(opt/info-val opt/info) arg ...)
          null
          null
          null
          #f
          #f
          null)]
        [else
         (opt/unknown opt/i opt/info stx)]))
    
    (syntax-case stx ()
      [(_ e) #'(opt/c e ())]
      [(_ e (opt-recursive-args ...))
       (let*-values ([(info) (make-opt/info #'ctc
                                            #'val
                                            #'pos
                                            #'neg
                                            #'src-info
                                            #'orig-str
                                            (syntax->list #'(opt-recursive-args ...))
                                            #f
                                            #f
                                            #'this
                                            #'that)]
                     [(next lifts superlifts partials _ __ stronger-ribs) (opt/i info #'e)])
         (with-syntax ([next next])
           (bind-superlifts
            superlifts
            (bind-lifts
             lifts
             #`(make-opt-contract
                (λ (ctc)
                  (λ (pos neg src-info orig-str)
                    #,(if (syntax-parameter-value #'define/opt-recursive-fn)
                          (with-syntax ([f (syntax-parameter-value #'define/opt-recursive-fn)])
                            (bind-superlifts
                             (cons
                              (cons (syntax-parameter-value #'define/opt-recursive-fn)
                                    #'(λ (val opt-recursive-args ...) next))
                              partials)
                             #'(λ (val) 
                                 (f val opt-recursive-args ...))))
                          (bind-superlifts
                           partials
                           #`(λ (val) next)))))
                (λ () e)
                (λ (this that) #f)
                (vector)
                (begin-lifted (box #f)))))))]))

  (define-syntax-parameter define/opt-recursive-fn #f)
  
  (define-syntax (define-opt/c stx)
    (syntax-case stx ()
      [(_ (id args ...) body)
       #'(define (id args ...)
           (syntax-parameterize ([define/opt-recursive-fn #'id])
                                (opt/c body (args ...))))]))
    
  ;; optimized contracts
  ;;
  ;; getting the name of an optimized contract is slow, but it is only
  ;; called when blame is raised (thankfully).
  ;;
  ;; note that lifts, partials, flat, and unknown are all built into the
  ;; projection itself and should not be exposed to the outside anyhow.
  (define-values (orig-ctc-prop orig-ctc-pred? orig-ctc-get)
    (make-struct-type-property 'original-contract))
  
  (define-struct/prop opt-contract (proj orig-ctc stronger stronger-vars stamp)
    ((proj-prop (λ (ctc) ((opt-contract-proj ctc) ctc)))
     ;; I think provide/contract and contract calls this, so we are in effect allocating
     ;; the original once 
     (name-prop (λ (ctc) (contract-name ((orig-ctc-get ctc) ctc))))
     (orig-ctc-prop (λ (ctc) ((opt-contract-orig-ctc ctc))))
     (stronger-prop (λ (this that)
                      (and (opt-contract? that)
                           (eq? (opt-contract-stamp this) (opt-contract-stamp that))
                           ((opt-contract-stronger this) this that))))))
  
  ;; opt-stronger-vars-ref : int opt-contract -> any
  (define (opt-stronger-vars-ref i ctc)
    (let ((v (opt-contract-stronger-vars ctc)))
      (vector-ref v i))))
