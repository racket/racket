(module contract-opt-guts mzscheme
  (require "contract-guts.ss")
  
  (provide get-opter reg-opter! opter
           
           make-opt-contract
           orig-ctc-prop orig-ctc-pred? orig-ctc-get

           make-lifts interleave-lifts)
  
  (define-values (orig-ctc-prop orig-ctc-pred? orig-ctc-get)
    (make-struct-type-property 'original-contract))
  
  ;; optimized contracts
  ;;
  ;; getting the name of an optimized contract is slow, but it is only
  ;; called when blame is raised (thankfully).
  ;;
  ;; note that lifts, partials, flat, and unknown are all built into the
  ;; projection itself and should not be exposed to the outside anyhow.
  (define-struct/prop opt-contract (proj orig-ctc)
    ((proj-prop (λ (ctc) ((opt-contract-proj ctc) ctc)))
     (name-prop (λ (ctc) ((name-get ((orig-ctc-get ctc) ctc)) ((orig-ctc-get ctc) ctc))))
     (orig-ctc-prop (λ (ctc) ((opt-contract-orig-ctc ctc))))
     (stronger-prop (λ (this that)
                      #f)))) ;; TODO, how to do this?

  ;; a hash table of opters
  (define opters-table
    (make-hash-table 'equal))
  
  ;; get-opter : syntax -> opter
  (define (get-opter ctc)
    (hash-table-get opters-table ctc #f))
  
  ;; opter : (union symbol identifier) -> opter
  (define (opter ctc)
    (if (or (identifier? ctc) (symbol? ctc))
        (let ((key (if (syntax? ctc) (syntax-e ctc) ctc)))
          (get-opter key))
        (error 'opter "the argument must either be an identifier or a syntax object of an identifier, got ~e" ctc)))
  
  ;; reg-opter! : symbol opter ->
  (define (reg-opter! ctc opter)
    (hash-table-put! opters-table ctc opter))
  
  ;; make-lifts : list -> syntax
  ;; converts a list of lifted-var lifted-expr pairs into a syntax object
  ;; suitable for use in a let.
  (define (make-lifts lst)
    (map (λ (x) (with-syntax ((var (car x))
                              (e (cdr x)))
                  (syntax (var e)))) lst))
  
  ;; interleave-lifts : list list -> list
  ;; interleaves a list of variables names and a list of sexps into a list of
  ;; (var sexp) pairs.
  (define (interleave-lifts vars sexps)
    (if (= (length vars) (length sexps))
        (if (null? vars) null
            (cons (cons (car vars) (car sexps))
                  (interleave-lifts (cdr vars) (cdr sexps))))
        (error 'interleave-lifts "expected lists of equal length, got ~e and ~e" vars sexps))))