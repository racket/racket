(module contract-opt mzscheme
  (require "contract.ss"
           "contract-guts.ss"
           "contract-arrow.ss")
  (require-for-syntax "contract-opt-guts.ss"
                      (lib "list.ss"))
  
  (provide opt/c define/opter)
  
  ;; TODO document this
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
  ;; opt is an optimization routine that takes in an s-expression containing
  ;; contract combinators and attempts to "unroll" those combinators to save
  ;; on things such as closure allocation time.
  (define-syntax (opt/c stx)
    
    ;; opt/i : syntax syntax syntax -> syntax list-of-syntax list-of-syntax boolean-or-syntax known
    (define (opt/i pos neg stx)
      (syntax-case stx ()
        [(ctc arg ...)
         (and (identifier? #'ctc) (opter #'ctc))
         (begin
           ((opter #'ctc) opt/i pos neg stx))]
        [argless-ctc
         (and (identifier? #'argless-ctc) (opter #'argless-ctc))
         ;; FIXME computes pred? twice
         ((opter #'argless-ctc) opt/i pos neg stx)]
        [else
         (if (opter 'unknown)
             ((opter 'unknown) opt/i pos neg stx)
             (error 'opt/c "opt libraries not loaded properly"))]))
    
    (syntax-case stx ()
      [(_ e)
       (let-values ([(next lifted partials _ __) (opt/i #'pos #'neg #'e)])
         (with-syntax ((next next)
                       (lifted (make-lifted lifted))
                       (partials (make-lifted partials)))
           (syntax (let* lifted
                     (make-proj-contract
                      contract-name
                      (λ (pos neg src-info orig-str)
                        (let partials
                          (λ (val)
                            next)))
                      #f)))))])))