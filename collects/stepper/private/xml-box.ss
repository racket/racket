(module xml-box mzscheme
  (require (lib "contract.ss")
           "shared.ss"
           (prefix kernel: (lib "kerncase.ss" "syntax")))
  
  (provide/contract [rewrite-xml-box (syntax?                  ; stx to rewrite
                                      (syntax? . -> . syntax?) ; rewriter for non-xml-box subcomponents
                                      . -> . 
                                      syntax?)])               ; rewritten
  
  
  (define (rewrite-xml-box stx rewrite-other)
      
    (define (recur stx)
      (rewrite-xml-box stx rewrite-other))
    
    (define (rewrite-xml-error)
      (error 'rewrite-xml-box "unexpected syntax in expansion of xml box: ~e" stx))

    (case (syntax-property stx 'stepper-hint) 
      [(from-scheme-box from-splice-box) (rewrite-other stx)]
      [(from-xml-box #f)
       (syntax-property
        (kernel:kernel-syntax-case stx #f
          [var-stx (identifier? (syntax var-stx)) (rewrite-xml-error)]
          
          [(lambda . clause) (rewrite-xml-error)]
          
          [(case-lambda . clauses) (rewrite-xml-error)]
          
          [(if test then) (rewrite-xml-error)]
          
          [(if test then else) (rewrite-xml-error)]
          
          [(begin . bodies) (rewrite-xml-error)]
          
          [(begin0 . bodies) (rewrite-xml-error)]
          
          [(let-values . clause) (rewrite-xml-error)]
          
          [(letrec-values . clause) (rewrite-xml-error)]
          
          [(set! var val) (rewrite-xml-error)]
          
          [(quote _) stx]
          
          [(quote-syntax _) (rewrite-xml-error)]
          
          [(with-continuation-mark key mark body) (rewrite-xml-error)]
          
          [(#%app . exprs)
           (rebuild-stx (map recur (syntax->list #`exprs)) stx)]
          
          [(#%datum . _) stx]
          
          [(#%top . var) (rewrite-xml-error)]
          
          [else (error 'syntax-object-iterator "unknown expr: ~a" stx)])
        'stepper-hint
        'from-xml-box)]
      
      [else
       (error 'rewrite-xml-box "unexpected stepper-hint \"~v\" on syntax from xml box: ~e" 
              (syntax-property stx 'stepper-hint)
              stx)])))



