(module xml-box scheme/base
  (require mzlib/contract
           "shared.rkt"
           "syntax-property.rkt"
           (prefix-in kernel: syntax/kerncase))
  
  (provide/contract [rewrite-xml-box (syntax?                  ; stx to rewrite
                                      (syntax? . -> . syntax?) ; rewriter for non-xml-box subcomponents
                                      . -> . 
                                      syntax?)])               ; rewritten
  
  
  (define (rewrite-xml-box stx rewrite-other)
      
    (define (recur stx)
      (rewrite-xml-box stx rewrite-other))
    
    (define (rewrite-xml-error)
      (error 'rewrite-xml-box "unexpected syntax in expansion of xml box: ~.s" stx))

    (case (stepper-syntax-property stx 'stepper-hint) 
      [(from-scheme-box from-splice-box) (rewrite-other stx)]
      [(from-xml-box #f)
       (stepper-syntax-property
        (kernel:kernel-syntax-case stx #f
          [var-stx (identifier? (syntax var-stx)) (rewrite-xml-error)]
          
          [(#%plain-lambda . clause) (rewrite-xml-error)]
          
          [(case-lambda . clauses) (rewrite-xml-error)]
          
          [(if test then else) (rewrite-xml-error)]
          
          [(begin . bodies) (rewrite-xml-error)]
          
          [(begin0 . bodies) (rewrite-xml-error)]
          
          [(let-values . clause) (rewrite-xml-error)]
          
          [(letrec-values . clause) (rewrite-xml-error)]
          
          [(set! var val) (rewrite-xml-error)]
          
          [(quote _) stx]
          
          [(quote-syntax _) (rewrite-xml-error)]
          
          [(with-continuation-mark key mark body) (rewrite-xml-error)]
          
          [(#%plain-app . exprs)
           (rebuild-stx (map recur (syntax->list #`exprs)) stx)]
          
          [(#%top . var) (rewrite-xml-error)]
          
          [else (error 'syntax-object-iterator "unknown expr: ~a" stx)])
        'stepper-hint
        'from-xml-box)]
      
      [else
       (error 'rewrite-xml-box "unexpected stepper-hint ~e on syntax from xml box: ~.s"
              (stepper-syntax-property stx 'stepper-hint)
              stx)])))
