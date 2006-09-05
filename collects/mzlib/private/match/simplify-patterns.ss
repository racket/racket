(module simplify-patterns mzscheme
  
  (require (lib "stx.ss" "syntax"))
  (require (rename (lib "1.ss" "srfi") map-append append-map))
  
  (require "match-error.ss"
           "match-helper.ss"
           "test-structure.scm"
           "coupling-and-binding.scm"
           "update-counts.scm"
           "update-binding-counts.scm"
           "reorder-tests.scm"
           "match-expander-struct.ss"
           "render-helpers.ss")
  
  (require "render-sigs.ss"
           (lib "unitsig.ss"))
  
  (require-for-syntax "match-helper.ss"
                      "match-expander-struct.ss"
                      "test-no-order.ss")
  
  (require-for-template mzscheme
			"match-error.ss"
			"test-no-order.ss"
                        "match-helper.ss")
  
  (provide simplify)
  
  ;; simplifies patterns by removing syntactic sugar and expanding match-expanders
  ;; simplify : syntax certifier-> syntax
  (define (simplify stx cert)
    (define (simplify/i stx) (simplify stx cert))
    (syntax-case*
        stx
      (_ list quote quasiquote vector box ? app and or not struct set! var
         list-rest get! ... ___ unquote unquote-splicing cons
         list-no-order hash-table regexp pregexp cons) stx-equal?

      ;; expand match-expanders
      ;; this doesn't work because we need to keep the certifier around
      [(expander args ...)
       (and (identifier? #'expander) 
            (match-expander? (syntax-local-value (cert #'expander) (lambda () #f))))
       (let* ([expander (syntax-local-value (cert #'expander))]
              [transformer (match-expander-plt-match-xform expander)])
         (unless transformer
           (match:syntax-err #'expander
                             "This expander only works with the match.ss library."))
         (let* ([introducer (make-syntax-introducer)]
                [certifier (match-expander-certifier expander)]
                [result (introducer (transformer (introducer stx)))]
                [cert* (lambda (id) (certifier (cert id) #f introducer))])
           (simplify result cert*)))]
      
      ;; label variable patterns
      [id
       (and (pattern-var? #'id) (not (stx-dot-dot-k? #'id)))
       #'(var id)]
      
      ;; match the empty list
      ['() (syntax/loc stx (list))]
      
      ;; other quoted data is untransformed
      [(quote data) stx]
      
      ;; transform quasi-patterns into regular patterns
      [`quasi-pat (simplify (parse-quasi #'quasi-pat))]
      
      ;; predicate patterns with binders are redundant with and patterns
      [(? pred pat . pats) (simplify/i (syntax/loc stx (and (? pred) pat . pats)))]
      [(? pred) (quasisyntax/loc stx (? #,(cert #'pred)))]
      
      ;; regexp patterns - FIXME: abstract here
      [(regexp re) (simplify (syntax/loc stx (and (? string?) (? (lambda (x) (regexp-match re x))))))]
      [(pregexp re) (simplify (syntax/loc stx (and (? string?) (? (lambda (x) (pregexp-match-with-error re x))))))]
      [(regexp re pat) (simplify (syntax/loc stx (and (? string?) (app (lambda (x) (regexp-match re x)) pat))))]
      [(pregexp re pat) (simplify (syntax/loc stx (and (? string?) (app (lambda (x) (pregexp-match-with-error re x)) pat))))]
      [(regexp . re) (match:syntax-err stx "regexp pattern must have one or two subpatterns")]
      [(pregexp . re) (match:syntax-err stx "pregexp pattern must have one or two subpatterns")]
     
      
      ;; cons is just list-rest with 2 arguments
      [(cons p1 p2) (simplify (syntax/loc stx (list-rest p1 p2)))]
      [(cons . rest) (match:syntax-err stx "cons pattern must have exactly two subpatterns")]
      
      ;; aggregates
      [(kw pats ...)
       (memq (syntax-e #'kw) '(list vector list-rest list-no-order and or not))
       (quasisyntax/loc stx (kw #,@(syntax-map simplify #'(pats ...))))]
      [(kw pats ... . rest)
       (match:syntax-err stx (format "~a pattern must have a proper list of subpatterns" (syntax-e #'kw)))]
      
      ;; hash table patterns have their own syntax
      [(hash-table (pat1 pat2) ...)
       (with-syntax ([(pat1* ...) (syntax-map simplify #'(pat1 ...))]
                     [(pat2* ...) (syntax-map simplify #'(pat2 ...))])
         (syntax/loc stx (hash-table (pat1* pat2*) ...)))]
      [(hash-table (pat1 pat2) ... ooo)
       (stx-dot-dot-k? #'ooo)
       (with-syntax ([(pat1* ...) (syntax-map simplify #'(pat1 ...))]
                     [(pat2* ...) (syntax-map simplify #'(pat2 ...))])
         (syntax/loc stx (hash-table (pat1* pat2*) ... ooo)))]      
      [(hash-table . rest) (match:syntax-err stx "syntax error in hash table pattern")]
      
      ;; struct patterns
      [(struct st (pats ...)) (with-syntax ([(pats* ...) (syntax-map simplify #'(pats ...))]
                                            [st* (cert #'st)])
                                (syntax/loc stx (struct st* (pats* ...))))]
      [(struct . rest) (match:syntax-err stx "syntax error in struct pattern")]
      
      [(box pat) (quasisyntax/loc stx (box #,(simplify #'pat)))]
      [(box . rest) (match:syntax-err stx "syntax error in box pattern")]
      
      [(app e pat) (quasisyntax/loc stx (app #,(cert #'e) #,(simplify #'pat)))]
      [(app . rest) (match:syntax-err stx "syntax error in app pattern")]
      
      [(set! id)
       (identifier? #'id)
       stx]
      [(set! . rest) (match:syntax-err stx "set! pattern must have one identifier")]

      [(get! id)
       (identifier? #'id)
       stx]
      [(get! . rest) (match:syntax-err stx "get! pattern must have one identifier")]
      
      [(var . rest) (match:internal-err stx "var pattern found before simplification!")]
      
      [_ stx])
    
    
    )
  
  
  )