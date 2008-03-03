(module unit-utils mzscheme
  (require (lib "unit.ss"))
  (require-for-syntax (lib "unit-exptime.ss")
                      (lib "list.ss" "srfi" "1")
                      (lib "match.ss"))
  
  (provide define-values/link-units/infer)

  (define-syntax (define-values/link-units/infer stx)
    ;; construct something we can put in the imports/exports clause from the datum
    (define (datum->sig-elem d)
      (if (car d)
          (quasisyntax/loc (cdr d) (tag . #,(cdr d)))
          (cdr d)))
    
    ;; identifier -> (list (listof imports) (listof exports))
    (define (get-sigs id)
      (define-values (imps exps) (unit-static-signatures id id))
      (list imps exps))
    
    ;; flatten one level of a list
    ;; listof[listof[a]] -> listof[a]
    (define (flatten l) (apply append l))
    
    ;; returns two lists of sig-elems
    (define (get-all-sigs ids)
      (define imps/exps (map get-sigs ids))
      (define-values (imps exps) (unzip2 imps/exps))
      (values (flatten imps) (flatten exps)))
    
    ;; construct the runtime code
    ;; takes 3 lists of identifiers and a syntax object for location info
    (define (mk imports exports units stx)
      (quasisyntax/loc stx
        (begin (define-compound-unit/infer new-unit@ 
                 (import #,@imports)
                 (export #,@exports)
                 (link #,@units))
               (define-values/invoke-unit/infer new-unit@))))
    
    ;; compares two signature datums for equality
    (define (sig=? sig1 sig2)
      (and (eq? (car sig1) (car sig2))
           (or (symbol? (car sig1)) (not (car sig1)))
           (bound-identifier=? (cdr sig1) (cdr sig2))))
    
    ;; is imp in the list of exports?
    (define (imp-in-exps? imp exps)
      (s:member imp exps sig=?))
    
    ;; produce the imports not satisfied by the exports, and all the exports
    ;; exports should not have duplicates
    (define (imps/exps-from-units units)
      (let-values ([(imps exps) (get-all-sigs units)])
         (let* ([exps* (map datum->sig-elem exps)]
                [imps* (map datum->sig-elem (filter (lambda (imp) (not (imp-in-exps? imp exps))) imps))])
           (values imps* exps*))))
    
    (syntax-case stx (import export)
      ;; here the exports are specified - they ought to be a subset of the allowable exports
      [(_ (export . sigs) . units)
       (let*-values ([(units) (syntax->list #'units)]
                     [(imps exps) (imps/exps-from-units units)])
         (mk imps (syntax->list #'sigs) units stx))]
      ;; here we just export everything that's available
      [(_ . units)
       (andmap identifier? (syntax->list #'units))
       (let*-values ([(units) (syntax->list #'units)]
                     [(imps exps) (imps/exps-from-units units)])
         (mk imps exps units stx))]))
  
    
  ;; Tests
  
  (define-signature x^ (x))
  (define-signature y^ (y))
  (define-signature z^ (z))
  
  (define-unit y@
    (import z^)
    (export y^)
    (define y (* 2 z)))
  
  (define-unit x@
    (import y^)
    (export x^)
    (define (x) (+ y 1)))
  
  (define z 45)
    
  (define-values/link-units/infer (export x^) x@ y@)
  
  )