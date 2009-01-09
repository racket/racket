#lang scheme/base

(require scheme/unit
         (for-syntax
          scheme/base
          (only-in srfi/1/list s:member delete-duplicates)
          scheme/unit-exptime
          scheme/match))

(provide define-values/link-units/infer cnt)

(define-signature-form (cnt stx)
  (syntax-case stx ()
    [(_ nm cnt)
     (list #'nm)
     #;(list #'[contracted (nm cnt)])]))

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
    (define-values (imps exps) (values (map car imps/exps) (map cadr imps/exps)))
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
  (define (sig-in-sigs? imp exps)
    (for/or ([e exps]) (sig=? imp e)))
  
  ;; produce the imports not satisfied by the exports, and all the exports
  ;; exports should not have duplicates
  (define (imps/exps-from-units units)
    (let-values ([(imps exps) (get-all-sigs units)])
      (let* ([exps* (map datum->sig-elem exps)]
             [imps* (map datum->sig-elem (filter (lambda (imp) (not (sig-in-sigs? imp exps))) imps))])
        (values imps* exps*))))

  (define (duplicates? sigs)
    (for/or ([s sigs]
             #:when
             (> 1 (length (for/list ([s* sigs] #:when (free-identifier=? s s*)) s*))))
            s))
  
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
       (cond [(duplicates? exps)
              =>
              (lambda (d)
                (raise-syntax-error #f (format "multiple units export the signature ~a" d) stx))]
             [else 
              (mk (delete-duplicates imps) exps units stx)]))]))


;; Tests
#|
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
(define-signature y^ (y))
 (define-unit x@ (import y^) (export))
 (define-unit z@ (import y^) (export))
 (define-values/link-units/infer x@ z@)

;(define-values/link-units/infer x@ y@)
|#

