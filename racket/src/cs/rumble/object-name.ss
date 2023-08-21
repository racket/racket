
(define-values (prop:object-name object-name? object-name-ref)
  (make-struct-type-property 'object-name
                             (lambda (v info)
                               (cond
                                [(exact-nonnegative-integer? v)
                                 (unless (< v (list-ref info 1))
                                   (raise-arguments-error 'guard-for-prop:object-name
                                                          "field index >= initialized-field count for structure type"
                                                          "field index" v
                                                          "initialized-field count" (list-ref info 1)))
                                 (unless (#%memv v (list-ref info 5))
                                   (raise-arguments-error 'guard-for-prop:object-name "field index not declared immutable"
                                                          "field index" v))
                                 (+ v (let ([p (list-ref info 6)])
                                        (if p
                                            (struct-type-total*-field-count p)
                                            0)))]
                                [(and (procedure? v)
                                      (procedure-arity-includes? v 1))
                                 v]
                                [else
                                 (raise-argument-error 'guard-for-prop:object-name
                                                       "(or/c exact-nonnegative-integer? (procedure-arity-includes/c 1))"
                                                       v)]))))

(define-thread-local procedure-names (make-weak-eq-hashtable))

(define (object-name v)
  (cond
   [(struct-type? v) ; needs to be before `object-name?` check
    (let ([v (strip-impersonator v)])
      (and (not (eq? (inspector-ref v) none))
           (record-type-name v)))]
   [(object-name? v)
    (let ([n (object-name-ref v)])
      (cond
       [(exact-integer? n)
        (unsafe-struct-ref v n)]
       [else
        (|#%app| n v)]))]
   [(#%procedure? v)
    (cond
     [(wrapper-procedure? v)
      (extract-wrapper-procedure-name v)]
     [else
      (let ([names procedure-names]
            [code (#%$closure-code v)])
        (or (eq-hashtable-ref names code #f)
            (let ([name (#%$code-name code)])
              (and name
                   (let ([n (procedure-name-string->visible-name-string name)])
                     (and n
                          (let ([sym (string->symbol n)])
                            (eq-hashtable-set! names code sym)
                            sym)))))))])]
   [(impersonator? v)
    (object-name (impersonator-val v))]
   [(procedure? v)
    (extract-procedure-name v)]
   [(struct-type-property? v)
    (struct-type-prop-name v)]
   [(record? v)
    (struct-object-name v)]
   [else #f]))

(define (struct-object-name v)
  (let ([rtd (record-rtd v)])
    (and
     ;; Having an entry in `rtd-props` is a sign that this structure
     ;; type was created with `make-struct-type`, or it could be a
     ;; prefab structure type
     (with-global-lock*
      (or (hashtable-contains? rtd-props rtd)
          (getprop (record-type-uid rtd) 'prefab-key+count #f)))
     (object-name (record-rtd v)))))

;; Since a procedure name is the one way we have to attach static
;; information to `lambda` forms, it can encode more than just a name:
;;   * Starting with "[" means a path-derived name, where that
;;     distinction is used instack trace.
;;   * Starting with "]" means that some other starting character
;;     ws escaped.
;;  * After "[" or "]", "!" means a method, and "^" means not-a-method.
(define (procedure-name-string->visible-name-string n)
  (cond
   [(not (string? n)) n]
   [else
    (let ([len (string-length n)])
      (cond
       [(fx= 0 len) n]
       [else
        (let ([strip-prefix
               (lambda ()
                 (cond
                  [(fx= 1 len) ""]
                  [(char=? #\! (string-ref n 1)) ; => method
                   (substring n 2 len)]
                  [(char=? #\^ (string-ref n 1)) ; => not a method
                   (substring n 2 len)]
                  [else
                   (substring n 1 len)]))])
          (cond
           [(char=? #\[ (string-ref n 0)) ; => name is derived from a path
            (let ([n (strip-prefix)])
              ;; Empty means no name
              (if (eqv? "" n)
                  #f
                  n))]
           [(char=? #\] (string-ref n 0))
            (strip-prefix)]
           [else n]))]))]))

;; name starting with a square bracket is meant to
;; encode a path or "no name"
(define (path-or-empty-procedure-name-string? n)
  (and (string? n)
       (fx> (string-length n) 0)
       (char=? #\[ (string-ref n 0))))
