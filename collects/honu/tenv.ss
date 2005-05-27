(module tenv mzscheme
  
  (require (lib "boundmap.ss" "syntax")
           (lib "contract.ss"))
  
  (require "ast.ss")
  (require "read-error-with-stx.ss")
  
  (provide (struct tenv-entry (src-stx))
           (struct tenv-init  (name type optional?))
           (struct tenv-type  (supers members))
           (struct tenv-class (sub-type impls inits final? super))
           (struct tenv-mixin (arg-type sub-type impls inits
                                      used-names used-types final?))
           (struct tenv-func  (arg-types return-type)))
  
  (define-struct tenv-entry (src-stx))

  (define-struct tenv-init (name type optional?))
 
  ;; members will be a hashtable from member names to types
  (define-struct (tenv-type  tenv-entry) (supers members))
  (define-struct (tenv-class tenv-entry) (sub-type impls inits final? super))
  (define-struct (tenv-mixin tenv-entry) (arg-type sub-type impls inits
                                           used-names used-types final?))
  (define-struct (tenv-func  tenv-entry) (arg-types return-type))
   
  (provide tenv?)
  (define tenv? bound-identifier-mapping?)

  (provide/contract [printable-key (identifier? . -> . symbol?)]
                    [tenv-key=?    (identifier? identifier? . -> . any)]
                    [tenv-map      (tenv?
                                    (identifier? tenv-entry? . -> . any)
                                    . -> .
                                    list?)])
  (define printable-key syntax-e)
  (define tenv-key=? bound-identifier=?)
  (define tenv-map bound-identifier-mapping-map)

  (provide/contract [empty-tenv (-> tenv?)]
                    [extend-tenv (identifier? tenv-entry? tenv? . -> . void?)]
                    [create-tenv ((listof identifier?)
                                  (listof tenv-entry?)
                                  . -> .
                                  tenv?)])
  (define (empty-tenv) (make-bound-identifier-mapping))
  (define (extend-tenv key val tenv)
    (if (get-tenv-entry key tenv)
        (raise-read-error-with-stx
         (format "~a already bound by top-level definition" (printable-key key))
         key)
        (bound-identifier-mapping-put! tenv key val)))
  (define (create-tenv keys vals)
    (let ((table (empty-tenv)))
      (begin (for-each extend-tenv table keys vals)
             table)))

  ;(provide get-tenv-entry get-tenv-member-entry)
  (define (get-tenv-entry key tenv)
    (bound-identifier-mapping-get tenv key (lambda () #f)))
  (define (get-tenv-member-entry type member tenv)
    (bound-identifier-mapping-get (tenv-type-members (get-tenv-entry type tenv))
                                  member
                                  (lambda () #f)))
  
  (provide/contract [get-type-entry  (identifier? tenv? . -> . tenv-type?)]
                    [get-class-entry (identifier? tenv? . -> . tenv-class?)]
                    [get-mixin-entry (identifier? tenv? . -> . tenv-mixin?)]
                    [get-func-entry  (identifier? tenv? . -> . tenv-func?)])
  (define (get-type-entry name tenv)
    (let ([entry (get-tenv-entry name tenv)])
      (cond
        [(not entry)
         (raise-read-error-with-stx
          (format "No type defined with name ~a" (printable-key name))
          name)]
        [(not (tenv-type? entry))
         (raise-read-error-with-stx
          (format "Definition of ~a is not a type" (printable-key name))
          name)]
        [else entry])))
  (define (get-class-entry name tenv)
    (let ([entry (get-tenv-entry name tenv)])
      (cond
        [(not entry)
         (raise-read-error-with-stx
          (format "No class defined with name ~a" (printable-key name))
          name)]
        [(not (tenv-class? entry))
         (raise-read-error-with-stx
          (format "Definition of ~a is not a class" (printable-key name))
          name)]
        [else entry])))
  (define (get-mixin-entry name tenv)
    (let ([entry (get-tenv-entry name tenv)])
      (cond
        [(not entry)
         (raise-read-error-with-stx
          (format "No mixin defined with name ~a" (printable-key name))
          name)]
        [(not (tenv-mixin? entry))
         (raise-read-error-with-stx
          (format "Definition of ~a is not a mixin" (printable-key name))
          name)]
        [else entry])))
  (define (get-func-entry name tenv)
    (let ([entry (get-tenv-entry name tenv)])
      (cond
        [(not entry)
         (raise-read-error-with-stx
          (format "No function defined with name ~a" (printable-key name))
          name)]
        [(not (tenv-func? entry))
         (raise-read-error-with-stx
          (format "Definition of ~a is not a function" (printable-key name))
          name)]
        [else entry])))
  )
