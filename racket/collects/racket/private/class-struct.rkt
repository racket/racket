#lang racket/base
(provide (protect-out prop:object _object? object-ref find-method/who)
         obj-error as-write as-write-list as-value-list as-lines
         class-field-ref class-field-set!
         (struct-out class) (struct-out exn:fail:object)
         just-check-existence just-check-existence?
         (struct-out object/c-wrapper-info)
         (struct-out allowed-opaque))
(require "../contract/private/blame.rkt"
         "../contract/private/guts.rkt")

(define (do-custom-write v port mode)
  (cond
    [(custom-write? v)
     ((custom-write-accessor v) v port mode)]
    [(equal? mode #t)
     (write v port)]
    [(equal? mode #f)
     (display v port)]
    [else
     (print v port mode)]))

;; used by both contracts and classes:
;;--------------------------------------------------------------------
;;  class implementation
;;--------------------------------------------------------------------

(define insp (current-inspector)) ; for all opaque structures
(define-struct class (name
                      pos supers     ; pos is subclass depth, supers is vector
                      self-interface ; self interface
                      insp-mk        ; dummy struct maker to control inspection access
                      obj-inspector  ; the inspector used for instances of this class
                      
                      method-width   ; total number of methods
                      [method-ht     ; maps public names to vector positions
                       #:mutable]    ;  mutable to support impersonation by object/c
                      method-ids     ; reverse-ordered list of public method names
                      abstract-ids   ; list of abstract method names
                      method-ictcs   ; list of indices of methods to fix for interface ctcs

                      [ictc-classes  ; #f or weak hash of cached classes keyed by blame
                       #:mutable]

                      methods        ; vector of methods (for external dynamic dispatch)
                                     ; vector might also contain lists; see comment below from Stevie
                      super-methods  ; vector of methods (for subclass super calls)
                      int-methods    ; vector of vector of methods (for internal dynamic dispatch)
                      beta-methods   ; vector of vector of methods
                      meth-flags     ; vector: #f => primitive-implemented
                      ;         'final => final
                      ;         'augmentable => can augment
                      
                      inner-projs    ; vector of projections for the last inner slot
                      dynamic-idxs   ; vector of indexs for access into int-methods
                      dynamic-projs  ; vector of vector of projections for internal dynamic dispatch
                      
                      field-width    ; total number of fields
                      field-pub-width ; total number of public fields
                      field-ht       ; maps public field names to field-infos (see make-field-info above)
                      field-ids      ; list of public field names
                      all-field-ids  ; list of field names in reverse order, used for `undefined` error reporting
                      
                      [struct:object ; structure type for instances
                       #:mutable]
                      [object?       ; predicate
                       #:mutable]
                      [make-object   ; : (-> object), constructor that creates an uninitialized object
                          #:mutable]
                      [field-ref     ; accessor
                       #:mutable]
                      [field-set!    ; mutator
                       #:mutable]
                      
                      init-args      ; list of symbols in order; #f => only by position
                      init-mode      ; 'normal, 'stop (don't accept by-pos for super), or 'list
                      
                      [init          ; initializer
                       #:mutable]    ; :   object
                      ;     (object class (box boolean) leftover-args new-by-pos-args new-named-args 
                      ;      -> void) // always continue-make-super?
                      ;     class
                      ;     (box boolean)
                      ;     leftover-args
                      ;     named-args
                      ;  -> void
                      
                      [orig-cls      ; uncontracted version of this class (or same class)
                       #:mutable]
                      [serializer    ; proc => serializer, #f => not serializable
                       #:mutable]
                      [fixup         ; for deserialization
                       #:mutable]

                      check-undef?   ; objects need an unsafe-undefined guarding chaperone?
                      
                      no-super-init?); #t => no super-init needed
  #:inspector insp
  #:property prop:equal+hash
  (list (λ (cls-a cls-b recur) (eq? (class-orig-cls cls-a) (class-orig-cls cls-b)))
        (λ (cls recur) (eq-hash-code (class-orig-cls cls)))
        (λ (cls recur) (eq-hash-code (class-orig-cls cls)))))

#|

From Stevie, explaining the shape of the elements of the vector in the 'methods' field:

For each level of interface, we build up the following structure:

(list <contract> <name of interface that contains this contract> <pos blame or #f> <neg blame or #f>)

The second part of the list is used for certain types of failure reporting, I think, 
whereas the other parts are what we need to build the correct contract forms (once we
have the method implementation to contract).  In the interface contract info returned
from a list of contracts, the info for the leaves contains #f negative blame (which 
will be filled in with the class that implements the interface) and the info for the
"roots" (more on that later) contains #f positive blame (which is filled in with the 
info for the client of the class).

When we have a particular class, we can fill in the neg. blame for the leaves in the hierarchy, and
then we also apply as much of these structures have complete data to the method implementation
 (that is, non-#f pos and neg blames so we can appropriately construct the correct `contract' forms).

What's left is a list of non-complete data for the root(s) of the hierarchy (by roots, I mean
the first interfaces where this method is mentioned in the interface hierarchy).  We store that
list along with the method implementation, so that once we have the neg. blame (the blame region
that instantiates the class in question), we can complete this data and apply those 
last few projections.

|#

;;--------------------------------------------------------------------
;;  misc utils
;;--------------------------------------------------------------------

(define-struct (exn:fail:object exn:fail) () #:inspector insp)

(struct as-write (content))
(struct as-write-list (content))
(struct as-value-list (content))
(struct as-lines (content))

(define (obj-error where 
                   msg
                   #:class-name [class-name #f]
                   #:intf-name [intf-name #f]
                   #:which-class [which-class ""]
                   . fields)
  (define all-fields
    (append fields
            (if class-name
                (list (string-append which-class "class name")
                      (as-write class-name))
                null)
            (if intf-name
                (list "interface name"
                      (as-write intf-name))
                null)))
  (raise (make-exn:fail:object
          (format "~a: ~a~a" where msg
                  (apply
                   string-append
                   (let loop ([fields all-fields])
                     (cond
                      [(null? fields) null]
                      [else
                       (define field (car fields))
                       (define val (cadr fields))
                       (list*
                        "\n  "
                        field
                        (if (or (as-write-list? val)
                                (as-lines? val))
                            ":"
                            ": ")
                        (cond
                         [(or (as-write-list? val)
                              (as-value-list? val))
                          (apply string-append
                                 (for/list ([v (in-list (if (as-write-list? val)
                                                            (as-write-list-content val)
                                                            (as-value-list-content val)))])
                                   (format (if (as-write-list? val)
                                               "\n   ~s"
                                               "\n   ~e")
                                           v)))]
                         [(as-write? val)
                          (format "~s" (as-write-content val))]
                         [(as-lines? val)
                          (as-lines-content val)]
                         [else
                          (format "~e" val)])
                        (loop (cddr fields)))]))))
          (current-continuation-marks))))

(define (for-class name)
  (if name (format " for class: ~a" name) ""))
(define (for-class/which which name)
  (if name (format " for ~a class: ~a" which name) ""))
(define (for-intf name)
  (if name (format " for interface: ~a" name) ""))


(define-values (prop:object _object? object-ref) 
  (make-struct-type-property 'object 'can-impersonate))

;; find-method/who : symbol[top-level-form/proc-name]
;;                   any[object] 
;;                   symbol[method-name] 
;;               -> method-proc
;; returns the method's procedure

(define (find-method/who who in-object name)
  (cond
    [(object-ref in-object #f) ; non-#f result implies `_object?`
     => (lambda (cls)
          (cond
            [(class? cls)
             (define mth-idx (hash-ref (class-method-ht cls) name #f))
             (if mth-idx
                 (vector-ref (class-methods cls) mth-idx)
                 (no-such-method who name cls))]
            [else
             (define an-object/c-wrapper-info cls)
             (define methods-proc (object/c-wrapper-info-methods-proc an-object/c-wrapper-info))
             (define meth (methods-proc name))
             (cond
               [meth meth]
               [else
                (define unwrapped (object/c-wrapper-info-val an-object/c-wrapper-info))
                (define opaque (object/c-wrapper-info-opaque-methods an-object/c-wrapper-info))
                (define meth (find-method/who who unwrapped name))
                (define method-allowed?
                  (cond
                    [(procedure? opaque) (opaque meth)]
                    [(allowed-opaque? opaque)
                     (not ((allowed-opaque-pred opaque) meth))]
                    [else opaque]))
                (when method-allowed?
                  (define blame+neg-party (object/c-wrapper-info-blame+neg-party an-object/c-wrapper-info))
                  (define blame (car blame+neg-party))
                  (define np (cdr blame+neg-party))
                  (raise-blame-error (blame-swap blame)
                                     #:missing-party np
                                     in-object
                                     `(expected:
                                       ,(if (eq? opaque #t)
                                            "to invoke only methods listed in the contract"
                                            (format "to invoke only methods with the impersonator property ~s"
                                                    (if (allowed-opaque? opaque)
                                                        (allowed-opaque-pred opaque)
                                                        opaque)))
                                       given:
                                       "a method invocation of ~e")
                                     meth))
                (case (procedure-arity meth)
                  [(1) (λ (this) (meth unwrapped))]
                  [(2) (λ (this x) (meth unwrapped x))]
                  [(3) (λ (this x y) (meth unwrapped x y))]
                  [else
                   (make-keyword-procedure
                    (λ (kwds kwd-args this . args)
                      (keyword-apply meth kwds kwd-args unwrapped args))
                    (λ (this . args)
                      (apply meth unwrapped args)))])])]))]
    [else
     (obj-error who "target is not an object"
                "target" in-object 
                "method name" (as-write name))]))

(define (no-such-method who name cls)
  (obj-error who 
             "no such method"
             "method name" (as-write name)
             #:class-name (class-name cls)))

(define-values (just-check-existence just-check-existence?)
  (let ()
    (struct just-check-existence ())
    (values (just-check-existence)
            just-check-existence?)))

(struct object/c-wrapper-info (val methods-proc blame+neg-party pos-fields neg-fields opaque-methods opaque-fields ctc
                                   do-not-check-class-field-accessor-or-mutator-access?))


;; pred : impersonator-property-predicate-procedure?
;; this is used in the opaque methods object/c record
;; to distinguish between the cases when an impersonator
;; property allows access vs disallows access.
;; when the property disallows access, there is no
;; wrapper, but when it is allowed, it uses this struct
;; this struct is transparent so that `equal?` works
;; to implement contract-equivalent
(struct allowed-opaque (pred) #:transparent)
