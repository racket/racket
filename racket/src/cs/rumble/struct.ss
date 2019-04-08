;; Naming conventions:
;;  - `rtd*` means an rtd that is not impersonators
;;  - `init-count` means the number of fields supplied to the constructor,
;;     not counting inherited fields
;;  - `init*-count` means `init-count` plus inherited constructed fields
;;  - `auto-count` means the number of fields automatically added,
;;     not counting inherited fields
;;  - `auto*-count` means `auto-count` plus inherited auto fields
;;  - `total-count` means `init-count` plus `auto-count`
;;  - `total*-count` means `init*-count` plus `auto*-count`
;;  - `prefab-key+count` has a `total*-count`

(define-record struct-type-prop (name guard supers))

;; Record the properties that are implemented by each rtd:
(define rtd-props (make-ephemeron-eq-hashtable))

;; Maps a property-accessor function to `(cons predicate-proc can-impersonate)`:
(define property-accessors (make-ephemeron-eq-hashtable))

(define (struct-type-property? v)
  (struct-type-prop? v))

(define/who make-struct-type-property
  (case-lambda
    [(name) (make-struct-type-property name #f '() #f)]
    [(name guard) (make-struct-type-property name guard '() #f)]
    [(name guard supers) (make-struct-type-property name guard supers #f)]
    [(name guard supers can-impersonate?)
     (check who symbol? name)
     (unless (or (not guard)
                 (eq? guard 'can-impersonate)
                 (and (#%procedure? guard) ; avoid `procedure?` until it's defined
                      (bitwise-bit-set? (#%procedure-arity-mask guard) 2))
                 (and (procedure? guard)
                      (procedure-arity-includes? guard 2)))
       (raise-argument-error who "(or/c (procedure-arity-includes/c 2) #f 'can-impersonate)" guard))
     (unless (and (or (null? supers) ; avoid `list?` until it's defined
                      (list? supers))
                  (andmap (lambda (p)
                            (and (pair? p)
                                 (struct-type-property? (car p))
                                 (procedure? (cdr p))
                                 (procedure-arity-includes? (cdr p) 1)))
                          supers))
       (raise-argument-error who "(listof (cons/c struct-type-property? (procedure-arity-includes/c 1)))" supers))
     (let* ([can-impersonate? (and (or can-impersonate? (eq? guard 'can-impersonate)) #t)]
            [st (make-struct-type-prop name (and (not (eq? guard 'can-impersonate)) guard) supers)]
            [pred (escapes-ok
                    (lambda (v)
                      (let* ([v (strip-impersonator v)]
                             [rtd (if (record-type-descriptor? v)
                                      v
                                      (and (record? v)
                                           (record-rtd v)))])
                        (and rtd
                             (not (eq? none (struct-property-ref st rtd none)))))))]
            [accessor-name (string->symbol (string-append
                                            (symbol->string name)
                                            "-ref"))]
            [predicate-name (string->symbol
                             (string-append
                              (symbol->string name)
                              "?"))]
            [default-fail
              (escapes-ok
                (lambda (v)
                  (raise-argument-error accessor-name
                                        (symbol->string predicate-name)
                                        v)))]
            [do-fail (lambda (fail v)
                       (cond
                        [(eq? fail default-fail) (default-fail v)]
                        [(procedure? fail) (|#%app| fail)]
                        [else fail]))])
       (letrec ([acc
                 (case-lambda
                  [(v fail)
                   (cond
                    [(and (impersonator? v)
                          (pred v))
                     (impersonate-struct-or-property-ref acc #f acc v)]
                    [else
                     (let* ([rtd (if (record-type-descriptor? v)
                                     v
                                     (and (record? v)
                                          (record-rtd v)))])
                       (if rtd
                           (let ([pv (struct-property-ref st rtd none)])
                             (if (eq? pv none)
                                 (do-fail fail v)
                                 pv))
                           (do-fail fail v)))])]
                  [(v) (acc v default-fail)])])
         (with-global-lock*
          (hashtable-set! property-accessors
                          acc
                          (cons pred can-impersonate?)))
         (values st
                 pred
                 acc)))]))

(define (struct-type-property-accessor-procedure? v)
  (and (procedure? v)
       (let ([v (strip-impersonator v)])
         (with-global-lock* (hashtable-ref property-accessors v #f)))
       #t))

(define (struct-type-property-accessor-procedure-pred v)
  (car (with-global-lock (hashtable-ref property-accessors v #f))))

(define (struct-type-property-accessor-procedure-can-impersonate? v)
  (cdr (with-global-lock* (hashtable-ref property-accessors v #f))))

(define (struct-property-ref prop rtd default)
  (getprop (record-type-uid rtd) prop default))

(define (struct-property-set! prop rtd val)
  (putprop (record-type-uid rtd) prop val))

;; ----------------------------------------

(define-record-type (inspector new-inspector inspector?) (fields parent))

(define root-inspector (new-inspector #f))

(define/who make-inspector
  (case-lambda
    [() (new-inspector (|#%app| current-inspector))]
    [(i)
     (check who inspector? i)
     (new-inspector i)]))

(define/who make-sibling-inspector
  (case-lambda
   [() (make-sibling-inspector (current-inspector))]
   [(i)
    (check who inspector? i)
    (make-inspector (inspector-parent i))]))

(define/who (inspector-superior? sup-insp sub-insp)
  (check who inspector? sup-insp)
  (check who inspector? sub-insp)
  (if (eq? sub-insp root-inspector)
      #f
      (let ([parent (inspector-parent sub-insp)])
        (or (eq? parent sup-insp)
            (inspector-superior? sup-insp parent)))))

(define (inspector-ref rtd)
  (getprop (record-type-uid rtd) 'inspector none))

(define (inspector-set! rtd insp)
  (putprop (record-type-uid rtd) 'inspector insp))

;; ----------------------------------------

(define (check-make-struct-type-arguments who name parent-rtd init-count auto-count
                                          props insp proc-spec immutables guard constructor-name)
  (check who symbol? name)
  (check who :or-false struct-type? parent-rtd)
  (check who exact-nonnegative-integer? init-count)
  (check who exact-nonnegative-integer? auto-count)
  (check who
         :test (or (not proc-spec)
                   (procedure? proc-spec)
                   (exact-nonnegative-integer? proc-spec))
         :contract "(or/c procedure? exact-nonnegative-integer? #f)"
         proc-spec)
  (check who
         :test (and (list props)
                    (andmap (lambda (i) (and (pair? i) (struct-type-property? (car i))))
                            props))
         :contract "(listof (cons/c struct-type-property? any/c))"
         props)
  (check who
         :test (or (not insp)
                   (inspector? insp)
                   (eq? insp 'prefab))
         :contract "(or/c inspector? #f 'prefab)"
         insp)
  (check who
         :test (and (list? immutables)
                    (andmap exact-nonnegative-integer? immutables))
         :contract "(listof exact-nonnegative-integer?)"
         immutables)
  (check who :or-false procedure? guard)
  (check who :or-false symbol? constructor-name)

  ;; The rest has to be delayed until we have an rtd:
  (lambda (rtd parent-rtd* all-immutables)
    (let ([props-ht
           ;; Check for duplicates and record property values
           (let ([get-struct-info
                  (escapes-ok
                    (lambda ()
                      (let ([parent-total*-count (if parent-rtd*
                                                     (struct-type-total*-field-count parent-rtd*)
                                                     0)])
                        (list name
                              init-count
                              auto-count
                              (make-position-based-accessor rtd parent-total*-count (+ init-count auto-count))
                              (make-position-based-mutator rtd parent-total*-count (+ init-count auto-count))
                              all-immutables
                              parent-rtd
                              #f))))])
             (let loop ([props props] [ht empty-hasheq])
               (cond
                [(null? props)
                 (if proc-spec
                     (let-values ([(ht props) (check-and-add-property who prop:procedure proc-spec rtd ht '()
                                                                      get-struct-info)])
                       ht)
                     ht)]
                [else
                 (let-values ([(ht props) (check-and-add-property who (caar props) (cdar props) rtd ht (cdr props)
                                                                  get-struct-info)])
                   (loop props ht))])))])

      (when (eq? insp 'prefab)
        (let ([bad
               (or (and (impersonator? parent-rtd)
                        "chaperoned supertype disallowed for non-generative structure type")
                   (and parent-rtd
                        (not (eq? (inspector-ref parent-rtd) 'prefab))
                        "generative supertype disallowed for non-generative structure type")
                   (and (pair? props)
                        "properties disallowed for non-generative structure type")
                   (and proc-spec
                        "procedure specification disallowed for non-generative structure type")
                   (and guard
                        "guard disallowed for non-generative structure type"))])
          (when bad
            (raise-arguments-error who bad
                                   "structure type name" name))))

      (let loop ([ht empty-hasheqv] [imms immutables])
        (cond
         [(null? imms) (void)]
         [else
          (let ([i (car imms)])
            (when (hash-ref ht i #f)
              (raise-arguments-error who
                                     "redundant immutable field index"
                                     "index" i
                                     "in list" immutables))
            (unless (< i init-count)
              (raise-arguments-error who
                                     "index for immutable field >= initialized-field count"
                                     "index" i
                                     "initialized-field count" init-count
                                     "in list" immutables))
            (loop (hash-set ht i #t) (cdr imms)))]))

      (let ([v (hash-ref props-ht prop:procedure #f)])
        (when v
          (cond
           [(exact-nonnegative-integer? v)
            (unless (< v init-count)
              (raise-arguments-error who
                                     "index for procedure >= initialized-field count"
                                     "index" v
                                     "field count" init-count))
            (unless (or (eq? v proc-spec) (chez:memv v immutables))
              (raise-arguments-error who
                                     "field is not specified as immutable for a prop:procedure index"
                                     "index" v))]
           [(procedure? v)
            (void)]
           [else
            (raise-arguments-error who
                                   "given value did not satisfy the contract for prop:procedure"
                                   "expected" "(or/c procedure? exact-nonnegative-integer?)"
                                   "given" v)])))

      (let ([parent-rtd* (strip-impersonator parent-rtd)])
        (when parent-rtd*
          (let ([authentic? (not (eq? (hash-ref props-ht prop:authentic none) none))]
                [authentic-parent? (struct-property-ref prop:authentic parent-rtd* #f)])
            (when (not (eq? authentic? authentic-parent?))
              (if authentic?
                  (raise-arguments-error who
                                         "cannot make an authentic subtype of a non-authentic type"
                                         "type name" name
                                         "non-authentic type" parent-rtd)
                  (raise-arguments-error who
                                         "cannot make a non-authentic subtype of an authentic type"
                                         "type name" name
                                         "authentic type" parent-rtd)))))

        (when guard
          (let ([expected-count (+ 1
                                   init-count
                                   (if parent-rtd*
                                       (get-field-info-init*-count (struct-type-field-info parent-rtd*))
                                       0))])
            (unless (procedure-arity-includes? guard expected-count)
              (raise-arguments-error who
                                     (string-append
                                      "guard procedure does not accept correct number of arguments;\n"
                                      " should accept one more than the number of constructor arguments")
                                     "guard procedure" guard
                                     "expected arity" expected-count))))))))

(define (check-and-add-property who prop val rtd ht props get-struct-info)
  (let* ([guarded-val
          (let ([guard (struct-type-prop-guard prop)])
            (if guard
                (|#%app| guard val (get-struct-info))
                val))]
         [check-val (cond
                     [(eq? prop prop:procedure)
                      ;; Save and check the original value, since the true
                      ;; guard is in `check-make-struct-type-arguments`
                      ;; (for historical reasons)
                      val]
                     [else guarded-val])]
         [old-v (hash-ref ht prop none)])
    (unless (or (eq? old-v none)
                (eq? old-v check-val))
      (raise-arguments-error who
                             "duplicate property binding"
                             "property" prop))
    (when (eq? prop prop:equal+hash)
      (record-type-equal-procedure rtd (let ([p (cadr guarded-val)])
                                         (if (#%procedure? p)
                                             p
                                             (lambda (v1 v2 e?) (|#%app| p v1 v2 e?)))))
      (record-type-hash-procedure rtd (let ([p (caddr guarded-val)])
                                        (if (#%procedure? p)
                                            p
                                            (lambda (v h) (|#%app| p v h))))))
    (struct-property-set! prop rtd guarded-val)
    (values (hash-set ht prop check-val)
            (append
             (if (eq? old-v none)
                 (map (lambda (super)
                        (cons (car super)
                              (|#%app| (cdr super) guarded-val)))
                      (struct-type-prop-supers prop))
                 ;; skip supers, because property is already added
                 null)
             props))))

;; ----------------------------------------

;; Records which fields of an rtd are mutable, where an rtd that is
;; not in the table has no mutable fields, and the field list can be
;; empty if a parent type is mutable:
(define rtd-mutables (make-ephemeron-eq-hashtable))

;; Accessors and mutators that need a position are wrapped in these records:
(define-record position-based-accessor (rtd offset field-count))
(define-record position-based-mutator (rtd offset field-count))

;; Register other procedures in hash tables; avoid wrapping to
;; avoid making the procedures slower
(define struct-constructors (make-ephemeron-eq-hashtable))
(define struct-predicates (make-ephemeron-eq-hashtable))
(define struct-field-accessors (make-ephemeron-eq-hashtable))
(define struct-field-mutators (make-ephemeron-eq-hashtable))

(define (register-struct-constructor! p)
  (add-to-table! struct-constructors p #t))

(define (register-struct-predicate! p)
  (add-to-table! struct-predicates p #t))

(define (register-struct-field-accessor! p rtd pos)
  (add-to-table! struct-field-accessors p (cons rtd pos)))

(define (register-struct-field-mutator! p rtd pos)
  (add-to-table! struct-field-mutators p (cons rtd pos)))

(define (struct-constructor-procedure? v)
  (and (procedure? v)
       (let ([v (strip-impersonator v)])
         (with-global-lock* (hashtable-ref struct-constructors v #f)))))

(define (struct-predicate-procedure? v)
  (and (procedure? v)
       (let ([v (strip-impersonator v)])
         (with-global-lock* (hashtable-ref struct-predicates v #f)))))

(define (struct-accessor-procedure? v)
  (and (procedure? v)
       (let ([v (strip-impersonator v)])
         (or (position-based-accessor? v)
             (with-global-lock* (hashtable-ref struct-field-accessors v #f))))
       #t))

(define (struct-mutator-procedure? v)
  (and (procedure? v)
       (let ([v (strip-impersonator v)])
         (or (position-based-mutator? v)
             (with-global-lock* (hashtable-ref struct-field-mutators v #f))))
       #t))

(define (struct-accessor-procedure-rtd+pos v)
  (with-global-lock* (hashtable-ref struct-field-accessors v #f)))

(define (struct-mutator-procedure-rtd+pos v)
  (with-global-lock* (hashtable-ref struct-field-mutators v #f)))

;; This indirection prevents the whole-program optimizer from inlining
;; the `with-glocal-lock*` expansion --- which, at the time of
;; writing, inflates the resulting code by 30%!
(define add-to-table! #f)
(define add-to-table!/done
  (set! add-to-table!
        (lambda (table key val)
          (with-global-lock*
           (hashtable-set! table key val)))))

;; ----------------------------------------

;; General structure-type creation, but not called when a `schemify`
;; transformation keeps the record type exposed to the compiler
(define make-struct-type
  (case-lambda 
    [(name parent-rtd init-count auto-count)
     (make-struct-type name parent-rtd init-count auto-count #f '() (|#%app| current-inspector) #f '() #f name)]
    [(name parent-rtd init-count auto-count auto-val)
     (make-struct-type name parent-rtd init-count auto-count auto-val '() (|#%app| current-inspector) #f '() #f name)]
    [(name parent-rtd init-count auto-count auto-val props)
     (make-struct-type name parent-rtd init-count auto-count auto-val props (|#%app| current-inspector) #f '() #f name)]
    [(name parent-rtd init-count auto-count auto-val props insp)
     (make-struct-type name parent-rtd init-count auto-count auto-val props insp #f '() #f name)]
    [(name parent-rtd init-count auto-count auto-val props insp proc-spec)
     (make-struct-type name parent-rtd init-count auto-count auto-val props insp proc-spec '() #f name)]
    [(name parent-rtd init-count auto-count auto-val props insp proc-spec immutables)
     (make-struct-type name parent-rtd init-count auto-count auto-val props insp proc-spec immutables #f name)]
    [(name parent-rtd init-count auto-count auto-val props insp proc-spec immutables guard)
     (make-struct-type name parent-rtd init-count auto-count auto-val props insp proc-spec immutables guard name)]
    [(name parent-rtd init-count auto-count auto-val props insp proc-spec immutables guard constructor-name)
     (let* ([install-props!
             (check-make-struct-type-arguments 'make-struct-type name parent-rtd init-count auto-count
                                               props insp proc-spec immutables guard constructor-name)]
            [prefab-uid (and (eq? insp 'prefab)
                             (structure-type-lookup-prefab-uid name parent-rtd init-count auto-count auto-val immutables))]
            [parent-rtd* (strip-impersonator parent-rtd)]
            [parent-fi (if parent-rtd*
                           (struct-type-field-info parent-rtd*)
                           empty-field-info)]
            [rtd (make-record-type-descriptor name
                                              parent-rtd*
                                              prefab-uid #f #f
                                              (make-fields (+ init-count auto-count)))]
            [parent-auto*-count (get-field-info-auto*-count parent-fi)]
            [parent-init*-count (get-field-info-init*-count parent-fi)]
            [parent-total*-count (get-field-info-total*-count parent-fi)]
            [init*-count (+ init-count parent-init*-count)]
            [auto*-count (+ auto-count parent-auto*-count)]
            [auto-field-adder (and (positive? auto*-count)
                                   (let ([pfa (get-field-info-auto-adder parent-fi)])
                                     (lambda (args)
                                       (args-insert args init-count auto-count auto-val pfa))))])
       (when (or parent-rtd* auto-field-adder)
         (let ([field-info (make-field-info init*-count auto*-count auto-field-adder)])
           (putprop (record-type-uid rtd) 'field-info field-info)))
       (struct-type-install-properties! rtd name init-count auto-count parent-rtd
                                        props insp proc-spec immutables guard constructor-name
                                        install-props!)
       (let ([ctr (struct-type-constructor-add-guards
                   (let ([c (record-constructor rtd)])
                     (procedure-rename
                      (if (zero? auto*-count)
                          c
                          (procedure-reduce-arity
                           (lambda args
                             (apply c (reverse (auto-field-adder (reverse args)))))
                           init*-count))
                      (or constructor-name name)))
                   rtd
                   (or constructor-name name))]
             [pred (procedure-rename
                    (lambda (v)
                      (or (record? v rtd)
                          (and (impersonator? v)
                               (record? (impersonator-val v) rtd))))
                    (string->symbol (string-append (symbol->string name) "?")))])
         (register-struct-constructor! ctr)
         (register-struct-constructor! pred)
         (values rtd
                 ctr
                 pred
                 (make-position-based-accessor rtd parent-total*-count (+ init-count auto-count))
                 (make-position-based-mutator rtd parent-total*-count (+ init-count auto-count)))))]))

;; Called both by `make-struct-type` and by a `schemify` transformation:
(define struct-type-install-properties!
  (case-lambda
   [(rtd name init-count auto-count parent-rtd)
    (struct-type-install-properties! rtd name init-count auto-count parent-rtd '() (|#%app| current-inspector) #f '() #f name #f)]
   [(rtd name init-count auto-count parent-rtd props)
    (struct-type-install-properties! rtd name init-count auto-count parent-rtd props (|#%app| current-inspector) #f '() #f name #f)]
   [(rtd name init-count auto-count parent-rtd props insp)
    (struct-type-install-properties! rtd name init-count auto-count parent-rtd props insp #f '() #f name #f)]
   [(rtd name init-count auto-count parent-rtd props insp proc-spec)
    (struct-type-install-properties! rtd name init-count auto-count parent-rtd props insp proc-spec '() #f name #f)]
   [(rtd name init-count auto-count parent-rtd props insp proc-spec immutables)
    (struct-type-install-properties! rtd name init-count auto-count parent-rtd props insp proc-spec immutables #f name #f)]
   [(rtd name init-count auto-count parent-rtd props insp proc-spec immutables guard)
    (struct-type-install-properties! rtd name init-count auto-count parent-rtd props insp proc-spec immutables guard name #f)]
   [(rtd name init-count auto-count parent-rtd props insp proc-spec immutables guard constructor-name)
    (struct-type-install-properties! rtd name init-count auto-count parent-rtd props insp proc-spec immutables guard constructor-name #f)]
   [(rtd name init-count auto-count parent-rtd props insp proc-spec immutables guard constructor-name install-props!)
    (let ([install-props!
           (or install-props!
               (check-make-struct-type-arguments 'make-struct-type name parent-rtd init-count auto-count
                                                 props insp proc-spec immutables guard constructor-name))])
      (unless (eq? insp 'prefab) ; everything for prefab must be covered in `prefab-key+count->rtd`
        (let* ([parent-rtd* (strip-impersonator parent-rtd)]
               [parent-props
                (if parent-rtd*
                    (with-global-lock* (hashtable-ref rtd-props parent-rtd* '()))
                    '())]
               [all-immutables (if (integer? proc-spec)
                                   (cons proc-spec immutables)
                                   immutables)]
               [mutables (immutables->mutables all-immutables init-count auto-count)])
          (when (not parent-rtd*)
            (record-type-equal-procedure rtd default-struct-equal?)
            (record-type-hash-procedure rtd default-struct-hash))
          ;; Record properties implemented by this type:
          (let ([props (let ([props (append (map car props) parent-props)])
                         (if proc-spec
                             (cons prop:procedure props)
                             props))])
            (with-global-lock* (hashtable-set! rtd-props rtd props)))
          (with-global-lock*
           (register-mutables! mutables rtd parent-rtd*))
          ;; Copy parent properties for this type:
          (for-each (lambda (prop)
                      (let loop ([prop prop])
                        (struct-property-set! prop rtd (struct-property-ref prop parent-rtd* #f))
                        (for-each (lambda (super)
                                    (loop (car super)))
                                  (struct-type-prop-supers prop))))
                    parent-props)
          ;; Finish checking and install new property values:
          (install-props! rtd parent-rtd* all-immutables)
          ;; Record inspector
          (inspector-set! rtd insp)
          ;; Register guard
          (register-guards! rtd parent-rtd guard 'at-start))))]))

;; Used by a `schemify` transformation:
(define (structure-type-lookup-prefab-uid name parent-rtd* init-count auto-count auto-val immutables)
  ;; Return a UID for a prefab structure type. We can assume that
  ;; `immutables` is well-formed, and checking an error reporting will
  ;; happen latter if necessary.
  (let ([prefab-key (derive-prefab-key name
                                       (and parent-rtd*
                                            (getprop (record-type-uid parent-rtd*) 'prefab-key+count))
                                       init-count
                                       immutables auto-count auto-val)]
        [total*-count (+ (if parent-rtd*
                             (struct-type-total*-field-count parent-rtd*)
                             0)
                         init-count
                         auto-count)])
    (record-type-uid
     (prefab-key+count->rtd (cons prefab-key total*-count)))))

;; A weak, `equal?`-based hash table that maps (cons prefab-key
;; total-field-count) to rtd. We'll create a table without a lock, and
;; we'll use it for all places, which means that we need to use a
;; global lock to access the table. Compute a hash code outside the
;; lock, though, just in case computing the code needs the lock.
(define prefabs #f)

;; Call with lock:
(define (prefab-ref prefab-key+count code)
  (and prefabs
       (weak-hash-ref prefabs prefab-key+count #f code equal?)))

(define (prefab-key+count->rtd prefab-key+count)
  (let ([code (equal-hash-code prefab-key+count)])
    (cond
     [(with-global-lock (prefab-ref prefab-key+count code))
      => (lambda (rtd) rtd)]
     [else
      (let* ([prefab-key (car prefab-key+count)]
             [name (if (symbol? prefab-key)
                       prefab-key
                       (car prefab-key))]
             [parent-prefab-key+count
              (prefab-key->parent-prefab-key+count (car prefab-key+count))]
             [parent-rtd (and parent-prefab-key+count
                              (prefab-key+count->rtd parent-prefab-key+count))]
             [total-count (- (cdr prefab-key+count)
                             (if parent-prefab-key+count
                                 (cdr parent-prefab-key+count)
                                 0))]
             [uid (encode-prefab-key+count-as-symbol prefab-key+count)]
             [rtd (make-record-type-descriptor name
                                               parent-rtd
                                               uid #f #f
                                               (make-fields total-count))]
             [mutables (prefab-key-mutables prefab-key)])
        (with-global-lock
         (cond
          [(prefab-ref prefab-key+count code)
           ;; rtd was created concurrently
           => (lambda (rtd) rtd)]
          [else
           (putprop uid 'prefab-key+count prefab-key+count)
           (unless prefabs (set! prefabs (make-weak-hash-with-lock #f)))
           (weak-hash-set! prefabs prefab-key+count rtd code equal?)
           (unless parent-rtd
             (record-type-equal-procedure rtd default-struct-equal?)
             (record-type-hash-procedure rtd default-struct-hash))
           (register-mutables! mutables rtd parent-rtd)
           (inspector-set! rtd 'prefab)
           rtd])))])))

;; call with lock held
(define (register-mutables! mutables rtd parent-rtd)
  (unless (and (equal? '#() mutables)
               (or (not parent-rtd)
                   (not (hashtable-contains? rtd-mutables parent-rtd))))
    (hashtable-set! rtd-mutables rtd mutables)))

(define (check-accessor-or-mutator-index who rtd pos)
  (let* ([total-count (#%vector-length (record-type-field-names rtd))])
    (unless (< pos total-count)
      (if (zero? total-count)
          (raise-arguments-error who
                                 "index too large; no fields accessible"
                                 "index" pos
                                 "structure type" rtd)
          (raise-arguments-error who
                                 "index too large"
                                 "index" pos
                                 "maximum allowed index" (sub1 total-count)
                                 "structure type" rtd)))))

(define/who make-struct-field-accessor
  (case-lambda
   [(pba pos name)
    (check who position-based-accessor?
           :contract "(and/c struct-accessor-procedure? (procedure-arity-includes/c 2))"
           pba)
    (check who exact-nonnegative-integer? pos)
    (check who symbol? :or-false name)
    (let ([rtd (position-based-accessor-rtd pba)])
      (check-accessor-or-mutator-index who rtd pos)
      (let* ([p (record-field-accessor rtd
                                       (+ pos (position-based-accessor-offset pba)))]
             [wrap-p
              (procedure-rename
                (lambda (v)
                  ($value
                   (if (impersonator? v)
                       (impersonate-ref p rtd pos v)
                       (p v))))
                (string->symbol (string-append (symbol->string (record-type-name rtd))
                                               "-"
                                               (if name
                                                   (symbol->string name)
                                                   (string-append "field" (number->string pos))))))])
        (register-struct-field-accessor! wrap-p rtd pos)
        wrap-p))]
   [(pba pos)
    (make-struct-field-accessor pba pos #f)]))

(define/who make-struct-field-mutator
  (case-lambda
   [(pbm pos name)
    (check who position-based-mutator?
           :contract "(and/c struct-mutator-procedure? (procedure-arity-includes/c 3))"
           pbm)
    (check who exact-nonnegative-integer? pos)
    (check who symbol? :or-false name)
    (let ([rtd (position-based-mutator-rtd pbm)])
      (check-accessor-or-mutator-index who rtd pos)
      (let* ([abs-pos (+ pos (position-based-mutator-offset pbm))]
             [p (record-field-mutator rtd abs-pos)]
             [name (string->symbol
                    (string-append "set-"
                                   (symbol->string (record-type-name rtd))
                                   "-"
                                   (if name
                                       (symbol->string name)
                                       (string-append "field" (number->string pos)))
                                   "!"))]
             [wrap-p
              (procedure-rename
               (if (struct-type-field-mutable? rtd pos)
                   (lambda (v a)
                     (if (impersonator? v)
                         (impersonate-set! p rtd pos abs-pos v a)
                         (p v a)))
                   (lambda (v a)
                     (raise-arguments-error name
                                            "cannot modify value of immutable field in structure"
                                            "structure" v
                                            "field index" pos)))
               name)])
        (register-struct-field-mutator! wrap-p rtd pos)
        wrap-p))]
   [(pbm pos)
    (make-struct-field-mutator pbm pos #f)]))

;; Takes constructor arguments and adds auto-argument values.
;; Receives and returns `args` is in reverse order.
(define (args-insert args fields-count auto-count auto-val pfa)
  (let loop ([auto-count auto-count])
    (if (zero? auto-count)
        (if pfa
            (let loop ([fields-count fields-count] [args args])
              (if (zero? fields-count)
                  (pfa args)
                  (cons (car args) (loop (fx1- fields-count) (cdr args)))))
            args)
        (cons auto-val (loop (fx1- auto-count))))))

;; ----------------------------------------

(define (struct-type? v) (record-type-descriptor? (strip-impersonator v)))

(define/who (procedure-struct-type? v)
  (check who struct-type? v)
  (procedure-struct? v))

(define (struct? v)
  (let ([v (strip-impersonator v)])
    (and (record? v)
         (struct-type-any-transparent? (record-rtd v)))))

(define (struct-info v)
  (cond
   [(impersonator? v)
    (if (record? (impersonator-val v))
        (impersonate-struct-info v)
        (values #f #t))]
   [(not (record? v)) (values #f #t)]
   [else (next-visible-struct-type (record-rtd v))]))

(define (next-visible-struct-type rtd)
  (let loop ([rtd rtd] [skipped? #f])
    (cond
     [(struct-type-immediate-transparent? rtd)
      (values rtd skipped?)]
     [else
      (let ([parent-rtd (record-type-parent rtd)])
        (if parent-rtd
            (loop parent-rtd #t)
            (values #f #t)))])))

(define/who (struct-type-info rtd)
  (check who struct-type? rtd)
  (let ([rtd* (strip-impersonator rtd)])
    (check-inspector-access 'struct-type-info rtd*)
    (let* ([fi (struct-type-field-info rtd*)]
           [parent-rtd* (record-type-parent rtd*)]
           [parent-fi (if parent-rtd*
                          (struct-type-field-info parent-rtd*)
                          empty-field-info)]
           [init-count (get-field-info-init-count fi parent-fi)]
           [auto-count (get-field-info-auto-count fi parent-fi)]
           [parent-total*-count (get-field-info-total*-count parent-fi)])
      (let-values ([(next-rtd* skipped?)
                    (if parent-rtd*
                        (next-visible-struct-type parent-rtd*)
                        (values #f #f))])
        (letrec ([get-results
                  (lambda ()
                    (values (record-type-name rtd*)
                            init-count
                            auto-count
                            (make-position-based-accessor rtd* parent-total*-count (+ init-count auto-count))
                            (make-position-based-mutator rtd* parent-total*-count (+ init-count auto-count))
                            (mutables->immutables (with-global-lock* (hashtable-ref rtd-mutables rtd* '#())) init-count)
                            next-rtd*
                            skipped?))])
          (cond
           [(struct-type-chaperone? rtd)
            (chaperone-struct-type-info rtd get-results)]
           [else
            (get-results)]))))))

(define (check-inspector-access who rtd)
  (unless (struct-type-immediate-transparent? rtd)
    (raise-arguments-error who
                           "current inspector cannot extract info for structure type"
                           "structure type" rtd)))

(define/who (struct-type-make-constructor rtd)
  (check who struct-type? rtd)
  (let ([rtd* (strip-impersonator rtd)])
    (check-inspector-access who rtd*)
    (let ([ctr (struct-type-constructor-add-guards
                (let* ([c (record-constructor rtd*)]
                       [fi (struct-type-field-info rtd*)]
                       [auto-field-adder (get-field-info-auto-adder fi)])
                  (cond
                   [auto-field-adder
                    (procedure-maybe-rename
                     (procedure-reduce-arity
                      (lambda args
                        (apply c (reverse (auto-field-adder (reverse args)))))
                      (get-field-info-init*-count fi))
                     (object-name c))]
                   [else c]))
                rtd*
                #f)])
      (register-struct-constructor! ctr)
      (cond
       [(struct-type-chaperone? rtd)
        (chaperone-constructor rtd ctr)]
       [else ctr]))))

;; Called directly from a schemified declaration that has a guard:
(define (struct-type-constructor-add-guards ctr rtd name)
  (let ([guards (struct-type-guards rtd)]
        [chaparone-undefined? (chaperone-unsafe-undefined? rtd)])
    (if (and (null? guards)
             (not chaparone-undefined?))
        ctr
        (procedure-maybe-rename
         (procedure-reduce-arity
          (let ([base-ctr
                 (if (null? guards)
                     ctr
                     (let ([name (record-type-name rtd)])
                       (lambda args
                         (let loop ([guards guards] [args args])
                           (cond
                            [(null? guards)
                             (apply ctr args)]
                            [else
                             (let ([guard (caar guards)]
                                   [init*-count (cdar guards)])
                               (call-with-values
                                   (lambda ()
                                     (apply guard (append-n args init*-count (list name))))
                                 (lambda results
                                   (unless (= (length results) init*-count)
                                     (apply raise-result-arity-error '|calling guard procedure| init*-count #f results))
                                   (loop (cdr guards)
                                         (if (= init*-count (length args))
                                             results
                                             (append results (list-tail args init*-count)))))))])))))])
            (if chaparone-undefined?
                (lambda args
                  (chaperone-struct-unsafe-undefined (apply base-ctr args)))
                base-ctr))
          (get-field-info-init*-count (struct-type-field-info rtd)))
         (or name (object-name ctr))))))

(define (struct-type-constructor-add-guards* ctr rtd guard name)
  (register-guards! rtd #f guard 'at-end)
  (struct-type-constructor-add-guards ctr rtd name))

(define/who (struct-type-make-predicate rtd)
  (check who struct-type? rtd)
  (let ([rtd* (strip-impersonator rtd)])
    (check-inspector-access who rtd*)
    (let ([pred (escapes-ok
                  (lambda (v)
                    (or (record? v rtd*)
                        (and (impersonator? v)
                             (record? (impersonator-val v) rtd*)))))])
      (register-struct-constructor! pred)
      pred)))

;; ----------------------------------------

(define-record field-info (init*-count   ; includes parent init fields
                           auto*-count   ; includes parent auto fields
                           auto-adder)) ; #f or procedure to add auto fields for constructor

(define empty-field-info 0)

;; Returns either a `field-info` record or a fixnum N that
;; corresponds to `(make-field-info N 0 #f)`.
(define (struct-type-field-info rtd*)
  (or (getprop (record-type-uid rtd*) 'field-info #f)
      (let ([n (#%vector-length (record-type-field-names rtd*))]
            [parent-rtd* (record-type-parent rtd*)])
        ;; If `parent-rtd` is not #f, then we'll get here
        ;; only if were still in the process of setting up
        ;; `rtd`, so we won't have to recur far or often
        ;; construct field-info records
        (if parent-rtd*
            (let ([parent-fi (struct-type-field-info parent-rtd*)])
              (if (fixnum? parent-fi)
                  (+ n parent-fi)
                  (make-field-info (+ n (field-info-init*-count parent-fi))
                                   (field-info-auto*-count parent-fi)
                                   #f)))
            n))))

(define (get-field-info-init*-count fi)
  (if (fixnum? fi)
      fi
      (field-info-init*-count fi)))

(define (get-field-info-auto*-count fi)
  (if (fixnum? fi)
      0
      (field-info-auto*-count fi)))

(define (get-field-info-total*-count fi)
  (if (fixnum? fi)
      fi
      (+ (field-info-init*-count fi)
         (field-info-auto*-count fi))))

(define (get-field-info-init-count fi parent-fi)
  (- (get-field-info-init*-count fi)
     (get-field-info-init*-count parent-fi)))

(define (get-field-info-auto-count fi parent-fi)
  (- (get-field-info-auto*-count fi)
     (get-field-info-auto*-count parent-fi)))

(define (get-field-info-auto-adder fi)
  (if (fixnum? fi)
      #f
      (field-info-auto-adder fi)))

(define (struct-type-total*-field-count rtd*)
  (get-field-info-total*-count (struct-type-field-info rtd*)))

(define (struct-type-parent-total*-count rtd*)
  (let ([p-rtd* (record-type-parent rtd*)])
    (if p-rtd*
        (struct-type-total*-field-count p-rtd*)
        0)))

;; ----------------------------------------

(define (struct-type-field-mutable? rtd pos)
  (let ([mutables (with-global-lock* (hashtable-ref rtd-mutables rtd '#()))])
    (let loop ([j (#%vector-length mutables)])
      (cond
       [(fx= j 0) #f]
       [else
        (let ([j (fx1- j)])
          (or (eqv? pos (#%vector-ref mutables j))
              (loop j)))]))))

;; Returns a list of (cons guard-proc field-count)
(define (struct-type-guards rtd)
  (getprop (record-type-uid rtd) 'guards '()))

(define (register-guards! rtd parent-rtd guard which-end)
  (let* ([parent-rtd* (record-type-parent rtd)]
         [parent-guards (if parent-rtd*
                            (struct-type-guards parent-rtd*)
                            '())])
    (when (or guard (pair? parent-guards) (struct-type-chaperone? parent-rtd))
      (let* ([fi (struct-type-field-info rtd)]
             [parent-guards (if (struct-type-chaperone? parent-rtd)
                                (cons (cons (struct-type-chaperone-guard parent-rtd)
                                            (get-field-info-init*-count
                                             (struct-type-field-info parent-rtd*)))
                                      parent-guards)
                                parent-guards)])
        (let ([new-guards (if guard
                              (if (eq? which-end 'at-start)
                                  ;; Normal:
                                  (cons (cons guard (get-field-info-init*-count fi))
                                        parent-guards)
                                  ;; Internal, makes primitive guards have a natural
                                  ;; error order:
                                  (append parent-guards
                                          (list (cons guard (get-field-info-init*-count fi)))))
                              parent-guards)])
          (putprop (record-type-uid rtd) 'guards new-guards))))))

(define (unsafe-struct*-ref s i)
  (#%$record-ref s i))
(define (unsafe-struct*-set! s i v)
  (#%$record-set! s i v))
(define (unsafe-struct? v r)
  (#3%record? v r))

(define (unsafe-struct-ref s i)
  (if (impersonator? s)
      (let loop ([rtd* (record-rtd (impersonator-val s))])
        (let ([pos (- i (struct-type-parent-total*-count rtd*))])
          (if (fx>= pos 0)
              (impersonate-ref (record-field-accessor rtd* i) rtd* pos s)
              (loop (record-type-parent rtd*)))))
      (unsafe-struct*-ref s i)))

(define (unsafe-struct-set! s i v)
  (if (impersonator? s)
      (let loop ([rtd* (record-rtd (impersonator-val s))])
        (let* ([pos (- i (struct-type-parent-total*-count rtd*))])
          (if (fx>= pos 0)
              (impersonate-set! (record-field-mutator rtd* i) rtd* pos i s v)
              (loop (record-type-parent rtd*)))))
      (unsafe-struct*-set! s i v)))

(define-values (prop:equal+hash equal+hash? equal+hash-ref)
  (make-struct-type-property 'equal+hash
                             (lambda (val info)
                               (check 'guard-for-prop:equal+hash
                                      :test (and (list? val)
                                                 (= 3 (length val))
                                                 (andmap procedure? val)
                                                 (procedure-arity-includes? (car val) 3)
                                                 (procedure-arity-includes? (cadr val) 2)
                                                 (procedure-arity-includes? (caddr val) 2))
                                      :contract (string-append
                                                 "(list/c (procedure-arity-includes/c 3)\n"
                                                 "        (procedure-arity-includes/c 2)\n"
                                                 "        (procedure-arity-includes/c 2))")
                                      val)
                               (cons (gensym) val))))

(define-values (prop:authentic authentic? authentic-ref)
  (make-struct-type-property 'authentic (lambda (val info) #t)))

;; A performance hack: cancels `prop:authentic` in
;; `impersonator-struct`, but leaves Schemify with the impression that
;; the structure type is authentic
(define-values (prop:authentic-override authentic-override? authentic-override-ref)
  (make-struct-type-property 'authentic-override (lambda (val info) #t)))

(define (struct-type-immediate-transparent? rtd)
  (let ([insp (inspector-ref rtd)])
    (and (not (eq? insp none))
         (or (not insp)
             (eq? insp 'prefab)
             (inspector-superior? (|#%app| current-inspector) insp)))))

;; Check whether a structure type is fully transparent
(define (struct-type-transparent? rtd)
  (and (struct-type-immediate-transparent? rtd)
       (let ([p-rtd (record-type-parent rtd)])
         (or (not p-rtd)
             (struct-type-transparent? p-rtd)))))

;; Checks whether a structure type is at least partially trasparent
(define (struct-type-any-transparent? rtd)
  (or (struct-type-immediate-transparent? rtd)
      (let ([p-rtd (record-type-parent rtd)])
        (and p-rtd
             (struct-type-any-transparent? p-rtd)))))

(define (default-struct-equal? s1 s2 eql?)
  (let ([t1 (record-rtd (strip-impersonator s1))]
        [t2 (record-rtd (strip-impersonator s2))])
    (and (eq? t1 t2)
         (struct-type-transparent? t1)
         (let ([n (struct-type-total*-field-count t1)])
           (let loop ([j 0])
             (if (fx= j n)
                 #t
                 (and (eql? (unsafe-struct-ref s1 j)
                            (unsafe-struct-ref s2 j))
                      (loop (fx+ j 1)))))))))
         
(define (default-struct-hash s hash-code)
  (cond
   [(not (impersonator? s))
    ;; Same as the loop below, but uses `unsafe-struct*-ref`:
    (let ([t (record-rtd s)])
      (if (struct-type-transparent? t)
          (let ([n (struct-type-total*-field-count t)])
            (let loop ([j 0] [hc 0])
              (if (fx= j n)
                  hc
                  (loop (fx+ j 1)
                        (hash-code-combine hc (hash-code (unsafe-struct*-ref s j)))))))
          (eq-hash-code s)))]
   [else
    ;; Impersonator variant uses `unsafe-struct-ref` to trigger wrappers:
    (let ([raw-s (impersonator-val s)])
      (let ([t (record-rtd raw-s)])
        (if (struct-type-transparent? t)
            (let ([n (struct-type-total*-field-count t)])
              (let loop ([j 0] [hc 0])
                (if (fx= j n)
                    hc
                    (loop (fx+ j 1)
                          (hash-code-combine hc (hash-code (unsafe-struct-ref s j)))))))
            (eq-hash-code raw-s))))]))

(define struct->vector
  (case-lambda
   [(s dots)
    (if (record? (strip-impersonator s))
        (let ([rtd (record-rtd (strip-impersonator s))])
          ;; Create that vector that has '... for opaque ranges and each field
          ;; value otherwise
          (let-values ([(vec-len rec-len)
                        ;; First, get the vector and record sizes
                        (let loop ([vec-len 1] [rec-len 0] [rtd rtd] [dots-already? #f])
                          (cond
                           [(not rtd) (values vec-len rec-len)]
                           [else
                            (let ([len (#%vector-length (record-type-field-names rtd))])
                              (cond
                               [(struct-type-immediate-transparent? rtd)
                                ;; A transparent region
                                (loop (+ vec-len len) (+ rec-len len) (record-type-parent rtd) #f)]
                               [dots-already?
                                ;; An opaque region that follows an opaque region
                                (loop vec-len (+ rec-len len) (record-type-parent rtd) #t)]
                               [else
                                ;; The start of opaque regions
                                (loop (add1 vec-len) (+ rec-len len) (record-type-parent rtd) #t)]))]))])
            ;; Walk though the record's types again, this time filling in the vector
            (let ([vec (#%make-vector vec-len dots)])
              (vector-set! vec 0 (string->symbol (format "struct:~a" (record-type-name rtd))))
              (let loop ([vec-pos vec-len] [rec-pos rec-len] [rtd rtd] [dots-already? #f])
                (when rtd
                  (let* ([len (#%vector-length (record-type-field-names rtd))]
                         [rec-pos (- rec-pos len)])
                    (cond
                     [(struct-type-immediate-transparent? rtd)
                      ;; Copy over a transparent region
                      (let ([vec-pos (- vec-pos len)])
                        (let floop ([n 0])
                          (cond
                           [(= n len) (loop vec-pos rec-pos (record-type-parent rtd) #f)]
                           [else
                            (vector-set! vec (+ vec-pos n) (unsafe-struct-ref s (+ rec-pos n)))
                            (floop (add1 n))])))]
                     [dots-already?
                      ;; Skip another opaque region
                      (loop vec-pos rec-pos (record-type-parent rtd) #t)]
                     [else
                      ;; The vector already has `dots`
                      (loop (sub1 vec-pos) rec-pos (record-type-parent rtd) #t)]))))
              vec)))
        ;; Any value that is not implemented as a record is treated as
        ;; a fully opaque struct
        (vector (string->symbol (format "struct:~a" ((inspect/object s) 'type))) dots))]
   [(s) (struct->vector s '...)]))

;; ----------------------------------------

(define (make-fields field-count)
  (list->vector
   (let loop ([i 0])
     (if (= i field-count)
         '()
         (cons `(mutable ,(string->symbol (format "f~a" i)))
               (loop (fx1+ i)))))))

;; ----------------------------------------
;; Convenience for Rumble implementation:

(define-syntax struct
  (lambda (stx)
    (syntax-case stx  (:guard)
      [(_ name (field ...))
       #'(struct name #f (field ...))]
      [(_ name (field ...) :guard guard-expr)
       #'(struct name #f (field ...) :guard guard-expr)]
      [(_ name parent (field ...))
       #'(struct name parent (field ...) :guard #f)]
      [(_ name parent (field ...) :guard guard-expr)
       (let ([make-id (lambda (id fmt . args)
                        (datum->syntax id
                                       (string->symbol (chez:apply format fmt args))))])
         (with-syntax ([struct:name (make-id #'name "struct:~a" (syntax->datum #'name))]
                       [unsafe-make-name (make-id #'name "unsafe-make-~a" (syntax->datum #'name))]
                       [authentic-name? (make-id #'name "authentic-~a?" (syntax->datum #'name))]
                       [name? (make-id #'name "~a?" (syntax->datum #'name))]
                       [(name-field ...) (map (lambda (field)
                                                (make-id field "~a-~a" (syntax->datum #'name) (syntax->datum field)))
                                              #'(field ...))]
                       [(field-index ...) (let loop ([fields #'(field ...)] [accum '()] [pos 0])
                                            (cond
                                             [(null? fields) (reverse accum)]
                                             [else (loop (cdr fields) (cons pos accum) (add1 pos))]))]
                       [struct:parent (if (syntax->datum #'parent)
                                          (make-id #'parent "struct:~a" (syntax->datum #'parent))
                                          #f)])
           (with-syntax ([ctr-expr (with-syntax ([mk #'(record-constructor (make-record-constructor-descriptor struct:name #f #f))])
                                     (if (or (syntax->datum #'parent) (syntax->datum #'guard-expr))
                                         #'(struct-type-constructor-add-guards* mk struct:name guard-expr 'name)
                                         #'mk))]
                         [uid (datum->syntax #'name ((current-generate-id) (syntax->datum #'name)))])
             #'(begin
                 (define struct:name (make-record-type-descriptor 'name struct:parent 'uid #f #f '#((immutable field) ...)))
                 (define unsafe-make-name (record-constructor (make-record-constructor-descriptor struct:name #f #f)))
                 (define name ctr-expr)
                 (define authentic-name? (record-predicate struct:name))
                 (define name? (lambda (v) (or (authentic-name? v)
                                               (and (impersonator? v)
                                                    (authentic-name? (impersonator-val v))))))
                 (define name-field
                   (let ([name-field (record-accessor struct:name field-index)])
                     (lambda (v)
                       (if (authentic-name? v)
                           (name-field v)
                           (pariah (impersonate-ref name-field struct:name field-index v))))))
                 ...
                 (define dummy
                   (begin
                     (register-struct-named! struct:name)
                     (register-struct-constructor! name)
                     (register-struct-field-accessor! name-field struct:name field-index) ...
                     (record-type-equal-procedure struct:name default-struct-equal?)
                     (record-type-hash-procedure struct:name default-struct-hash)
                     (inspector-set! struct:name #f)))))))])))

(define-syntax define-struct
  (lambda (stx)
    (syntax-case stx ()
      [(_ name . rest)
       (with-syntax ([make-name
                      (datum->syntax #'name
                                     (string->symbol (format "make-~a" (syntax->datum #'name))))])
         #'(begin
             (struct name . rest)
             (define make-name name)))])))

(define (register-struct-named! rtd)
  (with-global-lock* (hashtable-set! rtd-props rtd '())))
