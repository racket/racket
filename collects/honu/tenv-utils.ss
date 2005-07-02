(module tenv-utils mzscheme

  (require "readerr.ss"
           "ast.ss"
           "tenv.ss"
           "private/typechecker/type-utils.ss"
           (lib "plt-match.ss")
           (lib "struct.ss")
           (lib "list.ss" "srfi" "1"))

  (define (make-struct-type-decls inits mfidefns)
    (define (convert-to-decl d)
      (cond
        ;; can come from inits
        [(honu:formal? d)
         (make-honu:field-decl (honu:ast-stx d)
                               (honu:formal-name d)
                               (honu:formal-type d))]
        ;; can come from mdidefns
        [(honu:init-field? d)
         (make-honu:field-decl (honu:ast-stx d)
                               (honu:init-field-name d)
                               (honu:init-field-type d))]
        [(honu:field? d)
         (make-honu:field-decl (honu:ast-stx d)
                               (honu:field-name d)
                               (honu:field-type d))]
        [(honu:method? d)
         (make-honu:method-decl (honu:ast-stx d)
                                (honu:method-name d)
                                (honu:method-type d)
                                (map honu:formal-type (honu:method-formals d)))]))
    (map convert-to-decl (append inits mfidefns)))
  
  (define (make-struct-export typ inits mdidefns members)
    (define (grab-name d)
      (cond
        ;; can come from members
        [(tenv:member? d)      (tenv:member-name d)]
        ;; can come from inits
        [(honu:formal? d)      (honu:formal-name d)]
        ;; can come from mdidefns
        [(honu:init-field? d)  (honu:init-field-name d)]
        [(honu:field? d)       (honu:field-name d)]
        [(honu:method? d)      (honu:method-name d)]))
    (let ([binds (map (lambda (m)
                        (let ([name (grab-name m)])
                          (make-honu:exp-bind name name))) (append inits mdidefns members))])
      (make-honu:export #f typ binds)))
  
  (define (tenv-create-error skipped)
    ;; only subclasses and substructs can be skipped, so if the base of one of them
    ;; is in the skipped list as well, then we have a cycle.  If not, then there's
    ;; some missing definition.
    ;;
    ;; well, this isn't true anymore, so check to see if it's an iface first, which
    ;; is the other possibility for skipping.  We need to check a similar kind of
    ;; thing to see if the type hierarchy is a cycle or there's just something missing.
    ;;
    ;; FIXME: This function almost certainly does not always give the correct error message
    ;; (for example, it may give a cycle error when it turns out that the first thing
    ;; in the skipped list just depends on something else which just had stuff missing,
    ;; so there should have been a definitions needed missing error.  Will revisit later.
    (if (honu:iface? (car skipped))
        (let ([supers (honu:iface-supers (car skipped))])
          (if (find (lambda (d)
                      (cond
                        [(honu:iface? d) (s:member (honu:iface-name d) supers tenv-key=?)]
                        [else            #f]))
                    (cdr skipped))
              (raise-read-error-with-stx
               (format "Type ~a is involved in a type hierarchy cycle"
                       (printable-key (honu:iface-name (car skipped))))
               (honu:iface-name (car skipped)))
              (raise-read-error-with-stx
               (format "At least one supertype of type ~a is missing"
                       (printable-key (honu:iface-name (car skipped))))
               (honu:iface-name (car skipped)))))
        (let ([class-name (cond
                            [(honu:subclass?  (car skipped)) (honu:subclass-name  (car skipped))]
                            [(honu:substruct? (car skipped)) (honu:substruct-name (car skipped))])]
              [base-name  (cond
                            [(honu:subclass?  (car skipped)) (honu:subclass-base  (car skipped))]
                            [(honu:substruct? (car skipped)) (honu:substruct-base (car skipped))])])
          (if (find (lambda (d)
                      (cond
                        [(honu:subclass?  d) (tenv-key=? base-name (honu:subclass-name  d))]
                        [(honu:substruct? d) (tenv-key=? base-name (honu:substruct-name d))]
                        [else                #f]))
                    (cdr skipped))
              (raise-read-error-with-stx
               (format "Class ~a is involved in a class hierarchy cycle" (printable-key class-name))
               class-name)
              (raise-read-error-with-stx
               (format "Definitions needed for definition of class ~a are missing" (printable-key class-name))
               class-name)))))
  
  (provide add-defns-to-tenv add-defn-to-tenv)
  (define (add-defns-to-tenv defns tenv)
    (let loop ([defns     defns]
               [skipped   '()]
               [changed?  #f]
               [new-defns '()])
      (cond
        ;; we're done, so return the new defns
        [(and (null? defns) (null? skipped))
         (reverse new-defns)]
        ;; we skipped some, so go back and check those.
        [(null? defns)
         (if changed?
             (loop skipped '() #f new-defns)
             ;; we didn't change anything on the last run,
             ;; so we must either have a cycle in the class graph
             ;; or there are missing definitions.  Raise an
             ;; appropriate error for the first definition.
             (tenv-create-error skipped))]
        ;; for functions and top level bindings, we
        ;; don't add them here, so just skip them.
        [(or (honu:function? (car defns))
             (honu:bind-top? (car defns)))
         (loop (cdr defns) skipped #t (cons (car defns) new-defns))]
        [(honu:iface? (car defns))
         (let loop2 ([supers (map honu:type-iface-name (honu:iface-supers (car defns)))])
           (cond
             ;; if we went through all the supers with them being defined,
             ;; then we can add this type as well.
             [(null? supers)
              (loop (cdr defns) skipped #t (cons (add-defn-to-tenv (car defns) tenv) new-defns))]
             ;; if there is an entry, we check to make sure it's a type, and
             ;; if it is, then we continue looping in the inner loop
             [(get-tenv-entry tenv (car supers))
              =>
              (lambda (e)
                (if (not (tenv:type? e))
                    (raise-read-error-with-stx
                     (format "~a is not a type" (printable-key (car supers)))
                     (car supers))
                    (loop2 (cdr supers))))]
             ;; if there is no entry, then we can't add this type yet, so
             ;; recur on the outer loop with this type being skipped.
             [else
              (loop (cdr defns) (cons (car defns) skipped) changed? new-defns)]))]
        ;; for classes and mixins, we don't use the tenv to create
        ;; their entries, so we just run them through as we hit them.
        [(or (honu:class? (car defns))
             (honu:mixin? (car defns)))
         (loop (cdr defns) skipped #t (cons (add-defn-to-tenv (car defns) tenv) new-defns))]
        ;; for structs, we will get back a list of two things: the new type
        ;; and the new class definition, so append those onto new-defns
        [(honu:struct? (car defns))
         (match (car defns)
           [(struct honu:struct (stx name type final? impls inits members exports))
            (let ([new-iface (make-honu:iface stx (honu:type-iface-name type) (list)
                                              (make-struct-type-decls inits members))]
                  [new-class (make-honu:class stx name type final? (cons type impls) inits members 
                                              (cons (make-struct-export type inits members (list)) exports))])
              (loop (cdr defns) skipped #t (cons (add-defn-to-tenv new-class tenv)
                                                 (cons (add-defn-to-tenv new-iface tenv) new-defns))))])]
        ;; for subclasses, we check to make sure the base (and its self-type) and
        ;; the mixin (and its arg-type) are in the tenv.  If not, skip it.
        ;; Give appropriate errors for each thing that can go wrong.
        [(honu:subclass? (car defns))
         (let* ([base     (get-tenv-entry tenv (honu:subclass-base (car defns)))]
                [selftype (if (and base (tenv:class? base))
                              (get-tenv-entry tenv (honu:type-iface-name (tenv:class-sub-type base)))
                              #f)]
                [mixin    (get-tenv-entry tenv (honu:subclass-mixin (car defns)))]
                [argtype  (if (and mixin (tenv:mixin? mixin))
                              (get-tenv-entry tenv (honu:type-iface-name (tenv:mixin-arg-type mixin)))
                              #f)])
           (cond
             [(and base (not (tenv:class? base)))
              (raise-read-error-with-stx
               "Base class for subclass definition is not a class"
               (honu:subclass-base (car defns)))]
             [(and selftype (not (tenv:type? selftype)))
              (raise-read-error-with-stx
               "Selftype for class is not a type"
               (honu:ast-stx (tenv:class-sub-type base)))]
             [(and mixin (not (tenv:mixin? mixin)))
              (raise-read-error-with-stx
               "Mixin for subclass definition is not a mixin"
               (honu:subclass-mixin (car defns)))]
             [(and argtype (not (tenv:type? argtype)))
              (raise-read-error-with-stx
               "Argument type for mixin is not a type"
               (honu:ast-stx (tenv:mixin-arg-type mixin)))]
             [(and base selftype mixin argtype)
              ;; if the base is final, then we can't extend it.
              (if (tenv:class-final? base)
                  (raise-read-error-with-stx
                   (format "Cannot apply mixin to final class ~a"
                           (printable-key base))
                   base))
              ;; if the base's selftype does not match the mixin's argtype,
              ;; we cannot apply the mixin to the base.
              (if (not (<:_P tenv (tenv:class-sub-type base) (tenv:mixin-arg-type mixin)))
                  (raise-read-error-with-stx
                   (format "Class ~a (~a) is not of an appropriate type (~a) for mixin ~a"
                           (printable-key  (honu:subclass-base (car defns)))
                           (printable-type (tenv:class-sub-type base))
                           (printable-type (tenv:mixin-arg-type mixin))
                           (printable-key  (honu:subclass-mixin (car defns))))
                   (honu:subclass-base (car defns))))
              (loop (cdr defns) skipped #t (cons (add-defn-to-tenv (car defns) tenv) new-defns))]
             ;; if we get here, we cannot yet make the entry for this subclass,
             ;; so skip it.
             [else
              (loop (cdr defns) (cons (car defns) skipped) changed? new-defns)]))]
        ;; for substructs, we just deconstruct it and then let the subclass logic catch any problems.
        ;; we do a couple of checks, because getting the type right for the substruct requires having
        ;; the argtype of the substruct.
        [(honu:substruct? (car defns))
         (match (car defns)
           [(struct honu:substruct (stx name type base arg-type final? impls inits withs super-new
                                        members-before members-after exports))
            (let ([argtype (get-tenv-entry tenv (honu:type-iface-name arg-type))])
              (cond
                [(and argtype (not (tenv:type? argtype)))
                  (raise-read-error-with-stx
                   "Type at which we are extending is not a type"
                   (honu:ast-stx arg-type))]
                [argtype
                 (let* ([new-iface  (make-honu:iface stx (honu:type-iface-name type) (list arg-type)
                                                     (make-struct-type-decls inits
                                                                             (append members-before members-after)))]
                        [mixin-name (datum->syntax-object name (string->symbol
                                                                (string-append "$" (symbol->string (printable-key name))))
                                                          name)]
                        [new-mixin  (make-honu:mixin stx mixin-name type arg-type final? (cons type impls) inits withs
                                                     super-new members-before members-after
                                                     (cons (make-struct-export type
                                                                         inits 
                                                                         (append members-before members-after)
                                                                         (tenv:type-members argtype))
                                                           exports))]
                        [new-sclass (make-honu:subclass stx name base mixin-name)])
                   (loop (cons new-sclass (cdr defns)) skipped #t  (cons (add-defn-to-tenv new-mixin tenv)
                                                                         (cons (add-defn-to-tenv new-iface tenv) new-defns))))]
                [else
                 (loop (cdr defns) (cons (car defns) skipped) changed? new-defns)]))])])))
  
  (define (check-super-for-members tenv name members super-name)
    (match (get-tenv-entry tenv super-name)
      [(struct tenv:type (_ _ super-members super-inherited))
       ;; here we make sure to use both defined members and inherited members
       (let loop ([super-members (append super-members super-inherited)]
                  [inherited     '()])
         (cond
           ;; we've checked all the super members
           [(null? super-members)
            (reverse inherited)]
           ;; if we find a member of the subtype that matches the currently inspected member of
           ;; the supertype...
           [(find (lambda (m)
                    (tenv-key=? (tenv:member-name m)
                                (tenv:member-name (car super-members))))
                  members)
            =>
            (lambda (m)
              ;; if we eventually allow co-/contra-variance here, this is where
              ;; we'd do it.
              (if (honu:type-disp? (tenv:member-type (car super-members)))
                  (if (<:_P tenv (tenv:member-type m) (tenv:member-type (car super-members)))
                      (loop (cdr super-members) inherited)
                      (raise-read-error-with-stx
                       (format "Type ~a defines member ~a with type ~a, is not a subtype of type ~a as defined in supertype ~a"
                               (printable-key  name)
                               (printable-key  (tenv:member-name m))
                               (printable-type (tenv:member-type m))
                               (printable-type (tenv:member-type (car super-members)))
                               (printable-key  super-name))
                       (tenv:member-stx m)))
                  ;; this handles mutable fields -- we don't have immutable fields yet
                  (if (type-equal? tenv (tenv:member-type m) (tenv:member-type (car super-members)))
                      (loop (cdr super-members) inherited)
                      (raise-read-error-with-stx
                       (format "Type ~a defines member ~a with type ~a, was defined with type ~a in supertype ~a"
                               (printable-key  name)
                               (printable-key  (tenv:member-name m))
                               (printable-type (tenv:member-type m))
                               (printable-type (tenv:member-type (car super-members)))
                               (printable-key  super-name))
                       (tenv:member-stx m)))))]
           ;; if there was no match, then this is one we inherited and for which we did not give
           ;; an explicit declaration.
           [else
            (loop (cdr super-members) (cons (cons super-name (car super-members)) inherited))]))]))
  
  (define (mangle-disp-type iface member)
    (let ([member-type (tenv:member-type member)])
      (if (honu:type-disp? member-type)
          (copy-struct tenv:member member
            [tenv:member-type (make-method-type (honu:ast-stx member-type)
                                                iface
                                                (honu:type-disp-arg member-type)
                                                (honu:type-disp-ret member-type))])
          member)))

  (define (type-equal-modulo-disp? tenv t1 t2)
    (let ([t1 (if (honu:type-disp? t1)
                  (make-func-type (honu:ast-stx  t1)
                                  (honu:type-disp-arg t1)
                                  (honu:type-disp-ret t1))
                  t1)]
          [t2 (if (honu:type-disp? t2)
                  (make-func-type (honu:ast-stx  t2)
                                  (honu:type-disp-arg t2)
                                  (honu:type-disp-ret t2))
                  t2)])
      (type-equal? tenv t1 t2)))
  
  (define (check-and-remove-duplicate-members tenv subtype inherited-members)
    (let loop ([inherited-members inherited-members]
               [unique-members    '()])
      (if (null? inherited-members)
          (reverse unique-members)
          (let ([current-member (cdr (car inherited-members))])
            (let-values ([(matching-members rest-members)
                          (partition (lambda (p)
                                       (tenv-key=? (tenv:member-name current-member)
                                                   (tenv:member-name (cdr p))))
                                     (cdr inherited-members))])
              (let loop2 ([matching-members matching-members])
                (cond
                  [(null? matching-members)
                   (loop rest-members (cons (mangle-disp-type (make-iface-type subtype subtype)
                                                              current-member)
                                            unique-members))]
                  ;; members coming from supers that are _not_ redefined must be exactly equal
                  ;; (modulo the dispatch arguments of methods)
                  ;;
                  ;; doesn't matter which we keep, so we'll just keep the first one that matched.
                  [(type-equal-modulo-disp? tenv 
                                            (tenv:member-type current-member)
                                            (tenv:member-type (cdr (car matching-members))))
                   (loop2 (cdr matching-members))]
                  [else
                   (raise-read-error-with-stx
                    (format "For type ~a, supertype ~a has type ~a for member ~a, whereas supertype ~a has type ~a"
                            (printable-key  subtype)
                            (printable-key  (car (car inherited-members)))
                            (printable-type (tenv:member-type current-member))
                            (printable-key  (tenv:member-name current-member))
                            (printable-key  (car (car matching-members)))
                            (printable-type (tenv:member-type (cdr (car matching-members)))))
                    subtype)])))))))
  
  (define (add-defn-to-tenv defn tenv)
    (match defn
      ;; for types, we need to recur over our supertypes, make sure that we don't have any definitions that countermand
      ;; those in our super classes (which will also make sure that our superclass definitions are consistent), and
      ;; then we will add any member definitions in them that are _not_ declared in this type.
      ;;
      ;; If we get here, we know that all the supers are in the tenv and are type entries, so we can use
      ;; get-type-entry safely.
      [(struct honu:iface (src-stx name supers members))
       (let* ([tenv-members (convert-members (make-iface-type name name) members)]
              [inherited-decls
               (apply append (map (lambda (n) (check-super-for-members tenv name tenv-members n)) 
                                  (map honu:type-iface-name supers)))]
              [unique-inherited
               ;; remove duplicate entries for the same member name, making sure they match.
               (check-and-remove-duplicate-members tenv name inherited-decls)])
         (extend-tenv name 
                      (make-tenv:type src-stx supers tenv-members unique-inherited)
                      tenv)
         defn)]
      ;; for classes and mixins, just add a new appropriate entry.
      [(struct honu:class (src-stx name t f? impls inits defns _))
       (extend-tenv name (make-tenv:class src-stx t impls
                                          (get-inits inits defns)
                                          f? #f) tenv)
       defn]
      [(struct honu:mixin (src-stx name type arg-type final? impls inits
                                   withs _ defns-before defns-after _))
       (extend-tenv name (make-tenv:mixin src-stx arg-type type impls
                                          (get-inits inits
                                                     (append defns-before 
                                                             defns-after))
                                          withs final?) tenv)
       defn]
      ;; all the heavy lifting of subclasses is in generate-subclass-tenv,
      ;; which does things like make sure that the withs of the mixin are satisfied
      ;; by the base, collects all the inits needed for the resulting class, etc.
      [(struct honu:subclass (src-stx name base mixin))
       (extend-tenv name (generate-subclass-tenv defn tenv) tenv)
       defn]))

  (define (convert-members iface members)
    (let loop ([members members]
               [converted '()])
      (if (null? members)
          (reverse converted)
          (match (car members)
            [(struct honu:field-decl (stx name type))
             (loop (cdr members)
                   (cons (make-tenv:member stx name type) converted))]
            [(struct honu:method-decl (stx name type arg-types))
             (loop (cdr members)
                   (cons (make-tenv:member stx name (make-method-type stx
                                                                      iface
                                                                      (make-tuple-type stx arg-types) 
                                                                      type))
                         converted))]))))
  
  (define (get-inits inits defns)
    (let ([init-fields (filter (lambda (d)
                                 (honu:init-field? d))
                               defns)])
      (append (map (lambda (i)
                     (make-tenv:init (honu:formal-name i)
                                     (honu:formal-type i)
                                     #f))
                   inits)
              (map (lambda (d)
                     (if (not (honu:init-field-value d))
                         (make-tenv:init (honu:init-field-name d)
                                         (honu:init-field-type d)
                                         #f)
                         (make-tenv:init (honu:init-field-name d)
                                         (honu:init-field-type d)
                                         #t)))
                   init-fields))))

  (define (generate-subclass-tenv defn tenv)
    (let ([base  (get-class-entry tenv (honu:subclass-base defn))]
          [mixin (get-mixin-entry tenv (honu:subclass-mixin defn))])
      (let ([new-inits (remove-used-inits tenv defn
                                          (tenv:class-inits base)
                                          (tenv:mixin-withs mixin))])
        (make-tenv:class (honu:ast-stx defn)
                         (tenv:mixin-sub-type mixin)
                         (tenv:mixin-impls mixin)
                         (append (tenv:mixin-inits mixin)
                                 new-inits)
                         (tenv:mixin-final? mixin)
                         (honu:subclass-base defn)))))

  (define (remove-used-inits tenv defn old-inits withs)
    (let loop ([old-inits  old-inits]
               [withs      withs]
               [new-inits  '()])
      (if (null? old-inits)
          (if (not (null? withs))
              (raise-read-error-with-stx
               (format "Class ~a does not have an init arg ~a with the correct type"
                       (printable-key (honu:subclass-base defn))
                       (printable-key (honu:formal-name (car withs))))
               (honu:subclass-base defn))
              (reverse new-inits))
          (let* ([curr (car old-inits)]
                 [index (list-index (lambda (w)
                                      (tenv-key=? (honu:formal-name w) (tenv:init-name curr)))
                                    withs)])
            (if index
                (if (<:_P tenv (honu:formal-type (list-ref withs index)) (tenv:init-type curr))
                    (loop (cdr old-inits)
                          (append (take withs index)
                                  (drop withs (+ index 1)))
                          new-inits)
                    (raise-read-error-with-stx
                     (format "Mixin ~a needs an incompatible type for init arg ~a"
                             (printable-key (honu:subclass-mixin defn))
                             (printable-key (honu:formal-name (car withs))))
                     (honu:subclass-mixin defn)))
                (loop (cdr old-inits)
                      withs
                      (cons curr new-inits)))))))
  
  (provide display-lenv display-tenv)
  (define (display-lenv lenv)
    (tenv-for-each lenv
                   (lambda (k v)
                     (display (format "~a = ~a~%"
                                      (printable-key k)
                                      (printable-type (tenv:value-type v)))))))
  
  (define (display-tenv tenv)
    (tenv-for-each tenv
                   (lambda (k v)
                     (display (format "~a = " (printable-key k)))
                     (display-tenv-entry v))))
  
  (define (display-tenv-entry entry)
    (match entry
      [(struct tenv:type (_ supers members inherited))
       (display (format "type {~%"))
       (display (format "  supers = "))
       (if (null? supers) (display "(none)"))
       (for-each (lambda (s) (display (format "~a " (printable-type s)))) supers)
       (newline)
       (display (format "  members = "))
       (if (null? members) (display "(none)"))
       (newline)
       (for-each (lambda (m)
                   (match m
                     [(struct tenv:member (_ name type))
                      (display (format "    ~a : ~a~%"
                                       (printable-key name)
                                       (printable-type type)))]))
                 members)
       (display (format "  inherited members = "))
       (if (null? inherited) (display "(none)"))
       (newline)
       (for-each (lambda (m)
                   (match m
                     [(struct tenv:member (_ name type))
                      (display (format "    ~a : ~a~%"
                                       (printable-key name)
                                       (printable-type type)))]))
                 inherited)
       (display (format "}~%"))]
      [(struct tenv:class (_ sub-type impls inits final? super))
       (display (format "class {~%"))
       (display (format "  final? = ~a~%" (if final? "yes" "no")))
       (display (format "  super = ~a~%" (if super (printable-key super) "(none)")))
       (display (format "  sub-type = ~a~%" (printable-type sub-type)))
       (display (format "  impls = "))
       (for-each (lambda (s) (display (format "~a " (printable-type s)))) impls)
       (if (null? impls) (display "(none)"))
       (newline)
       (display (format "  inits = "))
       (if (null? inits) (display "(none)"))
       (newline)
       (for-each (lambda (i) (display (format "    ~a : ~a ~a~%"
                                              (printable-key (tenv:init-name i))
                                              (printable-type (tenv:init-type i))
                                              (if (tenv:init-optional? i) "(opt)" ""))))
                 inits)
       (display (format "}~%"))]
      [(struct tenv:mixin (_ arg-type sub-type impls inits withs final?))
       (display (format "mixin {~%"))
       (display (format "  final? = ~a~%" (if final? "yes" "no")))
       (display (format "  arg-type = ~a~%" (printable-type arg-type)))
       (display (format "  sub-type = ~a~%" (printable-type sub-type)))
       (display (format "  impls = "))
       (for-each (lambda (s) (display (format "~a " (printable-type s)))) impls)
       (if (null? impls) (display "(none)"))
       (newline)
       (display (format "  inits = "))
       (if (null? inits) (display "(none)"))
       (newline)
       (for-each (lambda (i) (display (format "    ~a : ~a ~a~%"
                                              (printable-key (tenv:init-name i))
                                              (printable-type (tenv:init-type i))
                                              (if (tenv:init-optional? i) "(opt)" ""))))
                 inits)
       (display (format "  withs = "))
       (if (null? withs) (display "(none)"))
       (newline)
       (for-each (lambda (i) (display (format "    ~a : ~a ~a~%"
                                              (printable-key (tenv:init-name i))
                                              (printable-type (tenv:init-type i))
                                              (if (tenv:init-optional? i) "(opt)" ""))))
                 withs)
       (display (format "}~%"))]))
  )
