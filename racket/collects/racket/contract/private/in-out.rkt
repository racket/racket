#lang racket/base

(provide (protect-out (for-syntax generate-in/out-code)))

(require (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/struct-info
                     setup/path-to-relative
                     "../../private/struct-util.rkt"
                     (prefix-in a: "helpers.rkt")
                     (rename-in syntax/private/boundmap
                                ;; the private version of the library
                                ;; (the one without contracts)
                                ;; has these old, wrong names in it.
                                [make-module-identifier-mapping make-free-identifier-mapping]
                                [module-identifier-mapping-get free-identifier-mapping-get]
                                [module-identifier-mapping-put! free-identifier-mapping-put!]))
         "module-boundary-ctc.rkt"
         "arrow-val-first.rkt"
         "guts.rkt"
         "exists.rkt"
         "blame.rkt"
         syntax/location
         syntax/srcloc)

(define-for-syntax (self-ctor-transformer orig stx)
  (with-syntax ([orig orig])
    (syntax-case stx ()
      [(_ arg ...) (datum->syntax stx
                                  (syntax-e (syntax (orig arg ...)))
                                  stx
                                  stx)]
      [_ (syntax orig)])))

;; make-contract-out-redirect-struct-info
;; : (-> (-> (and/c struct-info? list?)) (-> identifier?) struct-info?)
;; Create a struct-info? value from two thunks:
;;  the 1st must be a valid argument for `make-struct-info`, and
;;  the 2nd must return an identifier for a structure type descriptor.
;; The 2nd thunk is used to recover the original names for a struct --- from before
;;  `contract-out` started to mangle them.
;;
;; make-applicable-contract-out-redirect-struct-info
;; : (-> (-> (and/c struct-info? list?)) (-> identifier?) (-> identifier?) struct-info?)
;; Similar to the above, but the 3rd thunk must return an identifier for a
;;  contract-protected constructor.
;; Creates a value that can be applied to construct instances of the struct type.
;;
;; undo-contract-out-redirect
;; : (-> any/c (or/c identifier? #f))
;; Return the original struct name associated with the argument, or #f if
;;  the input is not an indirect struct info.
(define-values-for-syntax [make-contract-out-redirect-struct-info
                           make-contract-out-redirect/field-struct-info
                           make-applicable-contract-out-redirect-struct-info
                           make-applicable-contract-out-redirect/field-struct-info
                           undo-contract-out-redirect]
  (let ()
    (define-values (struct:r make-r r? r-ref r-set!)
      (make-struct-type
       'contract-out-redirect-struct-info struct:struct-info
       1 0 #f
       '()
       (current-inspector) #f '(0)))

    (define-values (struct:r/field make-r/field r/field? r/field-ref r/field-set!)
      (make-struct-type
       'contract-out-redirect/field-struct-info struct:r
       1 0 #f
       (list (cons prop:struct-field-info
                   (lambda (rec)
                     (r/field-ref rec 0))))))

    (define-values (struct:app-r make-app-r app-r? app-r-ref app-r-set!)
      (make-struct-type
       'applicable-contract-out-redirect-struct-info struct:r
       1 0 #f
       (list (cons prop:procedure
                   (lambda (v stx)
                     (self-ctor-transformer ((app-r-ref v 0)) stx))))
       (current-inspector) #f '(0)))

    (define-values (struct:app-r/field
                    make-app-r/field
                    app-r/field?
                    app-r/field-ref
                    app-r/field-set!)
      (make-struct-type
       'applicable-contract-out-redirect/field-struct-info struct:app-r
       1 0 #f
       (list (cons prop:struct-field-info
                   (lambda (rec)
                     (app-r/field-ref rec 0))))))

    (define (undo-contract-out-redirect v)
      (and (r? v) ((r-ref v 0))))

    (values make-r make-r/field make-app-r make-app-r/field undo-contract-out-redirect)))


(define-for-syntax current-unprotected-submodule-name (make-parameter #f))

;; make-unprotected-submodule-code
;; : (-> (-> syntax?) (listof syntax?))
;; Calls `thunk` if `current-unprotected-submodule-name` is non-#f,
;; incorporating the resulting syntax object into a singleton list of
;; `module+` form; otherwise returns empty list.
(define-for-syntax (make-unprotected-submodule-code thunk)
  (define upe-submod (current-unprotected-submodule-name))
  (if upe-submod (list #`(module+ #,upe-submod #,(thunk))) null))

(define-for-syntax (id-for-one-id mangled-id-scope who id mangle-for-maker?)
  (mangled-id-scope
   ((if mangle-for-maker?
        a:mangle-id-for-maker
        a:mangle-id)
    (format "~a/contract-id" who)
    id)))

;; given the syntax for an identifier and a contract,
;; builds a begin expression for the entire contract
;; and for `define-module-boundary-contract/proc`
;; the first syntax object is used for source locations
(define-for-syntax (code-for-one-id/new-name who stx id reflect-id ctrct/no-prop user-rename-id
                                             mangled-id-scope
                                             provide?
                                             [mangle-for-maker? #f]
                                             [generate-provides? #t]

                                             ;; if we're doing provides, we can lift to the end
                                             ;; otherwise, when we're doing `contract-in`, we
                                             ;; want those definitions to be part of the code
                                             ;; that's returned (but this is only the default)
                                             #:lift? [lift? provide?]
                                             #:start-swapped? [start-swapped? (not provide?)])
  (define ex-id (or reflect-id id))
  (define id-rename-without-source (id-for-one-id mangled-id-scope who (or user-rename-id reflect-id id) mangle-for-maker?))
  (define id-rename (datum->syntax id-rename-without-source
                                   (syntax-e id-rename-without-source)
                                   ex-id
                                   id-rename-without-source))
  (with-syntax ([ctrct (syntax-property
                        (syntax-property
                         ctrct/no-prop
                         'racket/contract:contract-on-boundary
                         (gensym 'in-out/contract-boundary))
                        'inferred-name ex-id)]
                [external-name (or user-rename-id id)])
    (define srcloc-id
      (cond
        [(and (not provide?) (syntax-source ex-id))
         ex-id]
        [(syntax-source id)
         id]
        [(and user-rename-id
              (syntax-source user-rename-id))
         user-rename-id]
        [else ex-id]))
    (define name-with-contract (if provide? id-rename id))
    (define name-before-contract (if provide? id id-rename))
    (with-syntax ([code
                   (syntax-property
                    (quasisyntax/loc stx
                      (begin #,(define-module-boundary-contract/proc name-with-contract
                                 name-before-contract
                                 #'ctrct
                                 ex-id
                                 (if provide? #'external-name ex-id)
                                 #`(quote-srcloc #,srcloc-id)
                                 '#,who
                                 pos-module-source-id
                                 #f
                                 lift?
                                 start-swapped?)
                             ;; `upe-id` is punned as an indicator of whether the `provide`s will be
                             ;; generated as well as the uncontracted identifier to be exported.  This
                             ;; is fine because we always need to generate both `provide`s anyway.
                             #,@(cond
                                  [generate-provides?
                                   (add-remapping id-rename #'external-name)
                                   (make-unprotected-submodule-code
                                    (lambda ()
                                      #`(provide (rename-out [#,id external-name]))))]
                                  [else '()])))
                    (if provide? 'provide/contract-original-contract 'require/contract-original-contract)
                    (vector #'external-name #'ctrct))])
      #`(code #,name-with-contract))))


(define-for-syntax pos-module-source-id
  ;; Avoid context on this identifier, since it will be defined
  ;; in another module, and the definition may have to pull
  ;; along all context to support `module->namespace`:
  (datum->syntax #f 'pos-module-source))

;; code-for-one-id : syntax syntax syntax (union syntax #f) -> syntax
;; given the syntax for an identifier and a contract,
;; builds a begin expression for the entire contract and
;; define-module-boundary-contract/proc use;
;; the first syntax object is used for source locations
(define-for-syntax (code-for-one-id who stx id reflect-id ctrct user-rename-id mangled-id-scope provide?)
  (with-syntax ([(code id) (code-for-one-id/new-name who stx id reflect-id ctrct user-rename-id mangled-id-scope provide?)])
    (syntax code)))

;; remappings control how variables are either imported or exported
;; as part of contract-in / contract-out. In the case of contract-in,
;; a remapping imports the second name and binds it to the first name
;; in the module where the `contract-in` appears. In the case of
;; contract-out, a remapping provides the second name, expecting the
;; first name to be bound.
(define-for-syntax current-remappings (make-parameter #f))
(define-for-syntax (add-remapping internal-name external-name)
  (define cr (current-remappings))
  (unless cr (error 'add-remapping "current-remappings not set"))
  (set-box! cr (cons (cons internal-name external-name) (unbox cr))))

(define-for-syntax (generate-in/out-code who stx p/c-clauses
                                         pos-module-source-expression
                                         #:unprotected-submodule-name unprotected-submodule-name
                                         #:just-check-errors? just-check-errors?
                                         #:provide? provide?
                                         #:struct-name-remappings [struct-name-remappings (make-free-identifier-mapping)])
  (define remappings (box '()))
  (define code
    (parameterize ([current-remappings remappings])
      (generate-in/out-code/remappings
       who stx p/c-clauses unprotected-submodule-name
       just-check-errors?
       provide?
       pos-module-source-expression
       struct-name-remappings)))
  (values code (unbox remappings)))

;; andmap/count : (X Y int -> Z) (listof X) (listof Y) -> (listof Z)
(define-for-syntax (andmap/count f l1)
  (let loop ([l1 l1]
             [i 0])
    (cond
      [(null? l1) #t]
      [else (and (f (car l1) i)
                 (loop (cdr l1)
                       (+ i 1)))])))

;; get-field-names/no-field-info :: symbol?
;;                                  string?
;;                                  (listof identifier?)
;;                                  (or/c identifier? boolean?)
;;                                  syntax?
;;                                  syntax?
;;                                  ->
;;                                  (listof symbol?)
;; attempts to extract field names from accessors
(define-for-syntax (get-field-names/no-field-info who
                                                  struct-name
                                                  accessors
                                                  super-info
                                                  orig-struct-name-stx
                                                  struct-name-remappings
                                                  stx)
  (define own-accessors
    (cond
      [(boolean? super-info) accessors]
      [else
       (define parent-accessors
         (list-ref (extract-struct-info (a:lookup-struct-info super-info struct-name-remappings stx)) 3))
       (drop-right accessors (length parent-accessors))]))
  (for/list ([accessor (in-list own-accessors)])
    (define accessor-str (symbol->string (syntax-e accessor)))
    (unless (string-prefix? accessor-str (string-append struct-name "-"))
      (raise-syntax-error
       who
       (format "unexpected accessor name ~a should start with ~a-"
               accessor-str struct-name)
       stx
       orig-struct-name-stx))
    (string->symbol (substring accessor-str (add1 (string-length struct-name))))))

;; extract-field+struct-names : struct-info? syntax? syntax? -> (listof (cons/c symbol? symbol?))
;; returns a list of pair of field name and the struct name the field belongs to
(define-for-syntax (extract-field+struct-names who the-struct-info orig-struct-name-stx struct-name-remappings stx)
  (define struct-info-list (extract-struct-info the-struct-info))
  (define predicate (list-ref struct-info-list 2))
  (define accessors (list-ref struct-info-list 3))
  (define super-info (list-ref struct-info-list 5))
  (define struct-name (predicate->struct-name who stx predicate))
  (define immediate-field-names
    (if (struct-field-info? the-struct-info)
        (struct-field-info-list the-struct-info)
        (get-field-names/no-field-info who
                                       struct-name
                                       accessors
                                       super-info
                                       orig-struct-name-stx
                                       struct-name-remappings
                                       stx)))
  (define immediate-field+struct-names
    (for/list ([fld (in-list immediate-field-names)])
      (cons fld (string->symbol struct-name))))
  (cond
    [(boolean? super-info) immediate-field+struct-names]
    [else (append immediate-field+struct-names
                  (extract-field+struct-names
                   who
                   (a:lookup-struct-info super-info struct-name-remappings stx)
                   orig-struct-name-stx
                   struct-name-remappings
                   stx))]))


;; build-constructor-contract : syntax (listof syntax) syntax -> syntax
(define-for-syntax (build-constructor-contract stx field-contract-ids predicate-id)
  (with-syntax ([(field-contract-ids ...) field-contract-ids]
                [predicate-id predicate-id])
    (syntax/loc stx
      (-> field-contract-ids ...
          predicate-id))))

;; build-selector-contract : syntax syntax -> syntax
;; constructs the contract for a selector
(define-for-syntax (build-selector-contract struct-name predicate-id field-contract-id)
  (with-syntax ([field-contract-id field-contract-id]
                [predicate-id predicate-id])
    (syntax (-> predicate-id field-contract-id))))

;; build-mutator-contract : syntax syntax -> syntax
;; constructs the contract for a selector
(define-for-syntax (build-mutator-contract struct-name predicate-id field-contract-id)
  (with-syntax ([field-contract-id field-contract-id]
                [predicate-id predicate-id])
    (syntax (-> predicate-id
                field-contract-id
                void?))))

(define-for-syntax (lookup-struct-identifier id struct-name-remappings)
  (syntax-local-value
   (free-identifier-mapping-get struct-name-remappings id (λ () id))
   (λ () #f)))

(define-for-syntax (add-struct-clause-to-struct-id-mapping
                    who mangled-id-scope struct-id-mapping struct-name-remappings
                    a flds)
  (define compile-time-info (lookup-struct-identifier a struct-name-remappings))
  (when (and (identifier? a)
             (struct-info? compile-time-info))
    (define parent
      (let ([parent (list-ref (extract-struct-info compile-time-info) 5)])
        (if (boolean? parent) #f parent)))
    (define parent-struct-info
      (and (identifier? parent)
           (lookup-struct-identifier parent struct-name-remappings)))
    (when (and (or (not parent)
                   (struct-info? parent-struct-info))
               flds
               (andmap identifier? flds))
      (free-identifier-mapping-put!
       struct-id-mapping
       a
       (mangled-id-scope
        (a:mangle-id (format "~a/contract-struct-expansion-info-id" who)
                     a)))
      (define parent-selectors
        (if parent
            (let ([parent-selectors (list-ref (extract-struct-info parent-struct-info) 3)])
              (length parent-selectors))
            0))
      ;; this test will fail when the syntax is bad; we catch syntax errors elsewhere
      (when (< parent-selectors (length flds))
        (for ([f (in-list (list-tail flds parent-selectors))])
          (define selector-id (datum->syntax
                               a
                               (string->symbol (format "~a-~a" (syntax-e a) (syntax-e f)))))
          (free-identifier-mapping-put!
           struct-id-mapping
           selector-id
           (id-for-one-id mangled-id-scope who selector-id #f)))))))

(define-for-syntax (build-struct-code stx who
                                      struct-id-mapping mangled-id-scope
                                      struct-name-position field-names field-contracts
                                      omit-constructor?
                                      provide?
                                      struct-name-remappings)
  (syntax-case struct-name-position ()
    [a
     (identifier? #'a)
     (add-struct-clause-to-struct-id-mapping who mangled-id-scope struct-id-mapping struct-name-remappings #'a field-names)]
    [(a b)
     (identifier? #'a)
     (add-struct-clause-to-struct-id-mapping who mangled-id-scope struct-id-mapping struct-name-remappings #'a field-names)]
    [_ (void)])
  (let* ([struct-name (syntax-case struct-name-position ()
                        [(a b) (syntax a)]
                        [else struct-name-position])]
         [the-struct-info (a:lookup-struct-info struct-name-position struct-name-remappings stx)]
         [true-field-names (and (struct-field-info? the-struct-info)
                                (struct-field-info-list the-struct-info))]
         [orig-struct-name
          (or (undo-contract-out-redirect the-struct-info)
              struct-name)]
         [the-struct-info-list (extract-struct-info the-struct-info)]
         [orig-struct-info-list (extract-struct-info (lookup-struct-identifier orig-struct-name struct-name-remappings))]
         [constructor-id (list-ref the-struct-info-list 1)]
         [predicate-id (list-ref the-struct-info-list 2)]
         [orig-predicate-id (list-ref orig-struct-info-list 2)]
         [selector-ids (reverse (list-ref the-struct-info-list 3))]
         [_ (when (and (not (null? selector-ids))
                       (not (last selector-ids)))
              (raise-syntax-error
               who
               (format "cannot determine the number of fields in struct")
               stx
               struct-name))]
         [orig-selector-ids (reverse (list-ref orig-struct-info-list 3))]
         [super-id (list-ref the-struct-info-list 5)]
         [parent-struct-count (cond
                                [(boolean? super-id) #f]
                                [else (length
                                       (list-ref
                                        (extract-struct-info
                                         (a:lookup-struct-info
                                          super-id
                                          struct-name-remappings
                                          stx))
                                        3))])]
         [chaperone-constructor-id
          (and constructor-id (car (generate-temporaries (list constructor-id))))]
         [is-id-ok?
          (λ (id i)
            (if (or (not parent-struct-count)
                    (parent-struct-count . <= . i))
                id
                #t))]
         [mutator-ids (reverse (list-ref the-struct-info-list 4))] ;; (listof (union #f identifier))
         [orig-mutator-ids (reverse (list-ref orig-struct-info-list 4))]

         [struct:struct-name
          (or (list-ref the-struct-info-list 0)
              (datum->syntax
               struct-name
               (string->symbol
                (string-append
                 "struct:"
                 (symbol->string (syntax-e struct-name))))))]

         [-struct:struct-name
          (datum->syntax
           struct-name
           (string->symbol
            (string-append
             "-struct:"
             (symbol->string (syntax-e struct-name)))))]

         [is-new-id?
          (λ (index)
            (or (not parent-struct-count)
                (parent-struct-count . <= . index)))])

    (let ([unknown-info
           (λ (what names)
             (raise-syntax-error
              who
              (format "cannot determine ~a, found ~s" what names)
              stx
              struct-name))])

      (unless (or (null? selector-ids)
                  (identifier? (last selector-ids)))
        (unknown-info "the selectors" (map syntax->datum selector-ids)))

      (unless constructor-id (unknown-info "constructor" constructor-id))
      (unless predicate-id (unknown-info "predicate" predicate-id))
      (unless (andmap/count is-id-ok? selector-ids)
        (unknown-info "selectors"
                      (map (λ (x) (if (syntax? x)
                                      (syntax->datum x)
                                      x))
                           selector-ids))))

    (unless (equal? (length selector-ids)
                    (length field-names))
      (raise-syntax-error who
                          (format "found ~a field~a in struct, but ~a contract~a"
                                  (length selector-ids)
                                  (if (= 1 (length selector-ids)) "" "s")
                                  (length field-names)
                                  (if (= 1 (length field-names)) "" "s"))
                          stx
                          struct-name))

    ;; make sure the field names are right.
    (define all-field+struct-names
      (extract-field+struct-names who the-struct-info struct-name struct-name-remappings stx))
    (for ([field+struct-name (in-list all-field+struct-names)]
          [field-name (in-list (reverse field-names))])
      (define field-name-should-be (car field+struct-name))
      (define field-name-is (syntax-e field-name))
      (unless (equal? field-name-should-be field-name-is)
        (raise-syntax-error who
                            (format "expected field name to be ~a, but found ~a"
                                    field-name-should-be
                                    field-name-is)
                            stx
                            field-name)))

    (define (make-identifier sym)
      (datum->syntax #f sym))

    (define field-contract-ids
      (for/list ([field+struct-name (in-list all-field+struct-names)])
        (mangled-id-scope
         (a:mangle-id "in-out/contract-field-contract"
                      (make-identifier (car field+struct-name))
                      (make-identifier (cdr field+struct-name))
                      (make-identifier 'for)
                      struct-name))))

    (with-syntax ([((selector-codes selector-new-names) ...)
                   (for/list ([selector-id (in-list selector-ids)]
                              [orig-selector-id (in-list orig-selector-ids)]
                              [field-contract-id (in-list field-contract-ids)]
                              [index (in-naturals)]
                              #:when (is-new-id? index))
                     (code-for-one-id/new-name
                      who stx
                      (if provide?
                          selector-id
                          (datum->syntax struct-name (syntax-e selector-id) struct-name struct-name))
                      #f
                      (build-selector-contract struct-name
                                               predicate-id
                                               field-contract-id)
                      (datum->syntax stx orig-selector-id)
                      mangled-id-scope provide?))]
                  [(rev-selector-old-names ...)
                   (reverse
                    (for/list ([selector-id (in-list selector-ids)]
                               [index (in-naturals)]
                               #:unless (is-new-id? index))
                      (let ([in-map (free-identifier-mapping-get struct-id-mapping
                                                                 selector-id
                                                                 (λ () #f))])
                        (or in-map
                            selector-id))))]
                  [(mutator-codes/mutator-new-names ...)
                   (for/list ([mutator-id (in-list mutator-ids)]
                              [orig-mutator-id (in-list orig-mutator-ids)]
                              [field-contract-id (in-list field-contract-ids)]
                              [index (in-naturals)])
                     (if (and mutator-id (is-new-id? index))
                         (code-for-one-id/new-name
                          who stx
                          (if provide?
                              mutator-id
                              (datum->syntax struct-name (syntax-e mutator-id) struct-name struct-name))
                          #f
                          (build-mutator-contract struct-name
                                                  predicate-id
                                                  field-contract-id)
                          (datum->syntax stx orig-mutator-id)
                          mangled-id-scope provide?)
                         #f))]
                  [(predicate-code predicate-new-name)
                   (code-for-one-id/new-name who stx
                                             (if provide?
                                                 predicate-id
                                                 (datum->syntax struct-name (syntax-e predicate-id) struct-name struct-name))
                                             #f (syntax predicate/c)
                                             (datum->syntax stx orig-predicate-id)
                                             mangled-id-scope provide?)]

                  [(field-contract-id-definitions ...)
                   (map (λ (field-contract-id field-contract)
                          #`(define #,field-contract-id
                              (coerce-contract '#,who #,field-contract)
                              #;
                              (opt/c field-contract #:error-name #,who)))
                        field-contract-ids
                        field-contracts)]
                  [(field-contract-ids ...) field-contract-ids])

      (with-syntax ([((mutator-codes _) ...)
                     (filter syntax-e (syntax->list #'(mutator-codes/mutator-new-names ...)))])
        (with-syntax ([(rev-selector-new-names ...)
                       (reverse (syntax->list (syntax (selector-new-names ...))))])
          (with-syntax ([struct:struct-name struct:struct-name]
                        [-struct:struct-name -struct:struct-name]
                        [struct-name struct-name]
                        [(constructor-args ...) (generate-temporaries selector-ids)]
                        [struct-name-srcloc `'(,(and (path-string? (syntax-source struct-name))
                                                     (path->relative-string/library
                                                      (syntax-source struct-name)))
                                               ,(syntax-line struct-name)
                                               ,(syntax-column struct-name)
                                               ,(syntax-position struct-name)
                                               ,(syntax-span struct-name))])


            (define contract-in-struct:struct-name
              (datum->syntax #'struct-name
                             (syntax-e #'struct:struct-name)
                             #'struct-name
                             #'struct-name))

            ;; the constructor does a bunch more than just get a contract added onto it.
            ;; It also needs to have reflective information put on it that points to the
            ;; contracted versions of all of the struct functions. So, we start from
            ;; `struct-name`, which comes from the input syntax (eg `(struct struct-name ...)`).
            ;; We get `constructor-id` which comes from
            ;;   (list-ref (extract-struct-info (syntax-local-value struct-name)) 1)
            ;; add use these generated names:
            ;;   - local-constructor-id
            ;;   - chaperone-constructor-id
            ;;   - contract-constructor-id
            ;;
            ;; the numbered steps below are written using this schema
            ;;   "id we have bound already"
            ;;    -> "action to take"
            ;;    -> "resulting id that gets bound based on that action"
            ;;
            ;; on require:
            ;; 1. `struct-name` -> import as -> local-constructor-id
            ;; 2. local-constructor-id -> wrap constructor to `chaperone-struct` -> chaperone-constructor-id
            ;; 3. chaperone-constructor-id -> add contract checking -> contract-constructor-id
            ;; 4. contract-constructor-id -> add compile time information -> `struct-name`
            ;; ... if `make-s` is requested, bind `constructor-id` to the value of `chaperone-constructor-id`
            ;;
            ;; on provide:
            ;; 1. constructor-id -> wrap constructor to `chaperone-struct` -> chaperone-constructor-id
            ;; 2. chaperone-constructor-id -> add contract-checking -> contract-constructor-id
            ;; 3. contract-constructor-id -> add compile time information -> local-constructor-id
            ;; 4. local-constructor-id -> export as -> `struct-name`
            ;; ... if `make-s` is requested, export `contract-constructor-id` as `constructor-id`

            (define struct+constructor-code
              (with-syntax ([local-constructor-id (free-identifier-mapping-get struct-id-mapping #'struct-name)]
                            [(rev-mutator-id-info ...)
                             (reverse
                              (for/list ([x (in-list
                                             (syntax->list
                                              #'(mutator-codes/mutator-new-names ...)))])
                                (syntax-case x ()
                                  [(a b) #'(quote-syntax b)]
                                  [else #f])))]
                            [(exported-selector-ids ...) (reverse selector-ids)]
                            [(contract-adding-code contract-constructor-id)
                             (code-for-one-id/new-name
                              who stx
                              chaperone-constructor-id #'struct-name
                              (build-constructor-contract stx #'(field-contract-ids ...) predicate-id)
                              constructor-id
                              mangled-id-scope
                              ;; we subvert the usual abstraction between contract-in/contract-out
                              ;; by passing #t here, as we're managing those names ourselves, due
                              ;; to the fact that we have multiple stages of id in play for the constructor
                              #t    ;; provide?
                              #t    ;; mangle-for-maker?
                              #f    ;; generate-provides?
                              #:lift? #f
                              #:start-swapped? (not provide?)
                              )])

                (define generate-make-s-binding?
                  (if provide?
                      (not (free-identifier=? constructor-id #'struct-name))
                      ;; the free identifier check doesn't work for
                      ;; `contract-in` because the mention of the
                      ;; struct name is in the requiring module but the
                      ;; constructor inside the struct info is from the
                      ;; providing module. So we use this check instead
                      (not (equal? (syntax-e constructor-id)
                                   (syntax-e #'struct-name)))))
                (define mk (if omit-constructor?
                               (if true-field-names
                                   #'make-contract-out-redirect/field-struct-info
                                   #'make-contract-out-redirect-struct-info)
                               (if true-field-names
                                   #'make-applicable-contract-out-redirect/field-struct-info
                                   #'make-applicable-contract-out-redirect-struct-info)))
                (define proc #`(λ () (list (quote-syntax -struct:struct-name)
                                           (quote-syntax #,(if provide?
                                                               (if generate-make-s-binding?
                                                                   #'contract-constructor-id
                                                                   #'local-constructor-id)
                                                               (if generate-make-s-binding?
                                                                   (datum->syntax #'struct-name
                                                                                  (syntax-e constructor-id)
                                                                                  #'struct-name
                                                                                  #'struct-name)
                                                                   #'contract-constructor-id)))
                                           (quote-syntax predicate-new-name)
                                           (list (quote-syntax rev-selector-new-names) ...
                                                 (quote-syntax rev-selector-old-names) ...)
                                           (list rev-mutator-id-info ...)
                                           #,(if (boolean? super-id)
                                                 super-id
                                                 #`(quote-syntax
                                                    #,(free-identifier-mapping-get
                                                       struct-id-mapping
                                                       super-id
                                                       (λ () super-id)))))))
                (add-remapping #'local-constructor-id #'struct-name)
                #`(begin
                    (define #,chaperone-constructor-id
                      (let ([struct-name
                             (λ (constructor-args ...)
                               (chaperone-struct (#,constructor-id constructor-args ...)
                                                 struct:struct-name
                                                 struct-info
                                                 (λ (struct-type skipped?)
                                                   (values #,(if provide?
                                                                 #'-struct:struct-name
                                                                 contract-in-struct:struct-name)
                                                           skipped?))))])
                        struct-name))
                    contract-adding-code
                    #,@(cond
                         [(and generate-make-s-binding?
                               (not omit-constructor?)
                               provide?)
                          (add-remapping #'contract-constructor-id constructor-id)
                          (make-unprotected-submodule-code
                           (lambda ()
                             #`(provide #,constructor-id)))]
                         [else '()])
                    #,@(make-unprotected-submodule-code
                        (lambda ()
                          #'(provide struct-name)))
                    (define-syntax #,(if provide?
                                         #'local-constructor-id
                                         #'struct-name)
                      (#,mk
                       #,proc
                       (lambda () (quote-syntax #,orig-struct-name))
                       #,@(if omit-constructor?
                              (list)
                              (list #`(lambda () (quote-syntax contract-constructor-id))))
                       #,@(if true-field-names
                              #`('#,true-field-names)
                              (list))))
                    #,@(if (and generate-make-s-binding?
                                (not omit-constructor?)
                                (not provide?))
                           (list #`(define #,(datum->syntax #'struct-name
                                                            (syntax-e constructor-id)
                                                            #'struct-name
                                                            #'struct-name)
                                     #,chaperone-constructor-id))
                           (list)))))
            (add-remapping #'-struct:struct-name #'struct:struct-name)
            (quasisyntax/loc stx
              (begin
                field-contract-id-definitions ...
                #,struct+constructor-code
                predicate-code
                selector-codes ...
                mutator-codes ...

                ;; expanding out the body of the `make-pc-struct-type' function
                ;; directly here in the expansion makes this very expensive at compile time
                ;; when there are a lot of in-out/contract clause using structs
                (define #,(if provide?
                              #'-struct:struct-name
                              contract-in-struct:struct-name)
                  (make-pc-struct-type #,pos-module-source-id
                                       'struct-name
                                       struct-name-srcloc
                                       struct:struct-name
                                       '(#,@field-names)
                                       field-contract-ids ...))
                #,@(make-unprotected-submodule-code
                    (lambda ()
                      #'(provide struct:struct-name)))))))))))

;; just-check-errors? : (or/c boolean? 'contract-in)
;;   when `just-check-errors?` is a true value, we'll check and signal syntax errors
;;   and won't generate code. If `just-check-errors?` is 'contract-in, then we'll
;;   also rename the structs to faciliate importing them on the first go
(define-for-syntax (generate-in/out-code/remappings
                    who stx p/c-clauses unprotected-submodule-name
                    just-check-errors?
                    provide?
                    pos-module-source-expression
                    struct-name-remappings)
  (define mangled-id-scope (make-syntax-introducer))

  ;; ids : table[id -o> (listof id)]
  ;; code-for-each-clause adds identifiers to this map.
  ;; when it binds things; they are then used to signal
  ;; a syntax error for duplicates
  (define dups-table (make-hash))
  (define (add-to-dups-table id)
    (hash-update!
     dups-table
     (syntax-e id)
     (λ (ids) (cons id ids))
     '()))
  (define (signal-dup-syntax-error)
    (hash-for-each
     dups-table
     (λ (k ids)
       (let loop ([ids ids])
         (cond
           [(null? ids) (void)]
           [else
            (cond
              [(ormap (λ (x) (bound-identifier=? (car ids) x)) (cdr ids))
               (let ([dups (filter (λ (x) (bound-identifier=? (car ids) x))
                                   ids)])
                 (raise-syntax-error who
                                     "duplicate identifiers"
                                     stx
                                     (car dups)
                                     (cdr dups)))]
              [else
               (loop (cdr ids))])])))))

  ;; code-for-each-clause : (listof syntax) -> (listof syntax)
  ;; constructs code for each clause of a contract-in/contract-out
  (define (code-for-each-clause clauses)
    (define struct-id-mapping (make-free-identifier-mapping))
    (let loop ([clauses clauses]
               [exists-binders '()])
      (cond
        [(null? clauses) null]
        [else
         (let ([clause (car clauses)])
           ;; compare raw identifiers for `struct` and `rename` just like `provide` does
           (syntax-case* clause (struct rename) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
             [exists
              (or (eq? '#:exists (syntax-e #'exists)) (eq? '#:∃ (syntax-e #'exists))
                  (eq? '#:forall (syntax-e #'exists)) (eq? '#:∀ (syntax-e #'exists)))
              (cond
                [(null? (cdr clauses))
                 (raise-syntax-error
                  who
                  (format (string-append
                           "expected either a single variable or a sequence of variables"
                           " to follow ~a, but found nothing")
                          (syntax-e #'exists))
                  stx
                  clause)]
                [else
                 (syntax-case (cadr clauses) ()
                   [x
                    (identifier? #'x)
                    (if just-check-errors?
                        (loop (cddr clauses) exists-binders)
                        (with-syntax ([(x-gen) (generate-temporaries #'(x))])
                          (cons (code-for-one-poly-id #'x #'x-gen #'exists)
                                (loop (cddr clauses)
                                      (add-a-binder #'x #'x-gen exists-binders)))))]
                   [(x ...)
                    (andmap identifier? (syntax->list #'(x ...)))
                    (if just-check-errors?
                        (loop (cddr clauses) exists-binders)
                        (with-syntax ([(x-gen ...) (generate-temporaries #'(x ...))])
                          (append (map (λ (x x-gen) (code-for-one-poly-id x x-gen #'exists))
                                       (syntax->list #'(x ...))
                                       (syntax->list #'(x-gen ...)))
                                  (loop (cddr clauses)
                                        (let loop ([binders exists-binders]
                                                   [xs (syntax->list #'(x ...))]
                                                   [x-gens (syntax->list #'(x-gen ...))])
                                          (cond
                                            [(null? xs) binders]
                                            [else
                                             (loop (add-a-binder (car xs) (car x-gens) binders)
                                                   (cdr xs)
                                                   (cdr x-gens))]))))))]
                   [else
                    (raise-syntax-error
                     who
                     (format (string-append "expected either a single variable or a sequence"
                                            " of variables to follow ~a")
                             (syntax-e #'exists))
                     stx
                     (cadr clauses))])])]
             [(rename this-name new-name contract)
              (and (identifier? (syntax this-name))
                   (identifier? (syntax new-name)))
              (begin
                (add-to-dups-table #'new-name)
                (if just-check-errors?
                    (loop (cdr clauses) exists-binders)
                    (cons (code-for-one-id who stx
                                           (if provide? #'this-name #'new-name)
                                           #f
                                           (add-exists-binders (syntax contract) exists-binders)
                                           (if provide? #'new-name #'this-name)
                                           mangled-id-scope provide?)
                          (loop (cdr clauses) exists-binders))))]
             [(rename this-name new-name contract)
              (identifier? (syntax this-name))
              (raise-syntax-error who
                                  "malformed rename clause, expected an identifier"
                                  stx
                                  (syntax new-name))]
             [(rename this-name new-name contract)
              (identifier? (syntax new-name))
              (raise-syntax-error who
                                  "malformed rename clause, expected an identifier"
                                  stx
                                  (syntax this-name))]
             [(rename . _)
              (raise-syntax-error who "malformed rename clause" stx clause)]
             [(struct struct-name ((field-name contract) ...) options ...)
              (and (well-formed-struct-name? (syntax struct-name))
                   (andmap identifier? (syntax->list (syntax (field-name ...)))))
              (let ()
                (for ([option (in-list (syntax->list #'(options ...)))])
                  (unless (member (syntax-e option) '(#:omit-constructor))
                    (raise-syntax-error who
                                        "malformed struct option"
                                        stx
                                        option)))
                (unless (<= (length (syntax->list #'(options ...))) 1)
                  (raise-syntax-error who
                                      "malformed struct option"
                                      stx))
                (define omit-constructor?
                  (member '#:omit-constructor (map syntax-e (syntax->list #'(options ...)))))
                (when (equal? just-check-errors? 'contract-in)
                  (define the-struct-name
                    (syntax-case #'struct-name ()
                      [(a b) #'a]
                      [a #'a]))
                  (add-remapping (car (generate-temporaries (list the-struct-name)))
                                 the-struct-name))
                (if just-check-errors?
                    (loop (cdr clauses) exists-binders)
                    (let ([sc (build-struct-code stx who
                                                 struct-id-mapping mangled-id-scope
                                                 (syntax struct-name)
                                                 (syntax->list (syntax (field-name ...)))
                                                 (map (λ (x) (add-exists-binders x exists-binders))
                                                      (syntax->list (syntax (contract ...))))
                                                 omit-constructor?
                                                 provide?
                                                 struct-name-remappings)])
                      (cons sc (loop (cdr clauses) exists-binders)))))]
             [(struct name)
              (identifier? (syntax name))
              (raise-syntax-error who
                                  "missing fields"
                                  stx
                                  clause)]
             [(struct name . rest)
              (not (well-formed-struct-name? (syntax name)))
              (raise-syntax-error
               who
               "name must be an identifier or two identifiers with parens around them"
               stx
               (syntax name))]
             [(struct name (fields ...) options ...)
              (let ()
                (for ([field [in-list (syntax->list (syntax (fields ...)))]])
                  (syntax-case field ()
                    [(x y)
                     (identifier? (syntax x))
                     (void)]
                    [(x y)
                     (raise-syntax-error who
                                         "malformed struct field, expected identifier"
                                         stx
                                         (syntax x))]
                    [else
                     (raise-syntax-error who
                                         "malformed struct field"
                                         stx
                                         field)]))
                ;; if we didn't find a bad field something is wrong!
                (raise-syntax-error who "internal error.1" stx clause))]
             [(struct name . fields)
              (raise-syntax-error who
                                  "malformed struct fields"
                                  stx
                                  clause)]
             [(name contract)
              (identifier? (syntax name))
              (begin
                (add-to-dups-table #'name)
                (if just-check-errors?
                    (loop (cdr clauses) exists-binders)
                    (cons (code-for-one-id who stx
                                           (syntax name) #f
                                           (add-exists-binders (syntax contract)
                                                               exists-binders)
                                           #f
                                           mangled-id-scope provide?)
                          (loop (cdr clauses) exists-binders))))]
             [(name contract)
              (raise-syntax-error who
                                  "expected identifier"
                                  stx
                                  (syntax name))]
             [unk
              (raise-syntax-error who
                                  "malformed clause"
                                  stx
                                  (syntax unk))]))])))

  ;; well-formed-struct-name? : syntax -> bool
  (define (well-formed-struct-name? stx)
    (or (identifier? stx)
        (syntax-case stx ()
          [(name super)
           (and (identifier? (syntax name))
                (identifier? (syntax super)))
           #t]
          [_ #f])))

  ;; code-for-one-poly-id : syntax -> syntax
  (define (code-for-one-poly-id x x-gen poly)
    (if (or (eq? '#:exists (syntax-e poly)) (eq? '#:∃ (syntax-e poly)))
        #`(define #,x-gen (new-∃/c '#,x))
        #`(define #,x-gen (new-∀/c '#,x))))

  (define (add-exists-binders stx exists-binders)
    (if (null? exists-binders)
        stx
        #`(let #,exists-binders #,stx)))

  (define (add-a-binder id id-gen binders)
    (cons #`[#,id #,id-gen] binders))

  (parameterize ([current-unprotected-submodule-name unprotected-submodule-name])
    (cond
      [just-check-errors?
       (begin0
         (code-for-each-clause p/c-clauses)
         (signal-dup-syntax-error))]
      [else
       (with-syntax ([(bodies ...) (code-for-each-clause p/c-clauses)]
                     [pos-module-source-id pos-module-source-id])
         #`(begin
             (define pos-module-source-id #,pos-module-source-expression)
             bodies ...))])))

(define (make-pc-struct-type pos-module-source struct-name srcloc struct-type field-names . ctcs)
  (define blame
    (make-blame (build-source-location srcloc) struct-type (λ () `(substruct-of ,struct-name))
                pos-module-source #f #t))
  (define late-neg-acceptors
    (for/list ([ctc (in-list ctcs)]
               [field-name (in-list field-names)])
      ((get/build-late-neg-projection ctc)
       (blame-add-context blame
                          (format "the ~a field of" field-name)
                          #:swap? #t))))
  (chaperone-struct-type
   struct-type
   (λ (a b c d e f g h) (values a b c d e f g h))
   (λ (x) x)
   (λ args
     (define name #f)
     (define vals
       (let loop ([args args])
         (cond
           [(null? args) null]
           [(null? (cdr args))
            (set! name (car args))
            null]
           [else (cons (car args) (loop (cdr args)))])))
     (apply values
            (map (λ (late-neg-acceptors val)
                   (late-neg-acceptors val 'not-enough-info-for-blame))
                 late-neg-acceptors
                 vals)))))
