(module check scheme/base

  (require "ast.ss"
           "types.ss"
           "parameters.ss"
           "error-messaging.ss"
           "restrictions.ss"
           "profj-pref.ss"
           "build-info.ss"
           scheme/class scheme/path
           (prefix-in srfi: srfi/1) mzlib/string)
  (provide check-defs check-interactions-types)
  
  ;symbol-remove-last: symbol->symbol
  (define (symbol-remove-last s)
    (let ((str (symbol->string s)))
      (string->symbol (substring str 0 (sub1 (string-length str))))))

  (define update-class-with-inner (make-parameter (lambda (x) (void))))
  (define current-method (make-parameter ""))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Environment functions

  ;env =>
  ;(make-environment (list var-type) (list string) (list type) (list string) (list inner-rec))
  (define-struct environment (types set-vars exns labels local-inners) #:transparent)
  
  ;Constant empty environment
  (define empty-env (make-environment null null null null null))

  ;; var-type => (make-var-type string type properties)
  (define-struct var-type (var type properties) #:transparent)
  
  ;;inner-rec ==> (make-inner-rec string (U symbol void) (list string) class-rec)
  (define-struct inner-rec (name unique-name package record))
  
  ;;Environment variable properties
  ;;(make-properties bool bool bool bool bool bool)
  (define-struct properties (parm? field? static? settable? final? usable? set?) #:transparent #:mutable)
  (define parm (make-properties #t #f #f #t #f #t #t))
  (define final-parm (make-properties #t #f #f #f #t #t #t))
  (define method-var (make-properties #f #f #f #t #f #t #f))
  (define final-method-var (make-properties #f #f #f #f #t #t #f))
  (define (obj-field set?) (make-properties #f #t #f #t #f #t set?))
  (define (final-field settable) (make-properties #f #t #f settable #t #t #f))
  (define (class-field set?) (make-properties #f #t #t #f #t #t set?))
  (define (final-class-field settable) (make-properties #f #t #t settable #t #t #f))
  (define inherited-conflict (make-properties #f #t #f #f #f #f #f))
  
  ;; add-var-to-env: string type properties env -> env
  (define (add-var-to-env name type properties env)
    (make-environment (cons (make-var-type name type properties) (environment-types env))
                      (environment-set-vars env)
                      (environment-exns env)
                      (environment-labels env)
                      (environment-local-inners env)))
  
  ;; lookup-var-in-env: string env -> (U var-type boolean)
  (define (lookup-var-in-env name env)
    (letrec ((lookup
              (lambda (env)
                (and (not (null? env))
                     (if (string=? name (var-type-var (car env)))
                         (car env)
                         (lookup (cdr env)))))))
      (lookup (environment-types env))))
  
  (define (lookup-field-in-env name env)
    (letrec ([lookup
              (lambda (env)
                (and (not (null? env))
                     (if (and (string=? name (var-type-var (car env)))
                              (properties-field? (var-type-properties (car env))))
                         (car env)
                         (lookup (cdr env)))))])
      (lookup (environment-types env))))

  ;lookup-specific-this: name env symbol type-records -> bool
  (define (lookup-enclosing-this name env level type-recs)
    (letrec ((type (name->type name #f (name-src name) level type-recs))
             (lookup
              (lambda (env)
                (and (not (null? env))
                     (if (and (or (string=? "this" (var-type-var (car env)))
                                  (regexp-match "encl-this-" (var-type-var (car env))))
                              (type=? type (var-type-type (car env))))
                         (car env)
                         (lookup (cdr env)))))))
      (lookup (environment-types env))))
                               
  ;remove-var-from-env string env -> env
  (define (remove-var-from-env name env)
    (letrec ((remove-from-env
              (lambda (env)
                (cond
                  ((null? env) null)
                  ((equal? name (var-type-var (car env)))
                   (remove-from-env (cdr env)))
                  (else (cons (car env) (remove-from-env (cdr env))))))))
      (make-environment (remove-from-env (environment-types env))
                        (environment-set-vars env)
                        (environment-exns env)
                        (environment-labels env)
                        (environment-local-inners env))))
  
  ;;lookup-containing-class-depth: string env -> num
  (define (lookup-containing-class-depth name env)
    (letrec ((lookup
              (lambda (env)
                (and (not (null? env))
                     (cond
		      [(string=? name (var-type-var (car env))) 0]
		      [(regexp-match "encl-this-" (var-type-var (car env)))
		       (add1 (lookup (cdr env)))]
		      [else (lookup (cdr env))])))))
      (lookup (environment-types env))))
  
  ;update-env-for-inner: env -> env
  (define (update-env-for-inner env)
    (letrec ((str "encl-this-")
             (update-env
              (lambda (env)
                (cond
                  ((null? env) null)
                  ((equal? (var-type-var (car env)) "this") 
                   (update-env (cdr env)))
                  ((regexp-match str (var-type-var (car env)))
                   (let ((var (car env)))
                     (cons (make-var-type (format "encl-this-~a" 
                                                  (add1 (string->number (regexp-replace str (var-type-var var) ""))))
                                          (var-type-type var)
                                          (var-type-properties var))
                           (update-env (cdr env)))))
                  ((properties-final? (var-type-properties (car env)))
                   (cons (car env) (update-env (cdr env))))
                  (else (update-env (cdr env)))))))
      (make-environment (update-env (environment-types env))
                        (environment-set-vars env)
                        (environment-exns env)
                        (environment-labels env)
                        (environment-local-inners env))))

  ;;add-set-to-env: string env -> env
  (define (add-set-to-env name env)
    (make-environment (environment-types env)
                      (if (not (member name (environment-set-vars env)))
                          (cons name (environment-set-vars env))
                          (environment-set-vars env))
                      (environment-exns env)
                      (environment-labels env)
                      (environment-local-inners env)))
  
  ;;var-set?: string env -> bool
  (define (var-set? name env)
    (member name (environment-set-vars env)))
  
  ;;intersect-var-sets: base-env env env -> env
  ;;Intersects the list of set variables for the two env, creating a new env with the remainder from base
  (define (intersect-var-sets base-env env-1 env-2)
    (make-environment (environment-types base-env)
                      (cond
                        ((null? (environment-set-vars env-1)) (environment-set-vars env-2))
                        ((null? (environment-set-vars env-2)) (environment-set-vars env-1))
                        ((equal? (environment-set-vars env-1) (environment-set-vars env-2))
                         (environment-set-vars env-1))
                        (else
                         (srfi:lset-intersection equal? (environment-set-vars env-1)
                                                 (environment-set-vars env-2))))
                      (environment-exns base-env)
                      (environment-labels base-env)
                      (environment-local-inners base-env)))
  
  (define (remove-set-vars to-remove sets)
    (cond
      ((null? sets) sets)
      ((member (car sets) to-remove) (remove-set-vars to-remove (cdr sets)))
      (else (cons (car sets) (remove-set-vars to-remove (cdr sets))))))
  
  (define (unnest-var base-env context+)
    (let ((adds 
           (map var-type-var
                (srfi:lset-difference equal? (environment-types context+) (environment-types base-env)))))
    (make-environment (environment-types base-env)
                      (remove-set-vars adds (environment-set-vars context+))
                      (environment-exns base-env)
                      (environment-labels base-env)
                      (environment-local-inners base-env))))
  
  ;;add-exn-to-env: type env -> env
  (define (add-exn-to-env exn env)
    (make-environment (environment-types env)
                      (environment-set-vars env)
                      (cons exn (environment-exns env))
                      (environment-labels env)
                      (environment-local-inners env)))
  
  ;;add-exns-to-env: (list type) env -> env
  (define (add-exns-to-env exns env)
    (if (null? exns)
        env
        (add-exns-to-env (cdr exns)
                         (add-exn-to-env (car exns) env))))
  
  ;restore-exn-env: env env -> env
  (define (restore-exn-env old-env new-env)
    (make-environment (environment-types new-env)
                      (environment-set-vars new-env)
                      (environment-exns old-env)
                      (environment-labels new-env)
                      (environment-local-inners new-env)))
  
  ;;lookup-exn: type env type-records symbol-> bool
  (define (lookup-exn type env type-recs level)
    (ormap (lambda (lookup)
             (assignment-conversion lookup type type-recs))
           (environment-exns env)))
  
  ;;add-label-to-env: string env -> env
  (define (add-label-to-env label env)
    (make-environment (environment-types env)
                      (environment-set-vars env)
                      (environment-exns env)
                      (cons label (environment-labels env))
                      (environment-local-inners env)))
  
  ;;lookup-label: string env -> bool
  (define (lookup-label label env)
    (member label (environment-labels env)))

  ;;add-local-inner-to-env: string symbol class-rec env -> env
  (define (add-local-inner-to-env name unique-name package rec env)
    (make-environment (environment-types env)
                      (environment-set-vars env)
                      (environment-exns env)
                      (environment-labels env)
                      (cons (make-inner-rec name unique-name package rec) (environment-local-inners env))))
  
  ;;lookup-local-inner: string env -> (U inner-rec #f)
  (define (lookup-local-inner name env)
    (letrec ((lookup 
              (lambda (l)
                (and (not (null? l))
                    (or (and (equal? name (inner-rec-name (car l)))
                             (car l))
                        (lookup (cdr l)))))))
      (lookup (environment-local-inners env))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Generic helper functions

  ;;(make-type/env type env)
  (define-struct type/env (t e))
  
  ;; set-expr-type: exp (U type type/env) -> (U type type/env)
  (define (set-expr-type exp t)
    (set-expr-types! exp (if (type/env? t) (type/env-t t) t)) t)

  ;lookup-this type-records env -> class-record
  (define (lookup-this type-recs env)
    (let ((this (lookup-var-in-env "this" env)))
      (if this 
          (send type-recs get-class-record (var-type-type this))
          interactions-record)))
  
  ;add-required (list string) string (list string) type-records -> void
  (define (add-required test-class class path type-recs)
    (unless (equal? (car test-class) class)
      (send type-recs add-req (make-req class path))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Checking functions

  ;check-def: ast symbol type-records -> void
  (define (check-defs def level type-recs)
    (when (not (null? (check-list)))
      (check-list (cdr (check-list))))
    (send type-recs set-location! (def-file def))
    (check-location (def-file def))
    (let ((package-name 
           (send type-recs lookup-path 
                 (id-string (def-name def))
                 (lambda () 
                   (error 'check-defs 
                          "Internal error: Current def does not have a record entry")))))
      (cond
        ((interface-def? def)
         (check-interface def package-name (def-level def) type-recs))
        ((class-def? def)
         (check-class def package-name (def-level def) type-recs empty-env))
        ((test-def? def)
         (check-test def package-name (def-level def) type-recs empty-env))))
    (packages (cons def (packages)))
    (when (not (null? (check-list)))
      (check-defs (car (check-list)) level type-recs)))
  
  ;check-interactions-types: ast symbol location type-records -> void
  (define (check-interactions-types prog level loc type-recs)
    (check-location loc)
    (send type-recs set-location! 'interactions)
    (send type-recs set-class-reqs null)
    (let ((env (create-field-env (send type-recs get-interactions-fields) empty-env "scheme-interactions"))
          (c-class (list "scheme-interactions")))
      (cond
        ((pair? prog)
         (for-each (lambda (p)
                     (check-interactions-types p level loc type-recs)) prog))
        ((var-init? prog) 
         (let* ((name (id-string (field-name prog)))
                (check-env (remove-var-from-env name env))
                (type (type-spec-to-type (field-type-spec prog) #f level type-recs)))
           (set-field-type! prog type)
           (check-var-init (var-init-init prog)
                           (lambda (e env) 
                             (check-expr e env level type-recs c-class #f #t #t #f))
                           check-env
                           type
                           (string->symbol name)
                           "local variable"
                           type-recs)))
        ((var-decl? prog) (void))
        ((statement? prog)
         (check-statement prog null env level type-recs c-class #f #t #f #f #t))
        ((expr? prog)
         (check-expr prog env level type-recs c-class #f #t #t #f))
        (else
         (error 'check-interactions "Internal error: check-interactions-types got ~a" prog)))))
  
  ;check-class: class-def (list string) symbol type-records env -> void
  (define (check-class class package-name level type-recs class-env)
    (let* ((old-reqs (send type-recs get-class-reqs))
          (old-update (update-class-with-inner))
          (name (id-string (def-name class)))
          (rec (send type-recs get-class-record (cons name package-name))))
      (update-class-with-inner (lambda (inner)
                                 (let ((name (id-string (def-name inner))))
                                   (set-def-members! class (cons inner (def-members class)))
                                   (set-class-record-inners! rec 
                                                             (cons (make-inner-record (filename-extension name) name 
                                                                                      (map modifier-kind (header-modifiers (def-header inner)))
                                                                                      (class-def? inner)) (class-record-inners rec))))))
      (send type-recs set-class-reqs (def-uses class))
      
      (send type-recs add-req (make-req "String" '("java" "lang")))
      (send type-recs add-req (make-req "Object" '("java" "lang")))
      
      (let ((this-ref (make-ref-type name package-name)))
        (check-members (def-members class)
                       (add-var-to-env "this" this-ref parm class-env)
                       level
                       type-recs 
                       (cons name package-name)
                       #f 
                       (memq 'abstract (map modifier-kind (header-modifiers (def-header class))))
                       (def-kind class)
                       (if (null? (header-extends (def-header class))) #f
                           (name-src (car (header-extends (def-header class)))))
                       ))
      (set-def-uses! class (send type-recs get-class-reqs))
      (update-class-with-inner old-update)
      (send type-recs set-class-reqs old-reqs)))
  
  ;check-test: test-def (list string) symbol type-recs -> void
  (define (check-test test package-name level type-recs env)
    (unless (null? (test-header-tests (def-header test)))
      (for-each (lambda (test-class)
                  (unless (type-exists? (id-string (name-id test-class))
                                        (map id-string (name-path test-class))
                                        #f (name-src test-class)
                                        level type-recs)
                    (tested-not-found (def-name test) test-class (name-src test-class))))
                (test-header-tests (def-header test))))
    (check-class test package-name level type-recs env))

  ;check-interface: interface-def (list string) symbol type-recs -> void
  (define (check-interface iface p-name level type-recs)
    (let ((old-reqs (send type-recs get-class-reqs))
          (old-update (update-class-with-inner))
          (rec (send type-recs get-class-record (cons (id-string (def-name iface)) p-name))))
      (update-class-with-inner (lambda (inner)
                                 (let ((name (id-string (def-name inner))))
                                   (set-def-members! iface (cons inner (def-members iface)))
                                   (set-class-record-inners! rec 
                                                             (cons (make-inner-record (filename-extension name) name 
                                                                                      (map modifier-kind (header-modifiers (def-header inner)))
                                                                                      (class-def? inner)) (class-record-inners rec))))))

      #;(update-class-with-inner (lambda (inner)
                                 (set-def-members! iface (cons inner (def-members iface)))))
      (send type-recs set-class-reqs (def-uses iface))
      
      (send type-recs add-req (make-req "String" '("java" "lang")))
      (send type-recs add-req (make-req "Object" '("java" "lang")))
      
      (check-members (def-members iface) empty-env level type-recs 
                     (cons (id-string (def-name iface)) p-name) #t #f (def-kind iface) #f)
      (set-def-uses! iface (send type-recs get-class-reqs))
      (update-class-with-inner old-update)
      (send type-recs set-class-reqs old-reqs)))
  
  ;check-inner def symbol type-records (list string) env -> (U (list symbol class-record) void)
  (define (check-inner-def def level type-recs c-class env)
    (let* ((statement-inner? (eq? (def-kind def) 'statement))
           (local-inner? (or (eq? (def-kind def) 'anon) statement-inner?))
           (p-name (cdr c-class))
           (inner-env (update-env-for-inner env))
           (this-type (var-type-type (lookup-var-in-env "this" env)))
           (unique-name 
            (when statement-inner? (symbol->string (gensym (string-append (id-string (def-name def)) "-")))))
           (inner-rec
            (when local-inner?
              (add-init-args def env)
              (begin0
                (build-inner-info def unique-name p-name level type-recs (def-file def) #t)
                (when statement-inner?
                  (set-id-string! (header-id (def-header def)) unique-name))
                ((update-class-with-inner) def)))))
      (if (interface-def? def)
          (check-interface def p-name level type-recs)
          (check-class def p-name level type-recs (add-var-to-env "encl-this-1" this-type final-parm inner-env)))
        ;; Propagate uses in internal defn to enclosing defn:
      (for-each (lambda (use)
                  (add-required c-class (req-class use) (req-path use) type-recs))
                (def-uses def))
      (list unique-name p-name inner-rec)))
    
  ;add-init-args: def env -> void
  ;Updates the inner class with the names of the final variables visible within the class
  (define (add-init-args def env)
    (set-def-closure-args! def
                           (map (lambda (type-var)
                                  (make-id (var-type-var type-var) #f))
                                (filter (lambda (type-var)
                                          (or (eq? (var-type-properties type-var) final-parm)
                                              (eq? (var-type-properties type-var) final-method-var)))
                                        (environment-types env)))))
  
  ;tested-not-found: id name src -> void
  (define (tested-not-found test class src)
    (raise-error
     'tests
     (format "test ~a cannot test class ~a, as the class cannot be found."
             (id->ext-name test) (path->ext (name->path class)))
     'tests src))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Member checking methods
  
  ;check-members: (list member) env symbol type-records (list string) bool bool src-> void
  (define (check-members members env level type-recs c-class iface? abst-class? class-kind extend-src)
    (let* ((class-record (lookup-this type-recs env))
           (fields (class-record-fields class-record))
           (field-env (create-field-env fields env (car c-class)))
           (base-fields (create-field-env (filter (lambda (field)
                                                    (not (equal? (field-record-class field) c-class)))
                                                  fields) env (car c-class)))
           (ctor-throw-env (if iface? field-env
                               (consolidate-throws 
                                (get-constructors (class-record-methods class-record)) field-env)))
           (static-env (get-static-fields-env field-env))
           (setting-fields null)
           (inherited-fields null))
      #;(when (eq? level 'beginner)
        (let ((parent (send type-recs get-class-record (car (class-record-parents class-record)))))
          (when (memq 'abstract (class-record-modifiers parent))
            (set! inherited-fields 
                  (filter (lambda (f) (not (field-record-init? f))) (get-field-records parent))))))
      (let loop ((rest members) (statics empty-env) (fields base-fields))
        (unless (null? rest)
          (let ((member (car rest)))
            (cond
              ((method? member)
               (cond
                 [(memq 'static (map modifier-kind (method-modifiers member)))
                  (check-method member static-env level type-recs c-class #t iface?)]
                 [(and (eq? level 'beginner) (eq? 'ctor (type-spec-name (method-type member))))
                  (check-method member fields level type-recs c-class #f iface?)]
                 [else 
                  (check-method member field-env level type-recs c-class #f iface?)])
               (loop (cdr rest) statics fields))
              ((initialize? member)
               (if (initialize-static member)
                   (check-statement (initialize-block member) 'void static-env
                                    level type-recs c-class #f #t #f #f #f)
                   (check-statement (initialize-block member) 'void ctor-throw-env level
                                    type-recs c-class #f #f #f #f #f))
               (loop (cdr rest) statics fields))
              ((field? member)
               (let ((static? (memq 'static (map modifier-kind (field-modifiers member))))
                     (name (id-string (field-name member)))
                     (type (field-type member)))
                 (when (ref-type? type)
                   (add-required c-class (ref-type-class/iface type) (ref-type-path type) type-recs))
                 (if (var-init? member)
                     (check-var-init (var-init-init member)
                                     (lambda (e env)
                                       (check-expr e env
                                                   level type-recs c-class #f
                                                   static? #f #f))
                                     (if static? statics fields)
                                     type
                                     (string->symbol name)
                                     "field"
                                     type-recs)
                     (when (field-needs-set? member level abst-class?)
                       (set! setting-fields (cons member setting-fields))))
                 (if static?
                     (loop (cdr rest) 
                           (add-var-to-env name type (class-field (var-init? member)) statics) 
                           (add-var-to-env name type (class-field (var-init? member)) fields))
                     (loop (cdr rest) statics 
                           (add-var-to-env name type (obj-field (var-init? member)) fields)))))
              ((def? member)
               (check-inner-def member level type-recs c-class field-env)
               (loop (cdr rest) statics fields))
              ))))
      (let ((assigns (get-assigns members level (car c-class)))
            (static-assigns (get-static-assigns members level)))
        #;(when (eq? level 'beginner)
            (for-each (lambda (f)
                        (andmap (lambda (assign)
                                  (inherited-field-set? f assign extend-src))
                                assigns))
                      inherited-fields))
        (for-each (lambda (field)
                    (if (memq 'static (map modifier-kind (field-modifiers field)))
                        (andmap
                         (lambda (assign)
                           (field-set? field assign (car c-class) level #t)) static-assigns)
                        (andmap
                         (lambda (assign)
                           (field-set? field assign (car c-class) level #f)) assigns)))
                  setting-fields))))
      
  ;create-field-env: (list field-record) env string -> env
  (define (create-field-env fields env class)
    (cond
      ((null? fields) env)
      (else
       (let* ((field (car fields))
              (name (field-record-name field))
              (in-env? (lookup-var-in-env name env))
              (static? (memq 'static (field-record-modifiers field)))
              (final? (memq 'final (field-record-modifiers field)))
              (current? (equal? class (car (field-record-class field)))))
         (add-var-to-env name
                         (field-record-type field)
                         (cond
                           ((and in-env? (not current?)) inherited-conflict)
                           ((and (not static?) (not final?)) (obj-field #t))
                           ((and (not static?) final?) (final-field current?))
                           ((and static? (not final?)) (class-field #t))
                           ((and static? final?) (final-class-field current?)))
                         (create-field-env (cdr fields) env class))))))
  
  ;get-constrcutors: (list method-record) -> (list method-record)
  (define (get-constructors methods)
    (filter (lambda (mr)
              (eq? 'ctor (method-record-rtype mr))) methods))

  ;consolidate-throws: (list method-record) env -> env
  (define (consolidate-throws mrs env)
    (let ((first-throws (method-record-throws (car mrs)))
          (other-throws (map method-record-throws (cdr mrs))))
      (add-exns-to-env (filter (lambda (throw)
                                 (andmap (lambda (throws)
                                           (member throw throws))
                                         other-throws))
                               first-throws) env)))
  
  ;get-static-fields-env: env -> env
  (define (get-static-fields-env env)
    (make-environment (filter (lambda (t) (properties-static? (var-type-properties t)))
                              (environment-types env))
                      (environment-set-vars env)
                      (environment-exns env)
                      (environment-labels env)
                      (environment-local-inners env)))

  ;field-needs-set?: field symbol bool-> bool
  (define (field-needs-set? field level abst-class?)
    (cond
      ((and (memq level '(beginner #;intermediate)) (not abst-class?) #t))
      ((memq 'final (map modifier-kind (field-modifiers field))) #t)
      (else #f)))
  
  ;get-assigns: (list member) symbol string -> (list (list assignment))
  (define (get-assigns members level class)
    (if (eq? level 'beginner)
        (list (get-beginner-assigns members class))
        (get-instance-assigns members)))
  
  ;get-beginner-assigns: (list member) string-> (list assignment)
  (define (get-beginner-assigns members class)
    (cond
      ((null? members) null)
      ((field? (car members)) (get-beginner-assigns (cdr members) class))
      ((method? (car members)) 
       (if (eq? (type-spec-name (method-type (car members))) 'ctor)
           (if (block? (method-body (car members)))
               (get-b-assigns (block-stmts (method-body (car members))) class)
               null)
           (get-beginner-assigns (cdr members) class)))))
  
  ;get-b-assigns: (list statement) string-> (list assignment)
  (define (get-b-assigns stmts class)
    (cond
      ((null? stmts) null)
      ((ifS? (car stmts)) 
       (beginner-ctor-error class (car stmts) (ifS-src (car stmts))))
      ((return? (car stmts)) 
       (beginner-ctor-error class (car stmts) (ifS-src (car stmts))))
      (else (append (get-b-assigns-expr (car stmts) class)
                    (get-b-assigns (cdr stmts) class)))))
  
  ;get-b-assigns-expr: Expression string -> assignment
  (define (get-b-assigns-expr body class)
    (cond
      ((assignment? body) 
       (unless (and (field-access? (access-name (assignment-left body)))
                    (special-name? (field-access-object (access-name (assignment-left body))))
                    (expr-src (field-access-object (access-name (assignment-left body)))))
         (beginner-assn-error 'not-left-this (expr-src (assignment-left body)))) 
       (when (and (access? (assignment-right body))
                  (field-access? (access-name (assignment-right body)))
                  (special-name? (field-access-object (access-name (assignment-right body)))))
         (beginner-assn-error 'right-this (expr-src (assignment-right body))))
       (list body))
      ((call? body) 
       (if (not (and (special-name? (call-method-name body)) 
                     (equal? "super" (special-name-name (call-method-name body)))))
           (beginner-ctor-error class body (expr-src body))
           null))
      (else 
       (beginner-ctor-error class body (expr-src body)))))
  
  ;get-instance-assigns: (list member) -> (list (list assignment))
  (define (get-instance-assigns members)
    (cond
      ((null? members) null)
      ((method? (car members))
       (if (eq? 'ctor (method-type (car members)))
           (cons (get-stmt-assigns (method-body (car members)))
                 (get-instance-assigns (cdr members)))
           (get-instance-assigns (cdr members))))
      (else (get-instance-assigns (cdr members)))))
  
  ;get-stmt-assigns: statement -> (list assign)
  (define (get-stmt-assigns b)
    (cond
      ((or (not b) (switch? b) (break? b) (continue? b)) null)
      ((ifS? b)
       (append (get-assigns-exp (ifS-cond b))
               (get-stmt-assigns (ifS-then b))
               (get-stmt-assigns (ifS-else b))))
      ((throw? b) (get-assigns-exp (throw-expr b)))
      ((return? b) (get-assigns-exp (return-expr b)))
      ((while? b) (append (get-assigns-exp (while-cond b))
                          (get-stmt-assigns (while-loop b))))
      ((doS? b) (append (get-assigns-exp (doS-cond b))
                        (get-stmt-assigns (doS-loop b))))
      ((for? b) (append (get-assigns-forInit (for-init b))
                        (get-assigns-exp (for-cond b))
                        (apply append (map get-assigns-exp (for-incr b)))
                        (get-stmt-assigns (for-loop b))))
      ((try? b) (get-stmt-assigns (try-body b)))
      ((block? b) (get-assigns-body (block-stmts b)))
      ((label? b) (get-stmt-assigns (label-stmt b)))
      ((synchronized? b) (append (get-assigns-exp (synchronized-expr b))
                                 (get-stmt-assigns (synchronized-stmt b))))
      (else (get-assigns-exp b))))
  
  ;get-assigns-forInit: (list forInit) -> (list assignment)
  (define (get-assigns-forInit b-list)
    (cond
      ((null? b-list) null)
      ((field? (car b-list)) null)
      (else (apply append (map get-assigns-exp b-list)))))
  
  ;get-assigns-body: (list statement) -> (list assignment)
  (define (get-assigns-body b-list)
    (cond
      ((null? b-list) null)
      ((field? (car b-list)) (get-assigns-body (cdr b-list)))
      (else (append (get-stmt-assigns (car b-list))
                    (get-assigns-body (cdr b-list))))))

  ;get-assigns-exp: expression -> (list assignment) 
  (define (get-assigns-exp exp)
    (cond
      ((or (not exp) (literal? exp) (special-name? exp)
           (class-alloc? exp) (inner-alloc? exp)) null)
      ((bin-op? exp) (append (get-assigns-exp (bin-op-left exp))
                             (get-assigns-exp (bin-op-right exp))))
      ((access? exp) (if (field-access? (access-name exp))
                         (get-assigns-exp (field-access-object (access-name exp)))
                         null))
      ((call? exp) (get-assigns-exp (call-expr exp)))
      ((array-alloc? exp) (apply append (map get-assigns-exp (array-alloc-size exp))))
      ((array-alloc-init? exp) (get-init-assigns (array-init-vals (array-alloc-init-init exp))))
      ((cond-expression? exp)
       (append (get-assigns-exp (cond-expression-cond exp))
               (get-assigns-exp (cond-expression-then exp))
               (get-assigns-exp (cond-expression-else exp))))
      ((array-access? exp)
       (append (get-assigns-exp (array-access-name exp))
               (get-assigns-exp (array-access-index exp))))
      ((post-expr? exp) (get-assigns-exp (post-expr-expr exp)))
      ((pre-expr? exp) (get-assigns-exp (pre-expr-expr exp)))
      ((unary? exp) (get-assigns-exp (unary-expr exp)))
      ((cast? exp) (get-assigns-exp (cast-expr exp)))
      ((instanceof? exp) (get-assigns-exp (instanceof-expr exp)))
      ((assignment? exp) (list exp))))
  
  ;get-init-assigns: (list (U Expression array-init)) -> (list assignment)
  (define (get-init-assigns inits)
    (cond
      ((null? inits) null)
      ((expr? (car inits))
       (apply append (map get-assigns-exp inits)))
      (else
       (apply append (map get-init-assigns (map array-init-vals inits))))))
          
  (define (get-static-assigns m l) null)
  
  ;field-set?: field (list assignment) string symbol bool -> bool
  (define (field-set? field assigns class level static?)
    (if (null? assigns) 
        (field-not-set-error (field-name field)
                             class
                             (if (memq level '(beginner intermediate))
                                 level
                                 (if static? 'static 'instance))
                             (field-src field))
        (let* ((assign (car assigns))
               (left (access-name (assignment-left assign))))
          (or (cond
                ((local-access? left) 
                 (equal? (id-string (local-access-name left))
                         (id-string (field-name field))))
                ((field-access? left)
                 (and (special-name? (field-access-object left))
                      (equal? "this" (special-name-name (field-access-object left)))
                      (equal? (id-string (field-access-field left))
                              (id-string (field-name field))))))
              (field-set? field
                          (if (assignment? (assignment-right assign))
                              (cons (assignment-right assign)
                                    (cdr assigns))
                              (cdr assigns))
                          class level static?)))))
  
  ;inherited-field-set? field-record (list assignment) src -> bool
  (define (inherited-field-set? field assigns src)
    (if (null? assigns)
        (inherited-field-not-set-error (field-record-name field) src)
        (let* ((assign (car assigns))
               (left (assignment-left assign)))
          (when (access? left)
            (set! left (access-name left)))
          (or (cond
                ((local-access? left)
                 (equal? (id-string (local-access-name left))
                         (field-record-name field)))
                ((field-access? left)
                 (and (special-name? (field-access-object left))
                      (equal? "this" (special-name-name (field-access-object left)))
                      (equal? (id-string (field-access-field left)) (field-record-name field)))))
              (inherited-field-set? field (cdr assigns) src)))))
  
  (define (inherited-field-not-set-error name src)
    (raise-error (string->symbol name)
                 (format "Inherited field ~a must be set in the constructor for the current class." name)
                 (string->symbol name) src))
  
  ;raise-forward-reference: id src -> void
  (define (raise-forward-reference field src)
    (let ((name (id->ext-name (id-string field))))
      (raise-error name 
                   (format "Field ~a cannot be referenced before its declaration." name)
                   name src)))
  
  ;check-method: method env type-records (list string) boolean boolean-> void
  (define (check-method method env level type-recs c-class static? iface?)
    (let* ((ctor? (eq? 'ctor (type-spec-name (method-type method))))
           (name (method-name method))
           (sym-name (string->symbol (id-string name)))
           (body (method-body method))
           (mods (map modifier-kind (method-modifiers method)))
           (return (if ctor? 
                       'void
                       (type-spec-to-type (method-type method) c-class level type-recs))))
      (when (ref-type? return)
        (add-required c-class (ref-type-class/iface return) (ref-type-path return) type-recs))
      (when (eq? 'string return)
        (add-required c-class "String" '("java" "lang") type-recs))
      (when iface? (set! mods (cons 'abstract mods)))
      (when (memq 'native mods)
        (send type-recs add-req (make-req (string-append (car c-class) "-native-methods") (cdr c-class))))
      (if (or (memq 'abstract mods) (memq 'native mods))
          (begin (when body
                   (method-error (if (memq 'abstract mods) 'abstract 'native) sym-name (id-src name)))
                 ;build the method env anyway, as that's where parametr checking happens
                 (build-method-env (method-parms method) env level c-class type-recs)
                 (void))
          (begin
            (when (not body) (method-error 'no-body sym-name (id-src name)))
            (when (and (not (eq? return 'void))
                       (or (not (memq 'abstract mods))
                           (not (memq 'native mods)))
                       (not (reachable-return? body)))
              (method-error 'no-reachable sym-name (id-src name)))
            (check-statement body
                             return
                             (add-exns-to-env (map (lambda (n)
                                                     (name->type n c-class (name-src n) level type-recs))
                                                   (method-throws method))
                                              (build-method-env (method-parms method) env level c-class type-recs))
                             level type-recs c-class
                             ctor? static? #f #f #f)
            ))))
  
  ;build-method-env: (list field) env symbol (list string) type-records-> env
  (define (build-method-env parms env level c-class type-recs)
    (cond
      ((null? parms) env)
      (else
       (when (ref-type? (field-type (car parms)))
         (add-required c-class (ref-type-class/iface (field-type (car parms)))
                       (ref-type-path (field-type (car parms))) type-recs))
       (when (eq? 'string (field-type (car parms)))
         (add-required c-class "String" '("java" "lang") type-recs))
       (build-method-env (cdr parms)
                         (add-var-to-env (id-string (field-name (car parms)))
                                         (field-type (car parms))
                                         (if (memq 'final (field-modifiers (car parms)))
                                             final-parm
                                             parm)
                                         env)
                         level
                         c-class
                         type-recs))))

  ;reachable-return?: statement -> bool
  (define (reachable-return? body)
    (cond
      ((ifS? body) 
       (if (ifS-else body)
           (and (reachable-return? (ifS-then body))
                (reachable-return? (ifS-else body)))
           #f))
      ((throw? body) #t)
      ((return? body) #t)
      ((while? body) #f #;(reachable-return? (while-loop body)))
      ((doS? body)  #f #;(reachable-return? (doS-loop body)))
      ((for? body)  #f #;(reachable-return? (for-loop body)))
      ((try? body) 
       (and (reachable-return? (try-body body))
            (or (and (try-finally body)
                     (reachable-return? (try-finally body)))
                #t)
            (andmap reachable-return? (map catch-body (try-catches body)))))
      ((switch? body) #f)
      ((block? body)
       (if (null? (block-stmts body))
           #f
           (reachable-return? (list-ref (block-stmts body)
                                        (sub1 (length (block-stmts body)))))))
      ((break? body) #f)
      ((continue? body) #f)
      ((label? body) (reachable-return? (label-stmt body)))
      ((synchronized? body) (reachable-return? (synchronized-stmt body)))
      (else #f)))

  ;check-var-init: expression (exp env -> type/env) type symbol string type-records -> type/env
  (define (check-var-init init check-e env dec-type name var-kind type-recs)
    (let ((type (if (array-init? init)
                    (if (array-type? dec-type)
                        (begin
                          (send type-recs add-req (make-req 'array null))
                          (check-array-init (array-init-vals init) check-e env
                                            (array-type-type dec-type) type-recs))
                        (var-init-error 'array var-kind name dec-type #f (array-init-src init)))
                    (check-e init env))))
      (unless (assignment-conversion dec-type (type/env-t type) type-recs)
        (var-init-error 'other var-kind name dec-type (type/env-t type) (expr-src init)))
      type))
  
  ;check-array-init (U (list array-init) (list exp)) (exp env->type) type type-records -> type/env
  (define (check-array-init inits check-e env dec-type type-recs)
    (cond
      ((null? inits) (make-type/env (make-array-type dec-type 1) env))
      ((array-init? (car inits))
       (let ((array-type/env (check-array-init-sub-inits inits check-e env dec-type type-recs)))
         (make-type/env 
          (make-array-type dec-type (add1 (array-type-dim (type/env-t array-type/env))))
          (type/env-e array-type/env))))
      (else
       (let loop ((exps inits) (env env))
         (cond 
           ((null? exps) (make-type/env (make-array-type dec-type 1) env))
           (else
            (let ((new-type/env (check-e (car exps) env)))
              (unless (assignment-conversion dec-type (type/env-t new-type/env) type-recs)
                (array-init-error dec-type (type/env-t new-type/env) (expr-src (car exps))))
              (loop (cdr exps) (type/env-e new-type/env)))))))))
  
  ;check-array-init-sub-inits: (list array-init) (exp env -> type/env) env type type-records -> type/env
  (define (check-array-init-sub-inits inits check-e env type type-recs)
    (cond
      ((null? (cdr inits))
       (check-array-init (array-init-vals (car inits)) check-e env type type-recs))
      (else
       (check-array-init-sub-inits (cdr inits) check-e
                                   (type/env-e (check-array-init (array-init-vals (car inits)) check-e env type type-recs))
                                   type type-recs))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Member errors

  (define (method-error kind method src)
    (raise-error method
                 (case kind
                   ((no-reachable) (format "Method ~a does not have a reachable return." method))
                   ((abstract)
                    (let ((line1 
                           (format "Abstract method ~a has an implementation, abstract methods may not have implementations."
                                   method))
                          (line2 "Either a ';'should come after the header, or the method should not be abstract."))
                      (format "~a~n~a" line1 line2)))
                   ((native) (format "Native method ~a has an implementation which is not allowed." method))
                   ((no-body) (format "Method ~a has no implementation and is not abstract." method)))
                 method src))
  
  ;var-init-error: symbol string symbol type type src -> void
  (define (var-init-error kind var-kind name dec-type given src)
    (raise-error name
                 (case kind
                   ((array)
                    (format "The value of ~a ~a must be a subtype of declared type ~a, given an array." 
                            var-kind name (type->ext-name dec-type)))
                   ((other)
                    (format "The declared type of ~a ~a must be a super type of the expression. ~a is not a super type of ~a."
                            var-kind name (type->ext-name dec-type) (type->ext-name given))))
                 name src))

  ;array-init-error: type type src -> void
  (define (array-init-error dec-type given src)
    (let ((d (type->ext-name dec-type))
          (g (type->ext-name given)))
      (raise-error g
                   (format "Error initializing declared array of ~a, given element with incompatible type ~a."
                           d g)
                   d src)))

  ;field-not-set-error: id string symbol src
  (define (field-not-set-error name class kind src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (format "Field ~a from ~a must be set in the ~a and is not."
                           n
                           class
                           (case kind
                             ((beginner intermediate) "constructor")
                             ((instance) "constructor or instance initialization")
                             ((static) "static initialization")))
                   n src)))

  ;beginner-ctor-error: string statement src -> void
  (define (beginner-ctor-error class kind src)
    (let ((exp (statement->ext-name kind)))
      (raise-error exp
                   (format "Constructor for ~a may only assign the fields of ~a. Found illegal statement ~a."
                           class class exp)
                   exp src)))
  
  ;beginner-assn-error: sym src -> void
  (define (beginner-assn-error kind src)
    (raise-error 
     '=
     (case kind
       ((not-left-this)
        "Constructor must assign the class's fields. This expression is not a field of this class and maynot be assigned.")
       ((right-this)
        "The constructor maynot assign fields with other of its fields. Other values must be used."))
     '= src))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Statement checking functions
  
  ;;check-statement: statement type env symbol type-records (U #f string) bool bool bool bool bool-> type/env
  (define (check-statement statement return env level type-recs c-c ctor? static? in-loop? in-switch? interactions?)
    (let* ((check-s (lambda (stmt env in-l? in-s?)
                      (check-statement stmt return env level type-recs c-c ctor? static? in-l? in-s? interactions?)))
           (check-s-env-change (lambda (smt env) (check-s smt env in-loop? in-switch?)))
           (check-s-no-change (lambda (stmt) (check-s stmt env in-loop? in-switch?)))
           (check-e (lambda (exp env)
                      (check-expr exp env level type-recs c-c ctor? static? interactions? #f)))
           (check-e-no-change (lambda (exp) (check-e exp env))))
      (cond
        ((ifS? statement) 
         (check-ifS (check-e-no-change (ifS-cond statement))
                    (expr-src (ifS-cond statement))
                    check-s-env-change
                    (ifS-then statement)
                    (ifS-else statement)))
        ((throw? statement)
         (check-throw (check-e-no-change (throw-expr statement))
                      (expr-src (throw-expr statement))
                      env
                      interactions?
                      type-recs))
        ((return? statement)
         (check-return statement
                       (return-expr statement)
                       return
                       env
                       check-e-no-change
                       (return-src statement)
                       interactions?
                       level
                       type-recs))
        ((while? statement) 
         (check-while (check-e-no-change (while-cond statement))
                      (expr-src (while-cond statement))
                      check-s
                      (while-loop statement)))
        ((doS? statement) 
         (check-do check-e
                   (doS-cond statement)
                   (expr-src (doS-cond statement))
                   (check-s (doS-loop statement) env #t #f)))
        ((for? statement)
         (check-for (for-init statement)
                    (for-cond statement)
                    (for-incr statement)
                    (for-loop statement)
                    check-e
                    check-s
                    env
                    level
                    c-c
                    type-recs
                    in-switch?))
        ((try? statement) 
         (check-try (try-body statement)
                    (try-catches statement)
                    (try-finally statement)
                    env
                    check-s-env-change
                    type-recs))
        ((switch? statement)
         (check-switch (check-e-no-change (switch-expr statement))
                       (expr-src (switch-expr statement))
                       (switch-cases statement)
                       in-loop?
                       env
                       check-e-no-change
                       check-s))
        ((block? statement)
         (check-block (block-stmts statement)
                      env
                      check-s-env-change
                      check-e
                      level
                      c-c
                      type-recs))
        ((def? statement) 
         (check-local-inner statement
                                env
                                level
                                c-c
                                type-recs))
        ((break? statement)
         (check-break (break-label statement)
                      (break-src statement)
                      in-loop?
                      in-switch?
                      level
                      env))
        ((continue? statement)
         (check-continue (continue-label statement)
                         (continue-src statement)
                         env
                         in-loop?))
        ((label? statement)
         (check-label (label-stmt statement)
                      (label-label statement)
                      check-s-env-change
                      env))
        ((synchronized? statement)
         (check-synchronized (check-e-no-change (synchronized-expr statement))
                             (expr-src (synchronized-expr statement)))
         (check-s-no-change (synchronized-stmt statement)))
        ((statement-expression? statement)
         (check-e-no-change statement)))))
  
  ;check-cond: symbol -> (type src -> void)
  (define (check-cond kind)
    (lambda (cond? cond-src)
      (let ((check 
             (lambda (t) 
               (unless (eq? 'boolean t)
                 (kind-condition-error kind t cond-src)))))
        (cond
          ((and (dynamic-val? cond?) (dynamic-val-type cond?)) => check)
          ((dynamic-val? cond?) (set-dynamic-val-type! cond? 'boolean))
          (else (check cond?))))))
        
  ;check-ifS: type/env src (stmt env -> type/env) stmt (U stmt #f) -> type/env
  (define (check-ifS cond-t/e src check-s then else)
    ((check-cond 'if) (type/env-t cond-t/e) src)
    (let ((then/env (check-s then (type/env-e cond-t/e)))
          (else/env (and else (check-s else (type/env-e cond-t/e)))))
      (if else/env
          (make-type/env 'void 
                         (intersect-var-sets 
                          (type/env-e cond-t/e) (type/env-e then/env) (type/env-e else/env)))
          cond-t/e)))
        
  ;check-throw: type/env src env bool type-records -> type/env
  (define (check-throw exp/env src env interact? type-recs)
    (let ((exp-type (type/env-t exp/env)))
      (cond
        ((and (dynamic-val? exp-type) (dynamic-val-type exp-type))
         =>
         (lambda (t) (check-throw t src env interact? type-recs)))
        ((dynamic-val? exp-type) 
         (set-dynamic-val-type! exp-type throw-type))
        ((or (not (ref-type? exp-type))
             (not (is-eq-subclass? exp-type throw-type type-recs)))
         (throw-error 'not-throwable exp-type src))
        ((not (is-eq-subclass? exp-type runtime-exn-type type-recs))
         (unless (or interact? (lookup-exn exp-type env type-recs 'full))
           (throw-error 'not-declared exp-type src))))
      (send type-recs add-req (make-req "Throwable" (list "java" "lang")))
      exp/env))
    
  ;check-return: statement expression type env (expression -> type/env) src bool symbol type-records -> type/env
  (define (check-return stmt ret-expr return env check src interact? level type-recs)
    (cond
      (interact? (check ret-expr))
      ((and ret-expr (not (eq? 'void return)))
       (set-return-exp-type! stmt return)
       (let ((ret/env (check ret-expr)))
         (if (assignment-conversion return (type/env-t ret/env) type-recs)
             ret/env
             (return-error 'not-equal (type/env-t ret/env) return src))))
      ((and ret-expr (eq? 'void return))
       (return-error 'void #f return src))
      ((and (not ret-expr) (not (eq? 'void return)))
       (return-error 'val #f return src))
      (else (make-type/env 'void env))))
  
  ;check-while: type/env  src -> void
  (define (check-while cond/env src check-s loop-body)
    ((check-cond 'while) (type/env-t cond/env) src)
    (check-s loop-body (type/env-e cond/env) #t #f)
    (make-type/env 'void (type/env-e cond/env)))
        
  ;check-do: (exp env -> type/env) exp src type/env -> type/env
  (define (check-do check-e exp src loop/env)
    (let ((cond/env (check-e exp (type/env-e loop/env))))
      ((check-cond 'do) (type/env-t cond/env) src)
      cond/env))
    
  ;check-for: forInit exp (list exp) stmt (exp env -> type/env) 
  ;           (stmt env bool bool-> type/env) env symbol (list string) type-records bool -> type/env
  (define (check-for init cond incr loop check-e check-s env level c-class type-recs in-switch?)
    (let* ((inits-env (if (and (not (null? init)) (field? (car init)))
                          (check-for-vars init env check-e level c-class type-recs)
                          (check-for-exps init env check-e)))
           (cond/env (check-e cond inits-env)))
      ((check-cond 'for) (type/env-t cond/env) (expr-src cond))
      (check-s loop (check-for-exps incr inits-env check-e) #t in-switch?))
    (make-type/env 'void env))

  ;check-for-vars: (list field) env (expression env -> type/env) symbol (list string) type-records -> env
  (define (check-for-vars vars env check-e level c-class types)
    (or (and (null? vars) env)
        (check-for-vars (cdr vars)
                        (check-local-var (car vars) env check-e level c-class types) 
                        check-e level c-class types)))
  
  ;check-for-exps (list exp) env (exp env -> type/env) -> env
  (define (check-for-exps exps env check-e)
    (or (and (null? exps) env)
        (check-for-exps (cdr exps)
                        (type/env-e (check-e (car exps) env))
                        check-e)))
  
  ;check-local-var: field env (exp env -> type/env) symbol (list string) type-records -> env
  (define (check-local-var local env check-e level c-class type-recs)
    (let* ((is-var-init? (var-init? local))
           (name (id-string (field-name local)))
           (in-env? (lookup-var-in-env name env))
           (sym-name (string->symbol name))
           (type (type-spec-to-type (field-type-spec local) c-class level type-recs))
           (new-env (lambda (extend-env) (add-var-to-env name type method-var extend-env))))
      (set-field-type! local type)
      (when (ref-type? type)
        (add-required c-class (ref-type-class/iface type) (ref-type-path type) type-recs))
      (when (eq? 'string type)
        (add-required c-class "String" '("java" "lang")))
      (when (and in-env? (not (properties-field? (var-type-properties in-env?))))
        (illegal-redefinition (field-name local) (field-src local)))
      (if is-var-init?
          (let ((new-type/env (check-var-init (var-init-init local) check-e env type sym-name "local variable" type-recs)))
            (unless (assignment-conversion type (type/env-t new-type/env) type-recs)
              (variable-type-error (field-name local) (type/env-t new-type/env) type (var-init-src local)))
            (add-set-to-env name (new-env (type/env-e new-type/env))))
          (new-env env))))

  ;check-try: statement (list catch) (U #f statement) env (statement env -> type/env) type-records -> type-env
  (define (check-try body catches finally env check-s type-recs)
    (let* ((new-env
            (let loop ((catches catches) (new-env env))
              (if (null? catches)
                  new-env
                  (let* ((catch (car catches))
                         (type (type-spec-to-type (field-type-spec (catch-cond catch)) #f 'full type-recs)))
                    (unless (and (ref-type? type)
                                 (is-eq-subclass? type throw-type type-recs))
                      (catch-error type (field-src (catch-cond catch))))
                    (set-field-type! (catch-cond catch) type)
                    (add-required '("" "") (ref-type-class/iface type) (ref-type-path type) type-recs)
                    (loop (cdr catches) (add-exn-to-env type env))))))
           (body-res (check-s body new-env)))
      (add-required '("" "") "Throwable" '("java" "lang") type-recs)
      (for-each (lambda (catch)
                  (let* ((field (catch-cond catch))
                         (name (id-string (field-name field)))
                         (in-env? (lookup-var-in-env name env)))
                    (if (and in-env? (not (properties-field? (var-type-properties in-env?))))
                        (illegal-redefinition (field-name field) (field-src field))
                        (check-s (catch-body catch)
                                 (add-var-to-env name (field-type field) parm env)))))
                catches)
      (when finally (check-s finally env))
      (make-type/env 'void (unnest-var env (type/env-e body-res)))))

  ;INCORRECT!!! This doesn't properly type check and I'm just raising an error for now
  ;Skipping proper checks of the statements + proper checking that constants aren't repeated
  ;check-switch: type src (list caseS) bool env (expression -> type) (statement env bool bool -> void) -> void
  (define (check-switch expr-type expr-src cases in-loop? env check-e check-s)
    (error 'internal-error "check-switch: Switch statements are not correctly implemented")
    (when (or (eq? expr-type 'long)
              (not (prim-integral-type? expr-type)))
      (switch-error 'switch-type 'switch expr-type #f expr-src))
    (for-each (lambda (case)
                (let* ((constant (caseS-constant case))
                       (cons-type (unless (eq? 'default constant) (check-e constant))))
                  (if (or (eq? 'default constant)
                          (type=? cons-type expr-type))
                      void
                      (switch-error 'incompat 'case cons-type expr-type (expr-src constant)))))
              cases))
  
  ;check-block: (list (U stmt field)) env (stmt env -> type/env) (expr env -> type/env) symbol 
  ;             (list string) type-records -> type/env
  (define (check-block stmts env check-s check-e level c-class type-recs)
    (let loop ((stmts stmts) (block-env env))
      (cond 
        ((null? stmts) (make-type/env 'void (unnest-var env block-env)))
        ((field? (car stmts))
         (loop (cdr stmts) 
               (check-local-var (car stmts) block-env check-e level c-class type-recs)))
        (else
         (loop (cdr stmts) (type/env-e (check-s (car stmts) block-env)))))))
      
  ;check-local-inner: def env symbol (list string) type-records -> type/env
  (define (check-local-inner def env level c-class type-recs)
    ;((update-class-with-inner) def)
    (let ((original-name (id-string (def-name def)))
          (rec/new-name (check-inner-def def level type-recs c-class env)))
      (make-type/env 
       (make-ref-type original-name null) 
       (add-local-inner-to-env original-name (car rec/new-name) (cadr rec/new-name) (caddr rec/new-name) env))))
  
  ;check-break: (U id #f) src bool bool symbol env-> type/env
  (define (check-break label src in-loop? in-switch? level env)
    (cond
      (label
       (unless (lookup-label (id-string label) env)
         (illegal-label 'break (id-string label) (id-src label))))
      ((not (or in-loop? in-switch?)) (break-error src level)))
    (make-type/env 'void env))
  
  ;check-continue: (U id #f) src env bool -> type/env
  (define (check-continue label src env in-loop?)
    (cond
      (label
       (unless (lookup-label (id-string label) env)
         (illegal-label 'continue (id-string label) (id-src label))))
      ((not in-loop?) (continue-error src)))
    (make-type/env 'void env))
  
  ;check-label: statement string (statement env -> void) env -> type/env
  (define (check-label stmt label check-s env)
    (check-s stmt (add-label-to-env label env)))

  ;check-synchronized: type/env src -> type/env
  (define (check-synchronized e-type e-src)
    (unless (reference-type? (type/env-t e-type))
      (synch-error (type/env-t e-type) e-src))
    e-type)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Statement error messages

  ;make-condition-error: symbol type src -> void
  (define (kind-condition-error kind cond src)
    (raise-error kind
                 (format "~a condition must be a boolean: Given ~a." 
                         kind (type->ext-name cond))
                 kind src))
    
  ;throw-error: symbol type src -> void
  (define (throw-error kind thrown src)
    (let ((t (type->ext-name thrown)))
      (raise-error 'throw
                   (case kind
                     ((not-throwable)
                      (format "Expression for throw must be a subtype of Throwable: given ~a." t))
                     ((not-declared)
                      (format "Thrown type ~a must be declared as thrown or caught." t)))
                   'throw src)))
  
  ;return-error: symbol type type src -> void
  (define (return-error kind given expected src)
    (let ((g (type->ext-name given))
          (e (type->ext-name expected)))
      (raise-error 
       'return
       (case kind
         ((not-equal)
          (let ((line1 
                 (format "The return expression's type must be equal to or a subclass of the method's return ~a." e))
                (line2
                 (format "The given expression has type ~a which is not equivalent to the declared return." g)))
            (format "~a~n~a" line1 line2)))
         ((void) "No value should be returned from void method, found a returned value.")
         ((val)
          (format "Expected a return value assignable to ~a. No value was given." e)))
       'return src)))

  ;illegal-redefinition: id src -> void
  (define (illegal-redefinition field src)
    (let ((f (id->ext-name field)))
      (raise-error 
       f
       (format "Variable name ~a has already been used and may not be reused. Another name must be chosen" f)
       f src)))
  
  ;variable-type-error: id type type src -> void
  (define (variable-type-error field given expt src)
    (let ((f (id->ext-name field)))
      (raise-error 
       f
       (format "Variable ~a was declared to be ~a, which is incompatible with the initial value type of ~a"
               f (type->ext-name expt) (type->ext-name given))
       f src)))

  ;catch-error: type src -> void
  (define (catch-error given src)
    (raise-error 'catch
                 (format "Catch clause must catch an argument of subclass Throwable: Given ~a"
                         (type->ext-name given))
                 'catch src))
  
  ;switch-error symbol symbol type type src -> void
  (define (switch-error kind syn given expected src)
    (raise-error
     syn
     (case kind
       ((switch-type) 
        (format "switch expression must be of type byte, short, int or char. Given: ~a" 
                (type->ext-name given)))
       ((incompat)
        (format "switch case must be same type as switch expression. Given ~a: expected ~a"
                (type->ext-name given) (type->ext-name expected))))
     syn src))
  
  ;illegal-label: symbol string src -> void
  (define (illegal-label kind label src)
    (raise-error kind
                 (format "~a references label ~a, no enclosing statement has this label."
                         kind label)
                 kind src))
                 
  ;break-error: src -> void
  (define (break-error src level)
    (raise-error 'break (if (eq? level 'full) 
                            "'break' must be in either a loop or a switch."
                            "'break' must be in a loop.")
                 'break src))
  (define (continue-error src)
    (raise-error 'continue "'continue' must be in a loop." 'continue src))

  ;synch-error: type src -> void
  (define (synch-error given src)
    (raise-error 'synchronize
                 (format "Synchronization expression must be a subtype of Object: Given ~a"
                         (type->ext-name given))
                 'synchronize src))                                        
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Expression checking functions
  
  ;; check-expr: expression env symbol type-records (U string #f) bool bool bool bool-> type/env
  (define (check-expr exp env level type-recs c-class ctor? static? interact? assign-left?)
    (let ((check-sub-expr 
           (lambda (expr env) (check-expr expr env level type-recs c-class ctor? static? interact? assign-left?)))
          (check-assign-left
           (lambda (expr env) (check-expr expr env level type-recs c-class ctor? static? interact? #t))))
      (cond
        ((literal? exp)
         (make-type/env
          (cond
            ((memq (expr-types exp) `(String string))
             (add-required c-class "String" `("java" "lang") type-recs)
             (set-expr-type exp string-type))
            ((eq? (expr-types exp) 'image)
             (get-record (send type-recs get-class-record '("Image" "graphics") #f
                               ((get-importer type-recs) '("Image" "graphics")
                                                         type-recs level (expr-src exp))) type-recs)
             (add-required c-class "Image" `("graphics") type-recs)
             (set-expr-type exp (make-ref-type "Image" '("graphics"))))
            (else (expr-types exp))) env))
        ((bin-op? exp)
         (set-expr-type exp 
                        (check-bin-op (bin-op-op exp) (bin-op-left exp) (bin-op-right exp)
                                      check-sub-expr env
                                      (expr-src exp)
                                      level type-recs)))
        ((access? exp)
         (set-expr-type exp
                        (check-access exp check-sub-expr env level type-recs c-class interact? static? assign-left?)))
        ((special-name? exp)
         (make-type/env (set-expr-type exp (check-special-name exp env static? interact?)) env))
        ((specified-this? exp)
         (make-type/env (set-expr-type exp (check-specified-this exp env static? interact? level type-recs)) env))
        ((call? exp)
         (set-expr-type exp (check-call exp
                                        (call-args exp)
                                        check-sub-expr
                                        c-class
                                        level
                                        env type-recs
                                        ctor? static? interact?)))
        ((class-alloc? exp)
         (set-expr-type exp
                        (check-class-alloc exp
                                           (class-alloc-name exp)
                                           (class-alloc-args exp)
                                           check-sub-expr
                                           (expr-src exp)
                                           type-recs
                                           c-class
                                           env level static? interact?)))
        ((inner-alloc? exp)
         (set-expr-type exp
                        (check-inner-alloc exp 
                                           (inner-alloc-obj exp)
                                           (inner-alloc-name exp)
                                           (inner-alloc-args exp)
                                           check-sub-expr
                                           (expr-src exp)
                                           type-recs
                                           c-class
                                           env
                                           level static?)))
        ((def? exp) 
         (set-expr-type exp
                        (check-local-inner exp type-recs c-class env level)))
        ((array-alloc? exp)
         (set-expr-type exp
                        (check-array-alloc (array-alloc-name exp)
                                           (array-alloc-size exp)
                                           (array-alloc-dim exp)
                                           (expr-src exp)
                                           check-sub-expr
                                           env
                                           level
                                           c-class
                                           type-recs)))
        ((array-alloc-init? exp)
         (set-expr-type exp
                        (check-array-alloc-init (array-alloc-init-name exp)
                                                (array-alloc-init-dim exp)
                                                (array-alloc-init-init exp)
                                                (expr-src exp)
                                                check-sub-expr
                                                env
                                                level
                                                c-class
                                                type-recs)))
        ((cond-expression? exp)
         (set-expr-type exp
                        (check-cond-expr (check-sub-expr (cond-expression-cond exp) env)
                                         (cond-expression-then exp)
                                         (cond-expression-else exp)
                                         check-sub-expr
                                         (expr-src exp)
                                         (expr-src (cond-expression-cond exp))
                                         level
                                         type-recs)))
        ((array-access? exp)
         (set-expr-type exp
                        (check-array-access (check-sub-expr (array-access-name exp) env)
                                            (array-access-index exp)
                                            check-sub-expr
                                            (expr-src exp)
                                            type-recs)))
        ((post-expr? exp)
         (set-expr-type exp
                        (check-pre-post-expr (check-sub-expr (post-expr-expr exp) env)
                                             (post-expr-op exp)
                                             (expr-src exp))))
        ((pre-expr? exp)
         (set-expr-type exp
                        (check-pre-post-expr (check-sub-expr (pre-expr-expr exp) env)
                                             (pre-expr-op exp)
                                             (expr-src exp))))      
        ((unary? exp)
         (set-expr-type exp
                        (check-unary (check-sub-expr (unary-expr exp) env)
                                     (unary-op exp)
                                     (expr-src exp))))
        ((cast? exp)
         (set-expr-type exp
                        (check-cast (check-sub-expr (cast-expr exp) env)
                                    (cast-type exp)
                                    (expr-src exp)
                                    level
                                    c-class 
                                    type-recs)))
        ((instanceof? exp)
         (set-expr-type exp
                        (check-instanceof (check-sub-expr (instanceof-expr exp) env) 
                                          (instanceof-type exp)
                                          (expr-src exp)
                                          level
                                          c-class
                                          type-recs)))
        ((assignment? exp)
         (set-expr-type exp
                        (check-assignment (assignment-op exp)
                                          (assignment-left exp)
                                          (assignment-right exp)
                                          check-assign-left
                                          check-sub-expr
                                          (expr-src exp)
                                          ctor?
                                          #f ; static-init?
                                          c-class
                                          level
                                          type-recs
                                          env)))
        ((check? exp)
         (set-expr-type exp
                        (check-test-exprs exp
                                          check-sub-expr
                                          env level type-recs)))
         )))

  ;;check-bin-op: symbol exp exp (exp env -> type/env) env src-loc symbol type-records -> type/env
  ;;Fully checks bin-ops, including checking the subexpressions
  (define (check-bin-op op left right check-e env src level type-recs)
    (let* ((l/env (check-e left env))
           (r/env (check-e right (type/env-e l/env))))
      (make-type/env 
       (bin-op-type-check op (type/env-t l/env) (type/env-t r/env) src level type-recs)
       (type/env-e r/env))))

  ;;bin-op-type-check: symbol type type src symbol type-recs -> type
  ;Just verifies the types of the operation, used by bin-op and assignment
  (define (bin-op-type-check op l r src level type-recs)
    (case op
      ((* / % *= /= %=)       ;; 15.17
       (prim-check prim-numeric-type? binary-promotion 'num l r op src))
      ((+ - += -=)      ;; 15.18
       (if (and (memq level '(advanced full))
                (eq? '+ op) (or (is-string-type? l) (is-string-type? r)))
           string-type
           (prim-check prim-numeric-type? binary-promotion 'num l r op src)))
      ((<< >> >>> <<= >>= >>>=)      ;; 15.19
       (prim-check prim-integral-type? 
                   (lambda (l r) (unary-promotion l)) 'int l r op src))
      ((< > <= >=)      ;; 15.20
       (prim-check prim-numeric-type? (lambda (l r) 'boolean) 'num l r op src))
      ((== !=)      ;; 15.21
       (cond
         ((eq? level 'beginner)
          (if (or (and (prim-integral-type? l) (prim-integral-type? r))
                  (and (eq? 'boolean l) (eq? 'boolean r)))
              'boolean
              (bin-op-beginner-error op l r src)))
         ((or (and (prim-numeric-type? l) (prim-numeric-type? r))
              (and (eq? 'boolean l) (eq? 'boolean r)))
          'boolean)
         ((and (reference-or-array-type? l) (reference-or-array-type? r))
          (let ((right-to-left (castable? l r type-recs))
                (left-to-right (castable? r l type-recs)))
            (cond 
              ((or right-to-left left-to-right) 'boolean)
              (else (bin-op-equality-error 'both op l r src)))))
         (else 
          (bin-op-equality-error 'prim op l r src))))
      ((& ^ or &= ^= or=)      ;; 15.22
       (cond
         ((and (prim-integral-type? l) (prim-integral-type? r)) (binary-promotion l r))
         ((and (eq? 'boolean l) (eq? 'boolean r)) 'boolean)
         (else (bin-op-bitwise-error op l r src))))
      ((&& oror)      ;; 15.23, 15.24
       (prim-check (lambda (b) (or (dynamic-val? b) (eq? b 'boolean)))
                   (lambda (l r) 
                     (when (dynamic-val? l) (set-dynamic-val-type! l 'boolean))
                     (when (dynamic-val? r) (set-dynamic-val-type! r 'boolean))
                     'boolean)
                   'bool l r op src))))
  
  ;prim-check: (type -> bool) (type type -> type) type type src -> type
  (define (prim-check ok? return expt l r op src)
    (cond
      ((and (ok? l) (ok? r)) (return l r))
      ((ok? l) (bin-op-prim-error 'right op expt l r src))
      ((ok? r) (bin-op-prim-error 'left op expt l r src))
      (else (bin-op-prim-error 'both op expt l r src))))

  ;; 5.6.1
  ;;unary-promotion: type -> symbol
  (define (unary-promotion t)
    (cond
      ((and (dynamic-val? t) (dynamic-val-type t))
       (unary-promotion (dynamic-val-type t)))
      ((dynamic-val? t) 
       (set-dynamic-val-type! t 'int) 'int)
      (else 
       (case t ((byte short char) 'int) (else t)))))
  
  ;; 5.6.2
  ;; binary-promotion: type type -> type
  (define (binary-promotion t1 t2)
    (cond
      ((and (dynamic-val? t1) (dynamic-val? t2))
       (cond
         ((and (dynamic-val-type t1) (dynamic-val-type t2))
          (binary-promotion (dynamic-val-type t1) (dynamic-val-type t2)))
         ((dynamic-val-type t1)
          (binary-promotion (dynamic-val-type t1) t2))
         ((dynamic-val-type t2)
          (binary-promotion t1 (dynamic-val-type t2)))
         (else (make-dynamic-val #f))))
      ((dynamic-val? t1)
       (cond
         ((dynamic-val-type t1) (binary-promotion (dynamic-val-type t1) t2))
         (else (set-dynamic-val-type! t1 t2) t2)))
      ((dynamic-val? t2)
       (cond
         ((dynamic-val-type t2) (binary-promotion t1 (dynamic-val-type t2)))
         (else (set-dynamic-val-type! t2 t1) t1)))
      ((or (eq? 'double t1) (eq? 'double t2)) 'double)
      ((or (eq? 'float t1) (eq? 'float t2)) 'float)
      ((or (eq? 'long t1) (eq? 'long t2)) 'long)
      (else 'int)))

  (define (get-inners class type-recs)
    (let ((rec (get-record (send type-recs get-class-record class) type-recs)))
      (class-record-inners rec)))
  
  (define (inner-member class inners)
    (member (car class) (map inner-record-full-name inners)))
  
  ;;check-access: expression (expr env -> type/env) env symbol type-records (list string) bool bool bool -> type/env
  (define (check-access exp check-e env level type-recs c-class interact? static? assign-left?)
    (let ((acc (access-name exp)))
      (cond
        ((field-access? acc)
         (let* ((obj (field-access-object acc))
                (obj-type/env (check-e obj env))
                (fname (id-string (field-access-field acc)))
                (src (id-src (field-access-field acc)))
                (class-rec null)
                (record
                 (cond
                   ((and obj (dynamic-val? (expr-types obj)))
                    (set-dynamic-val-type! (expr-types obj) 
                                           (make-unknown-ref (make-field-contract fname (make-dynamic-val #f))))
                    (expr-types obj))
                   (obj (field-lookup fname (type/env-t obj-type/env) obj src level type-recs))
                   (else
                    (let* ((name (var-access-class (field-access-access acc))))
                      (set! class-rec
                            ;First clause: static field of a local inner class
                            (or (and (or (string? name) (= 1 (length name)))
                                     (let ((rec? (lookup-local-inner (if (pair? name) (car name) name) env)))
                                       (and rec? (inner-rec-record rec?))))
                                (get-record (send type-recs get-class-record 
                                                  (if (pair? name) name (list name))
                                                  #f
                                                  ((get-importer type-recs) name type-recs level src))
                                            type-recs)))
                      (cond
                        ((class-record? class-rec)
                         (get-field-record fname class-rec
                                           (lambda () 
                                             (let* ((class? (member fname (send type-recs get-class-env)))
                                                    (method? (not (null? (get-method-records fname class-rec type-recs)))))
                                               (field-lookup-error (if class? 'class-name 
                                                                       (if method? 'method-name 'not-found))
                                                                   (string->symbol fname)
                                                                   (make-ref-type (if (pair? name) (car name) name) null)
                                                                   src)))))
                        ((scheme-record? class-rec)
                         (module-has-binding? class-rec fname 
                                              (lambda () (field-lookup-error 'not-found
                                                                             (string->symbol fname)
                                                                             (make-ref-type (if (pair? name) (car name) name) 
                                                                                            (list "scheme"))
                                                                             src)))
                         (set-id-string! (field-access-field acc) (java-name->scheme fname))
                         (make-dynamic-val #f))))))))
           (cond 
             ((field-record? record)
              (let* ((field-class (if (null? (cdr (field-record-class record))) 
                                      (cons (car (field-record-class record))
                                            (send type-recs lookup-path 
                                                  (car (field-record-class record)) (lambda () null)))
                                      (field-record-class record)))
                     (mods (field-record-modifiers record))
                     (public? (memq 'public mods))
                     (private? (memq 'private mods))
                     (protected? (memq 'protected mods))
                     (local-inner-field-class? 
                      (and (null? (cdr field-class))
                           (lookup-local-inner (car field-class) env))))
                
                (when (and (memq level '(beginner intermediate intermediate+access))
                           (special-name? obj)
                           (not (lookup-var-in-env fname env)))
                  (access-before-define (string->symbol fname) src))
                                
                (when (and (eq? 'beginner level)
                           assign-left?
                           (special-name? obj)
                           (properties-set? (var-type-properties (lookup-field-in-env fname env))))
                  (assign-twice (string->symbol fname) src))
                
                (when (and (eq? 'beginner level)
                           assign-left?
                           (special-name? obj))
                  (set-properties-set?! (var-type-properties (lookup-field-in-env fname env)) #t))
                
                (when (and (eq? 'beginner level)
                           (special-name? obj)
                           (not (properties-set? (var-type-properties (lookup-field-in-env fname env)))))
                  (access-before-assign (string->symbol fname) src))
                           
                
                (when (and (field-access-access acc)
                           (var-access-static? (field-access-access acc)))
                  (unless (memq 'static mods)
                    (not-static-field-access-error (string->symbol fname) level src)))
                
                (when (and (eq? level 'beginner)
                           (eq? (car c-class) (car field-class))
                           (or (not obj) (and (special-name? obj) (not (expr-src obj)))))
                  (beginner-field-access-error (string->symbol fname) src))                    
                
                (when private?
                  (unless (or (equal? c-class field-class)
                              (inner-member c-class (get-inners field-class type-recs)))
                  (illegal-field-access 'private (string->symbol fname) level (car field-class) src)))
                
                (when (and protected? 
                           (not (or (and (equal? c-class '("scheme-interactions"))
                                         (equal? (send type-recs get-interactions-package)
                                               (cdr field-class)))
                                    (equal? c-class field-class)
                                    (is-subclass? c-class (make-ref-type (car field-class) (cdr field-class)) type-recs)
                                    (package-members? c-class field-class type-recs))))
                  (illegal-field-access 'protected (string->symbol fname) level (car field-class) src))
                          
                (when (and (not private?) (not protected?)
                           (not public?) (not (package-members? c-class field-class type-recs)))
                  (illegal-field-access 'package (string->symbol fname) level (car field-class) src))
           
                (set-field-access-access! acc (make-var-access (memq 'static mods)
                                                               (memq 'final mods)
                                                               (field-record-init? record)
                                                               (cond
                                                                 (private? 'private)
                                                                 (public? 'public)
                                                                 (protected? 'protected)
                                                                 (else 'package))
                                                               (if local-inner-field-class?
                                                                   (inner-rec-unique-name local-inner-field-class?)
                                                                   (car field-class))))
                (unless local-inner-field-class?
                  (add-required c-class (car field-class) (cdr field-class) type-recs))
                (unless (eq? level 'full)
                  (when (is-field-restricted? fname field-class)
                    (restricted-field-access-err (field-access-field acc) field-class src)))
                (make-type/env 
                 (if (eq? 'dynamic (field-record-type record)) (make-dynamic-val #f) (field-record-type record))
                 (if (type/env? obj-type/env) (type/env-e obj-type/env) env))))
             ((and (dynamic-val? record) (dynamic-val-type record))
              (set-field-access-access! acc (make-var-access #f #t #t 'public 'unknown))
              (make-type/env (field-contract-type (unknown-ref-access (dynamic-val-type record)))
                             obj-type/env))
             ((dynamic-val? record)
              (add-required c-class (scheme-record-name class-rec) 
                            (cons "scheme" (scheme-record-path class-rec)) type-recs)
              (set-field-access-access! acc (make-var-access #t #t #t 'public (scheme-record-name class-rec)))
              (make-type/env record (if obj (type/env-e obj-type/env) env)))
             (else 
              (error 'internal-error "field-access given unknown form of field information")))))
        ((local-access? acc) 
         (let ((var (lookup-var-in-env (id-string (local-access-name acc)) env)))
           (unless (properties-usable? (var-type-properties var))
             (unusable-var-error (string->symbol (var-type-var var)) (id-src (local-access-name acc))))

           (when (and (eq? level 'beginner)
                      (not interact?)
                      (properties-field? (var-type-properties var)))
             (beginner-field-access-error (string->symbol (var-type-var var))
                                          (id-src (local-access-name acc))))
           (unless interact?
             (unless assign-left?
               (unless (properties-parm? (var-type-properties var))
                 (unless (var-set? (var-type-var var) env)
                   (unset-var-error (string->symbol (var-type-var var)) (id-src (local-access-name acc)))))))
           (make-type/env (if (eq? 'dynamic (var-type-type var))
                              (make-dynamic-val #f)
                              (var-type-type var))
                          env)))
        
        (else
         (let* ((first-acc (id-string (car acc)))
                (first-binding (lookup-var-in-env first-acc env))
                (new-acc
                 (cond
                   ((and (eq? level 'full) (not first-binding) (> (length acc) 1))
                    (let* ((static-class (find-static-class acc level type-recs))
                           (accs (cadr static-class)))
                      (build-field-accesses 
                       (make-access #f (expr-src exp)
                                    (make-field-access 
                                     #f
                                     (car accs)
                                     (make-var-access #t #f #f 'temp 
                                                      (if (class-record? (car static-class))
                                                          (class-record-name (car static-class))
                                                          (cons (scheme-record-name (car static-class))
                                                                (cons "scheme"
                                                                      (scheme-record-path (car static-class))))))))
                       (cdr accs))))
                   ((and (memq level '(beginner intermediate intermediate+access advanced)) (not first-binding) (> (length acc) 1)
                         (with-handlers ((exn:fail:syntax? (lambda (e) #f)))
                           (type-exists? first-acc null c-class (id-src (car acc)) level type-recs)))
                    (build-field-accesses
                     (make-access #f
                                  (expr-src exp)
                                  (make-field-access #f
                                                     (cadr acc)
                                                     (make-var-access #t #f #f 'temp first-acc)))
                     (cddr acc)))
                   ((and first-binding (not (properties-field? (var-type-properties first-binding))))
                    (build-field-accesses
                     (make-access #f (expr-src exp) (make-local-access (car acc)))
                     (cdr acc)))
                   (first-binding
                    (let* ((encl-depth (lookup-containing-class-depth (id-string (car acc)) env))
                           (encl-type (unless (or interact? static?)
                                        (if (= encl-depth 0) 
                                            (var-type-type (lookup-var-in-env "this" env))
                                            (var-type-type (lookup-var-in-env (format "encl-this-~a" encl-depth) env)))))
                           (encl-class (if static?
                                           c-class
                                           (unless interact?
                                             (cons (ref-type-class/iface encl-type) (ref-type-path encl-type))))))
                      (if (properties-static? (var-type-properties first-binding))
                          (build-field-accesses
                           (make-access #f (expr-src exp)
                                        (make-field-access #f
                                                           (car acc)
                                                           (make-var-access #t #f #f 'temp encl-class)))
                           (cdr acc))
                          (if interact?
                              (build-field-accesses (make-access #f (expr-src exp) (make-local-access (car acc)))
                                                    (cdr acc))
                              (build-field-accesses
                               (make-access #f (expr-src exp)
                                            (make-field-access 
                                             (if (= encl-depth 0)
                                                 (make-special-name #f #f "this")
                                                 (make-access #f (expr-src exp) 
                                                              (make-local-access 
                                                               (make-id (format "encl-this-~a" encl-depth)
                                                                        (expr-src exp)))))
                                             (car acc)
                                             #f))
                               (cdr acc))))))
                   (else 
                    (let ((class? (member (id-string (car acc)) (send type-recs get-class-env)))
                          (method? (not (null? (get-method-records (id-string (car acc)) (lookup-this type-recs env) type-recs)))))
                      (cond
                        ((or class? method?)
                         (variable-not-found-error (if class? 'class-name 'method-name) (car acc) (id-src (car acc))))
                        ((close-to-keyword? (id-string (car acc)))
                         (close-to-keyword-error 'field (car acc) (id-src (car acc))))
                        ((and (not static?) (not interact?)
                              (get-field-record (id-string (car acc)) 
                                                (send type-recs get-class-record 
                                                      (var-type-type (lookup-var-in-env "this" env))) (lambda () #f)))
                         (access-before-define (string->symbol (id-string (car acc)))
                                               (id-src (car acc))))
                        (else
                         (variable-not-found-error 'not-found (car acc) (id-src (car acc))))))))))
           (set-access-name! exp new-acc)
           (check-e exp env))))))
  
  ;package-members? (list string) (list string) type-records -> bool
  (define (package-members? class1 class2 type-recs)
    (cond
      ((equal? (car class1) "scheme-interactions") 
       (equal? (send type-recs get-interactions-package) (cdr class2)))
      ((equal? (car class2) "scheme-interactions")
       (equal? (send type-recs get-interactions-package) (cdr class1)))
      (else (equal? (cdr class1) (cdr class2)))))
  
  ;; field-lookup: string type expression src symbol type-records -> (U field-record dynamic-val)
  (define (field-lookup fname obj-type obj src level type-recs)
    (let ((obj-src (expr-src obj))
          (name (string->symbol fname)))
      (cond
        ((reference-type? obj-type)
         (let ((obj-record (get-record (send type-recs get-class-record obj-type #f
                                             ((get-importer type-recs) obj-type type-recs level obj-src))
                                       type-recs)))
           (get-field-record fname obj-record
                             (lambda () 
                               (let* ((class? (member fname (send type-recs get-class-env)))
                                      (method? (not (null? (get-method-records fname obj-record type-recs)))))
                                 (field-lookup-error 
                                  (if class? 'class-name 
                                      (if method? 'method-name 'not-found)) name obj-type src))))))
        ((array-type? obj-type)
         (unless (equal? fname "length")
           (field-lookup-error 'array name obj-type src))
         (make-field-record "length" `(public) #f `(array) 'int))
        (else (field-lookup-error 'primitive name obj-type obj-src)))))
  
  ;; build-field-accesses: access (list id) -> field-access
  (define (build-field-accesses start accesses)
    (cond
      ((null? accesses) (access-name start))
      (else
       (build-field-accesses
        (make-access #f (expr-src start) 
                     (make-field-access start (car accesses) #f))
        (cdr accesses)))))
  
  ;;find-static-class: (list access) symbol type-recs -> (list class-record (list access))
  (define (find-static-class accs level type-recs)
    (let ((path (send type-recs lookup-path (id-string (car accs)) (lambda () #f))))
      (if path
          (list (let* ((name (cons (id-string (car accs)) path))
                       (record (get-record 
                                (send type-recs get-class-record name #f
                                      ((get-importer type-recs) name type-recs level (id-src (car accs))))
                                type-recs)))
                  record)
                (cdr accs))
          (let ((found? (find-static (list (car accs)) (cdr accs))))
            (if (car found?)
                (list (get-record (send type-recs get-class-record (car found?)) type-recs)
                      (cdr found?))
                (class-lookup-error (caadr found?) (id-src (car accs))))))))
    
  ;find-static: (list id) (list id) -> (list (U #f (list id)) (list string)))
  (define (find-static test-path remainder)
    (let ((string-path (map id-string test-path)))
      (cond
        ((null? (cdr remainder))
         (list #f (list (apply build-path string-path))))
        ((find-directory string-path) =>
         (lambda (directory)
           (if (class-exists? directory (id-string (car remainder)))
               (list (cdr remainder) (cons (id-string (car remainder)) string-path))
               (find-static (append string-path (list (id-string (car remainder))))
                            (cdr remainder)))))
      (else (list #f (apply build-path (append string-path (list (id-string (car remainder))))))))))
  
  ;find-directory: (list string) -> (U string bool)
  (define (find-directory path)
    (if (null? path)
        (build-path 'same)
        (let loop ((paths (get-classpath)))
          (cond
            ((null? paths) #f)
            ((directory-exists? (build-path (car paths)
                                            (apply build-path path)))
             (build-path (car paths) (apply build-path path)))
            (else (loop (cdr paths)))))))
  
  ;class-exists?: string string -> bool
  (define (class-exists? path class)
    (or (file-exists? (string-append (build-path path class) ".java"))
        (file-exists? (string-append (build-path path "compiled" class) ".jinfo"))))
  
  ;check-special-name: expression env bool bool-> type
  (define (check-special-name exp env static? interact?)
    (when static? 
      (special-error (expr-src exp) interact?))
    (var-type-type (lookup-var-in-env "this" env)))
  
  ;check-specified-this: expression env bool bool -> type
  (define (check-specified-this exp env static? interact? level type-recs)
    (when static?
      (special-error (expr-src exp) interact?))
    (let ((var (lookup-enclosing-this (specified-this-class exp) env level type-recs)))
      (set-specified-this-var! exp (var-type-var var))
      (var-type-type var)))
    
  ;check-args: (list exp) (expr env -> type/env) env -> (list (list type) env)
  (define (check-args args check-e env)
    (let loop ((args args) (arg-types null) (env env))
      (cond
        ((null? args) (list (reverse arg-types) env))
        (else 
         (let ((arg/env (check-e (car args) env)))
           (loop (cdr args) (cons (type/env-t arg/env) arg-types) (type/env-e arg/env)))))))
    
  ;; 15.12
  ;check-call: exp (list exp) (expr env ->type/env) (list string) symbol env type-records bool bool-> type/env
  (define (check-call call arg-exps check-sub c-class level env type-recs ctor? static? interact?)
    (let* ((this (unless static? (lookup-this type-recs env)))
           (src (expr-src call))
           (name (call-method-name call))
           (name-string (when (id? name) (id-string name)))
           (expr (call-expr call))
           (exp-type #f)
           (handle-call-error 
            (lambda (exn)
              (when (not (access? expr)) (raise exn))
              (when (or (field-access? (access-name expr)) (local-access? (access-name expr))) (raise exn))
              (if (eq? level 'full)
                  (let ((record (car (find-static-class (append (access-name expr) (list name))
                                                        level type-recs))))
                    (set-call-expr! call #f)
                    (cond
                      ((class-record? record)
                       (unless (equal? (class-record-name record) c-class)
                         (send type-recs add-req (make-req (car (class-record-name record))
                                                           (if (null? (cdr (class-record-name record)))
                                                               (send type-recs lookup-path 
                                                                     (car (class-record-name record))
                                                                     (lambda () null))
                                                               (cdr (class-record-name record))))))
                       (get-method-records name-string record type-recs))
                      ((scheme-record? record)
                       (module-has-binding? record  name-string 
                                            (lambda () (no-method-error 'class 'not-found
                                                                        (string->symbol
                                                                         (scheme-record-name record))
                                                                        name
                                                                        src)))
                       (send type-recs add-req (make-req (scheme-record-name record)
                                                         (cons "scheme" (scheme-record-path record))))
                       (cond
                         ((name? name) (set-id-string! (name-id name) (java-name->scheme name-string)))
                         ((id? name) (set-id-string! name (java-name->scheme name-string))))
                       (list (make-method-contract (java-name->scheme name-string) #f #f 
                                                   (scheme-record-name record))))))
                  ;Teaching languages
                  (if (and (= (length (access-name expr)) 1)
                           (with-handlers ((exn:fail:syntax? (lambda (exn) #f)))
                             (type-exists? (id-string (car (access-name expr)))
                                           null c-class
                                           (id-src (car (access-name expr)))
                                           level type-recs)))
                      (let ((record (send type-recs get-class-record (list (id-string (car (access-name expr)))))))
                        (set-call-expr! call #f)
                        (cond 
                          ((class-record? record)
                           (unless (equal? (class-record-name record) c-class)
                             (send type-recs add-req (make-req (car (class-record-name record))
                                                               (send type-recs lookup-path 
                                                                     (car (class-record-name record))
                                                                     (lambda () null)))))
                           (let ((methods (get-method-records name-string record type-recs)))
                             (unless (andmap (lambda (x) x) 
                                             (map (lambda (mrec) (memq 'static (method-record-modifiers mrec)))
                                                  methods))
                               (class-as-object-call level (id-string (car (access-name expr))) name (id-src name)))
                             methods))
                          ((scheme-record? record) (raise exn))))
                      (raise exn)))))
           (methods 
            (cond 
              ((special-name? name)
               (let ((n (special-name-name name)))
                 (unless ctor? (illegal-ctor-call n src level))
                 (if (string=? n "super")
                     (let ((parent (car (class-record-parents this))))
                       (get-method-records (car parent)
                                           (get-record (send type-recs get-class-record parent) type-recs) type-recs))
                     (get-method-records (car (class-record-name this)) this type-recs))))
              (else
               (cond
                 ((and (special-name? expr) (equal? (special-name-name expr) "super"))
                  (when static?
                    (super-special-error (expr-src expr) interact?))
                  (let ((parent (car (class-record-parents this))))
                    (set! exp-type 'super)
                    (get-method-records name-string
                                        (send type-recs get-class-record parent) type-recs)))
                 (expr
                  (let* ((call-exp/env 
                          (with-handlers ((exn:fail:syntax? handle-call-error))
                            (check-sub expr env)))
                         (call-exp 
                          (if (type/env? call-exp/env) 
                              (begin (set! env (type/env-e call-exp/env))
                                     (type/env-t call-exp/env))
                              call-exp/env)))
                    (cond
                      ;List of methods found
                      ((list? call-exp) call-exp)
                      ((eq? call-exp 'null)
                       (prim-call-error call-exp name src level))
                      ((array-type? call-exp)
                       (set! exp-type call-exp)
                       (get-method-records name-string
                                           (send type-recs get-class-record object-type) type-recs))
                      ((dynamic-val? call-exp) 
                       (let ((m-contract (make-method-contract name-string #f #f #f)))
                         (set-dynamic-val-type! call-exp (make-unknown-ref m-contract))
                         (set! exp-type call-exp)
                         (list m-contract)))
                      ((reference-type? call-exp)
                       (set! exp-type call-exp)
                       (get-method-records name-string
                                           (get-record 
                                            (send type-recs get-class-record call-exp #f
                                                  ((get-importer type-recs) 
                                                   (cons (ref-type-class/iface call-exp) (ref-type-path call-exp))
                                                   type-recs level src))
                                            type-recs) type-recs))
                      (else (prim-call-error call-exp name src level)))))
                 (else 
                  (if (and (eq? level 'beginner) (not interact?))
                      (beginner-method-access-error name (id-src name))
                      (let ((rec (if static? (send type-recs get-class-record c-class) this)))
                        (cond 
                          ((and (dynamic?) (lookup-var-in-env name-string env)) =>
                           (lambda (var-type)
                             (if (eq? 'dynamic (var-type-type var-type))
                                 (list (make-method-contract (string-append name-string "~f") #f #f #f))
                                 null)))
                          ((null? rec) null)
                          (else (get-method-records name-string rec type-recs)))))))))))
      
      (when (null? methods)
        (let* ((rec (if exp-type 
                        (send type-recs get-class-record exp-type)
                        (if static? (send type-recs get-class-record c-class) this)))
               (class? (member (id-string name) (send type-recs get-class-env)))
               (field? (cond
                         ((array-type? exp-type) (equal? (id-string name) "length"))
                         ((null? rec)
                          (member name-string 
                                  (map field-record-name (send type-recs get-interactions-fields))))
                         (else (member name-string (map field-record-name (get-field-records rec))))))
               (sub-kind (if class? 'class-name (if field? 'field-name 'not-found))))
        (cond 
          ((eq? exp-type 'super) (no-method-error 'super sub-kind exp-type name src))
          (exp-type (no-method-error 'class sub-kind exp-type name src))
          (else 
           (cond
             ((close-to-keyword? name-string)
              (close-to-keyword-error 'method name src))
             (interact? 
              (if (or class? field?)
                  (no-method-error 'interact sub-kind exp-type name src)
                  (interaction-call-error name src level)))
             (else
              (no-method-error 'this sub-kind exp-type name src)))))))
      
      (unless (method-contract? (car methods))
        (when (and (not ctor?)
                   (eq? (method-record-rtype (car methods)) 'ctor))
          (ctor-called-error exp-type name src)))
      
      (let* ((args/env (check-args arg-exps check-sub env))
             (args (car args/env))
             (method-record
              (cond
                ((method-contract? (car methods))
                 (set-method-contract-args! (car methods) args)
                 (set-method-contract-return! (car methods) (make-dynamic-val #f))
                 (car methods))
                ((memq level '(full advanced))
                 (resolve-overloading methods 
                                      args
                                      (lambda () (call-arg-error 'number name args exp-type src))
                                      (lambda () (call-arg-error 'conflict name args exp-type src))
                                      (lambda () (call-arg-error 'no-match name args exp-type src))
                                      type-recs))
                ((> (length methods) 1)
                 (let ((teaching-error
                        (lambda (kind)
                          (if (error-file-exists? (method-record-class (car methods)) type-recs)
                              (call-provided-error name-string args kind)
                              (teaching-call-error kind #f name args exp-type src methods)))))
                   (resolve-overloading methods
                                        args
                                        (lambda () (teaching-error 'number))
                                        (lambda () (teaching-error 'type))
                                        (lambda () (teaching-error 'type))
                                        type-recs)))
                (else
                 (when 
                     (check-method-args args (method-record-atypes (car methods)) name exp-type src type-recs)
                   (car methods)))))
             (mods (when (method-record? method-record) (method-record-modifiers method-record))))
        
        (cond 
          ((method-record? method-record)        
           (when (and static? (not (memq 'static mods)) (not expr))
             (non-static-called-error name c-class src level))
           
           (when (and (memq 'abstract mods)
                      (special-name? expr)
                      (equal? "super" (special-name-name expr)))
             (call-abstract-error level name exp-type src))
           
           (when (and (memq 'protected mods)
                      (reference-type? exp-type))
             (unless (or (is-eq-subclass? this exp-type type-recs)
                         (let* ((e-class (ref-type-class/iface exp-type))
                                (e-path (ref-type-path exp-type))
                                (true-path (if (null? e-path)
                                               (send type-recs lookup-path e-class (lambda () null))
                                               e-path)))
                           #;(printf "~a ~a ~a~n" c-class (cons e-class true-path)
                                   (send type-recs get-interactions-package))
                           (package-members? c-class 
                                             (cons e-class true-path)
                                             type-recs)))
               (call-access-error 'pro level name exp-type src)))
           (when (and (memq 'private mods)
                      (reference-type? exp-type)
                      (if static?
                          (not (and (equal? (ref-type-class/iface exp-type) (car c-class))
                                    (equal? (ref-type-path exp-type) (cdr c-class))))
                          (not (eq? this (send type-recs get-class-record exp-type)))))
             (call-access-error 'pri level name exp-type src))
           (when (and (not (memq 'private mods)) (not (memq 'public mods)) 
                      (not (memq 'protected mods)) (reference-type? exp-type)
                      (not (package-members? c-class (cons (ref-type-class/iface exp-type) 
                                                           (ref-type-path exp-type)) type-recs)))
             (call-access-error 'pac level name exp-type src))
           (when (eq? level 'full)
             (for-each (lambda (thrown)
                         (unless (lookup-exn thrown env type-recs level)
                           (thrown-error (ref-type-class/iface thrown) name exp-type src)))
                       (method-record-throws method-record)))
           (when (and (eq? level 'beginner)
                      (eq? 'void (method-record-rtype method-record)))
             (beginner-call-error name src))
           (unless (eq? level 'full)
             (when (and (id? name) (is-method-restricted? name-string (method-record-class method-record)))
               (restricted-method-call name (method-record-class method-record) src)))
           (set-call-method-record! call method-record)
           (make-type/env 
            (if (eq? 'dynamic (method-record-rtype method-record))
                (make-dynamic-val #f)
                (method-record-rtype method-record))
            (cadr args/env)))
          ((method-contract? method-record)
           (set-call-method-record! call method-record)
           (make-type/env (method-contract-return method-record) (cadr args/env)))))))
    
  ;close-to-keyword: string -> bool
  (define (close-to-keyword? str)
    (let ((s (string-copy str)))
      (string-lowercase! s)
      (member s `("if" "return"))))
  
  (define (error-file-exists? class type-recs) #f)
  (define (call-provided-error) null)
  
  ;check-method-args: (list type) (list type) id type src type-records -> void
  (define (check-method-args args atypes name exp-type src type-recs)
    (let ((bad-length? (not (= (length args) (length atypes)))))
      (cond
        ((and (special-name? name) (not (expr-src name)) bad-length?)
         (method-arg-error 'implicit-ctor args atypes name exp-type src))
        (bad-length?
         (method-arg-error 'number args atypes name exp-type src))
        (else
         (for-each (lambda (arg atype)
                     (unless (assignment-conversion atype arg type-recs)
                       (method-arg-error 'type (list arg) (cons atype atypes) name exp-type src)))
                   args atypes)))))
  
  ;find-class: string rec-type env type-records -> (values boolean type record)
  #;(define (find-class name this env type-recs)
    (let ((local-inner? (lookup-local-inner name env))
          ...)))
    
  
  ;; 15.9
  ;;check-class-alloc: expr (U name identifier) (list exp) (exp env -> type/env) src type-records 
  ;                   (list string) env symbol bool bool-> type/env
  (define (check-class-alloc exp name/def arg-exps check-e src type-recs c-class env level static? interact?)
    (let* ((args/env (check-args arg-exps check-e env))
           (args (car args/env))
           (name (cond
                   ((def? name/def)
                    (check-inner-def name/def level type-recs c-class env)
                    (make-name (def-name name/def) null (id-src (def-name name/def))))
                   ((id? name/def) (make-name name/def null (id-src name/def)))
                   (else name/def)))
           (inner-lookup? (lookup-local-inner (id-string (name-id name)) env))
           (type (if inner-lookup?
                     (make-ref-type (inner-rec-unique-name inner-lookup?) (inner-rec-package inner-lookup?))
                     (name->type name c-class (name-src name) level type-recs)))
           (class-record 
            (if inner-lookup?
                (inner-rec-record inner-lookup?)
                (get-record (send type-recs get-class-record type c-class) type-recs)))
           (methods (get-method-records (if inner-lookup?
                                            (id-string (name-id name)) 
                                            (car (class-record-name class-record)))
                                        class-record type-recs)))
      (unless (equal? (car (class-record-name class-record))
                      (id-string (name-id name)))
        (set-id-string! (name-id name) (car (class-record-name class-record))))
      (unless (or (equal? (car (class-record-name class-record)) (ref-type-class/iface type)))
        (set-id-string! (name-id name) (car (class-record-name class-record)))
        (set-class-alloc-class-inner?! exp #t))
      (when inner-lookup?
        (set-id-string! (name-id name) (inner-rec-unique-name inner-lookup?))
        (set-class-alloc-local-inner?! exp #t))
      (unless (or (equal? (ref-type-class/iface type) (car c-class))
                  (equal? (car (class-record-name class-record))
                          (format "~a.~a" (car c-class) (id-string (name-id name))))
                  (class-alloc-class-inner? exp)
                  (class-alloc-local-inner? exp)
                  (inner-alloc? exp))
        (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
      (when (memq 'abstract (class-record-modifiers class-record))
        (class-alloc-error 'abstract type (name-src name)))
      (unless (class-record-class? class-record)
        (class-alloc-error 'interface type (name-src name)))
      (let* ((const (if (memq level `(full advanced))
                        (resolve-overloading methods 
                                             args
                                             (lambda () (ctor-overload-error 'number type args src))
                                             (lambda () (ctor-overload-error 'conflict type args src))
                                             (lambda () (ctor-overload-error 'no-match type args src))
                                             type-recs)
                        (if (> (length methods) 1)
                            (let ((teaching-error
                                   (lambda (kind)
                                     (if (error-file-exists? class-record type-recs)
                                         (call-provided-error (id-string (name-id name)) args kind)
                                         (teaching-call-error kind #t (name-id name) args #f src methods)))))
                              (resolve-overloading methods
                                                   args
                                                   (lambda () (teaching-error 'number))
                                                   (lambda () (teaching-error 'type))
                                                   (lambda () (teaching-error 'type))
                                                   type-recs))
                            (when (check-ctor-args args (method-record-atypes (car methods)) type src level type-recs)
                              (car methods)))))
             (mods (method-record-modifiers const))
             (this (if static? 
                       class-record
                       (lookup-this type-recs env))))
        (when (eq? level 'full)
          (for-each (lambda (thrown)
                      (unless (lookup-exn thrown env type-recs level)
                        (ctor-throws-error (ref-type-class/iface thrown) type src)))
                    (method-record-throws const)))
        (when (and (memq 'private mods) (or interact? (not (eq? class-record this))))
          (class-access-error 'pri level type src))
        (when (and (memq 'protected mods) (or (not (is-eq-subclass? this type type-recs)) 
                                              (not (package-members? c-class (cons (ref-type-class/iface type) 
                                                                                   (ref-type-path type)) type-recs))))
          (class-access-error 'pro level type src))
        (when (and (not (or (memq 'private mods) (memq 'protected mods) (memq 'public mods)))
                   (not (package-members? c-class 
                                          (cons (ref-type-class/iface type) 
                                                (if (null? (ref-type-path type))
                                                    (send type-recs lookup-path (ref-type-class/iface type)
                                                          (lambda () null))
                                                    (ref-type-path type)))
                                          type-recs)))
          (class-access-error 'pac level type src))
        ((if (class-alloc? exp) set-class-alloc-ctor-record! set-inner-alloc-ctor-record!)exp const)
        (make-type/env type (cadr args/env)))))
  
  ;check-inner-alloc: exp exp id (list exp) (exp env -> type/env) src type-records (list string) 
  ;                   env symbol bool -> type/env
  (define (check-inner-alloc exp obj name args check-e src type-recs c-class env level static?)
    (let* ((obj-type/env (check-e obj env))
           (obj-type (type/env-t obj-type/env))
           (cur-env (type/env-e obj-type/env))
           (class-rec (send type-recs get-class-record obj-type)))
      (unless (ref-type? obj-type) (inner-on-non-obj obj-type src))
      (unless (member (id-string name) 
                      (map bytes->string/locale (map inner-record-name (class-record-inners class-rec))))
        (check-inner-error obj-type name src))
      (set-id-string! name (string-append (ref-type-class/iface obj-type) "." (id-string name)))
      (check-class-alloc exp (make-name name null (id-src name)) args check-e src type-recs c-class env level static?)))
  
  (define (inner-on-non-obj type src)
    (let ((t (type->ext-name type)))
      (raise-error t
                   (format "class invocation cannot be preceeded by non-object value ~a" t)
                   t src)))
  
  (define (check-inner-error type name src)
    (let ((t (type->ext-name type))
          (n (id->ext-name name)))
      (raise-error n
                   (format "class ~a does not contain an inner class ~a" t n)
                   t src)))

  ;check-ctor-args: (list type) (list type) type src symbol type-records -> void
  (define (check-ctor-args args atypes name src level type-recs)
    (unless (= (length args) (length atypes))
      (ctor-arg-error 'number args atypes name src))
    (for-each (lambda (arg atype)
                (unless (assignment-conversion atype arg type-recs)
                  (ctor-arg-error 'type (list arg) (cons atype atypes) name src)))
              args atypes))
  
  ;; 15.10
  ;;check-array-alloc: type-spec (list exp) int src (expr env ->type/env) env 
  ;                    symbol (list string) type-records -> type/env
  (define (check-array-alloc elt-type exps dim src check-sub-exp env level c-class type-recs)
    (send type-recs add-req (make-req 'array null))
    (let* ((inner-lookup? 
            (and (name? (type-spec-name elt-type))
                 (lookup-local-inner (id-string (name-id (type-spec-name elt-type))) env)))
           (type 
            (if inner-lookup?
                (if (> (type-spec-dim elt-type) 0)
                    (make-array-type
                     (make-ref-type (inner-rec-unique-name inner-lookup?) (inner-rec-package inner-lookup?))
                     (type-spec-dim elt-type))
                    (make-ref-type (inner-rec-unique-name inner-lookup?) (inner-rec-package inner-lookup?)))
                (type-spec-to-type elt-type c-class level type-recs))))
      (when (and (ref-type? type) (not inner-lookup?))
        (add-required c-class (ref-type-class/iface type) (ref-type-path type) type-recs))
      (set-type-spec-dim! elt-type (+ (length exps) dim))
      (let loop ((subs exps) (env env))
        (cond
          ((null? subs)
           (make-type/env (make-array-type type (+ (length exps) dim)) env))
          (else
           (let* ((t/env (check-sub-exp (car subs) env))
                  (t (type/env-t t/env)))
             (when (and (dynamic-val? t) (not (dynamic-val-type t)))
               (set-dynamic-val-type! t 'int))
             (unless (prim-integral-type? t)
               (array-size-error type t (expr-src (car subs))))
             (loop (cdr subs) (type/env-e t/env))))))))
  
  ;;15.10
  ;;check-array-alloc-init: type-spec int array-init src (expr env->type/env) env symbol 
  ;;                        (list string) type-records -> type/env
  (define (check-array-alloc-init elt-type dim init src check-sub-exp env level c-class type-recs)
    (send type-recs add-req (make-req 'array null))
    (let* ((inner-lookup? 
            (and (name? (type-spec-name elt-type))
                 (lookup-local-inner (id-string (name-id (type-spec-name elt-type))) env)))
           (type 
            (if inner-lookup?
                (if (> (type-spec-dim elt-type) 0)
                    (make-array-type
                     (make-ref-type (inner-rec-unique-name inner-lookup?) (inner-rec-package inner-lookup?))
                     (type-spec-dim elt-type))
                    (make-ref-type (inner-rec-unique-name inner-lookup?) (inner-rec-package inner-lookup?)))
                (type-spec-to-type elt-type c-class level type-recs)))
           (a-type/env (check-array-init (array-init-vals init) check-sub-exp env type type-recs))
           (a-type (type/env-t a-type/env)))
      (when (and (ref-type? type) (not inner-lookup?))
        (add-required c-class (ref-type-class/iface type) (ref-type-path type) type-recs))
      (unless (= (array-type-dim a-type) dim)
        (array-dim-error type dim (array-type-dim a-type) src))
      (make-type/env (make-array-type type dim) (type/env-e a-type/env))))
  
  ;; 15.25
  ;check-cond-expr: type/env exp exp (exp env -> type/env) src src symbol type-records -> type/env
  (define (check-cond-expr test/env then-e else-e check-e src test-src level type-recs)
    (let* ((test (type/env-t test/env))
           (then/env (check-e then-e (type/env-e test/env)))
           (else/env (check-e else-e (type/env-e test/env)))
           (then (type/env-t then/env))
           (else-t (type/env-t else/env)))
      (cond
        ((and (dynamic-val? test) (dynamic-val-type test))
         (unless (eq? 'boolean (dynamic-val-type test))
           (condition-error (dynamic-val-type test) test-src)))
        ((dynamic-val? test) (set-dynamic-val-type! test 'boolean))
        (else 
         (unless (eq? 'boolean test) (condition-error test test-src))))
      (make-type/env
       (cond
         ((and (or (dynamic-val? then) (dynamic-val? else-t))
               (or (eq? 'boolean then) (eq? 'boolean else-t)))
          (cond
            ((dynamic-val? then)
             (cond
               ((and (dynamic-val-type then) (eq? 'boolean (dynamic-val-type then))) 'boolean)
               (else (set-dynamic-val-type! then 'boolean) 'boolean)))
            ((dynamic-val? else-t)
             (cond
               ((and (dynamic-val-type else-t) (eq? 'boolean (dynamic-val-type else-t))) 'boolean)
               (else (set-dynamic-val-type! else-t 'boolean) 'boolean)))))
         ((and (dynamic-val? then) (dynamic-val? else-t)
               (not (dynamic-val-type then)) (not (dynamic-val-type else-t)))
          (make-dynamic-val #f))
         ((and (eq? 'boolean then) (eq? 'boolean else-t)) 'boolean)
         ((and (prim-numeric-type? then) (prim-numeric-type? else-t))
          ;; This is not entirely correct, but close enough due to using scheme ints
          (binary-promotion then else-t))
         ((and (eq? 'null then) (reference-type? else-t)) else-t)
         ((and (eq? 'null else-t) (reference-type? then)) then)
         ((and (reference-type? then) (reference-type? else-t))
          (if (assignment-conversion then else-t type-recs) 
              then
              (if (assignment-conversion else-t then type-recs)
                  else-t
                  (condition-mismatch-error then else-t src))))
         (else (condition-mismatch-error then else-t src)))
       (intersect-var-sets (type/env-e test/env) (type/env-e then/env) (type/env-e else/env)))))

  ;; 15.13
  ;check-array-access: type/env exp (exp env -> type/env) src -> type/env
  (define (check-array-access ref/env idx check-e src type-recs)
    (let* ((ref-type (type/env-t ref/env))
           (idx/env (check-e idx (type/env-e ref/env)))
           (idx-type (type/env-t idx/env)))
      (send type-recs add-req (make-req 'array null))
      (unless (array-type? ref-type)
        (illegal-array-access ref-type src))
      (when (or (not (prim-integral-type? idx-type))
                (not (eq? 'int (unary-promotion idx-type))))
        (array-access-error ref-type idx-type src))
      (make-type/env (if (= 1 (array-type-dim ref-type))
                         (array-type-type ref-type)
                         (make-array-type (array-type-type ref-type)
                                          (sub1 (array-type-dim ref-type))))
                     (type/env-e idx/env))))
  
  ;; 15.14 & 15.15
  ;;Skips checking of whether expr is variable or value, and whether that variable is final
  ;;check-pre-post-expr: type/env symbol src -> type/env
  (define (check-pre-post-expr type/env op src)
    (let ((type (type/env-t type/env)))
       (if (prim-numeric-type? type)
           type/env
           (unary-error op 'num type src))))
  
  ;; 15.15
  ;check-unary: type/env symbol src -> type/env
  (define (check-unary expr-type/env op src)
    (let ((expr-type (type/env-t expr-type/env)))
      (make-type/env
       (case op
         ((+ -)
          (if (prim-numeric-type? expr-type)
              (unary-promotion expr-type)
              (unary-error op 'num expr-type src)))
         ((~)
          (if (prim-integral-type? expr-type)
              (unary-promotion expr-type)
              (unary-error op 'int expr-type src)))
         ((!)
          (if (eq? 'boolean expr-type)
              'boolean
              (unary-error op 'bool expr-type src))))
       (type/env-e expr-type/env))))
  
  ;; 15.16
  ;check-cast: type/env type-spec src symbol (list string) type-records -> type/env
  (define (check-cast exp-type/env cast-type src level current-class type-recs)
    (let* ((exp-type (type/env-t exp-type/env))
           (env (type/env-e exp-type/env))
           (inner-lookup? 
            (and (name? (type-spec-name cast-type))
                 (lookup-local-inner (id-string (name-id (type-spec-name cast-type))) env)))
           (type 
            (if inner-lookup?
                (if (> (type-spec-dim cast-type) 0)
                    (make-array-type
                     (make-ref-type (inner-rec-unique-name inner-lookup?) (inner-rec-package inner-lookup?))
                     (type-spec-dim cast-type))
                    (make-ref-type (inner-rec-unique-name inner-lookup?) (inner-rec-package inner-lookup?)))
                (type-spec-to-type cast-type current-class level type-recs))))
      (when (and (reference-type? type) (not inner-lookup?))
        (unless (equal? (car current-class) (ref-type-class/iface type))
          (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type)))))
      (make-type/env
       (cond
         ((dynamic-val? exp-type) 
          (set-dynamic-val-type! exp-type type)
          type)
         ((eq? 'dynamic type) (make-dynamic-val #f))
         ((and (reference-or-array-type? exp-type) (reference-or-array-type? type))               
          (unless (castable? exp-type type type-recs) (cast-error 'incompatible exp-type type src))
          type)
         ((and (not (reference-type? exp-type)) (not (reference-type? type))) 
          (unless (or (and (prim-numeric-type? exp-type)
                           (prim-numeric-type? type)
                           (or (widening-prim-conversion exp-type type)
                               (widening-prim-conversion type exp-type)))
                      (and (eq? 'boolean type)
                           (eq? 'boolean exp-type)))
            (cast-error 'incompatible-prim exp-type type src))
          type)
         ((reference-type? exp-type) (cast-error 'from-prim exp-type type src))
         (else (cast-error 'from-ref exp-type type src)))
       (type/env-e exp-type/env))))

  ;; 15.20.2
  ;check-instanceof type/env type-spec src symbol (list string) type-records -> type/env
  (define (check-instanceof exp-type/env inst-type src level current-class type-recs)
    (let* ((exp-type (type/env-t exp-type/env))
           (env (type/env-e exp-type/env))
           (inner-lookup? 
            (and (name? (type-spec-name inst-type))
                 (lookup-local-inner (id-string (name-id (type-spec-name inst-type))) env)))
           (type 
            (if inner-lookup?
                (if (> (type-spec-dim inst-type) 0)
                    (make-array-type
                     (make-ref-type (inner-rec-unique-name inner-lookup?) (inner-rec-package inner-lookup?))
                     (type-spec-dim inst-type))
                    (make-ref-type (inner-rec-unique-name inner-lookup?) (inner-rec-package inner-lookup?)))
                (type-spec-to-type inst-type current-class level type-recs))))
      (when (and (ref-type? type) (not inner-lookup?))
        (unless (equal? (car current-class) (ref-type-class/iface type))
          (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type)))))
      (make-type/env 
       (cond 
         ((and (ref-type? exp-type) (ref-type? type)
               (or (is-eq-subclass? exp-type type type-recs)
                   (is-eq-subclass? type exp-type type-recs)
                   (implements? exp-type type type-recs)
                   (implements? type exp-type type-recs))) 'boolean)
         ((and (ref-type? type) (eq? 'null exp-type)) 'boolean)
         ((and (ref-type? exp-type) (ref-type? type))
          (instanceof-error 'not-related-type type exp-type src))
         ((ref-type? exp-type)
          (instanceof-error 'not-class type exp-type src))
         (else
          (cond
            ((memq level '(beginner intermediate intermediate+access)) (instanceof-error 'not-ref type exp-type src))
            ((and (array-type? exp-type) (array-type? type)
                  (= (array-type-dim exp-type) (array-type-dim type))
                  (or (assignment-conversion exp-type type type-recs))) 'boolean)
            ((dynamic-val? exp-type) 'boolean)
            ((and (array-type? exp-type) (array-type? type))
             (instanceof-error 'not-related-array type exp-type src))
            ((array-type? exp-type)
             (instanceof-error 'not-array type exp-type src))
            (else (instanceof-error 'not-reforarray type exp-type src)))))
       (type/env-e exp-type/env))))
  
  ;; 15.26
  ;; SKIP - doing the check for compound assignment
  ;check-assignment: symbol exp exp (exp env -> type/env) (exp env -> type/env) src bool bool string symbol type-records env -> type/env
  (define (check-assignment op l-exp r-exp check-l check-r src c-tor? static-init? c-class level type-recs env)
    (when (and (eq? level 'beginner) (not c-tor?)) (illegal-assignment src))
    (let* ((ltype/env (check-l l-exp env))
           (rtype/env (check-r r-exp (type/env-e ltype/env)))
           (ltype (type/env-t ltype/env))
           (rtype (type/env-t rtype/env)))
      (when (access? l-exp)
        (check-final l-exp c-tor? static-init? c-class env))
      (when (and (eq? level 'beginner) c-tor?
                 (access? l-exp) (field-access? (access-name l-exp))
                 (var-access-init? (field-access-access (access-name l-exp))))
        (ctor-illegal-assignment (field-access-field (access-name l-exp))
                                 (expr-src l-exp)))
      (make-type/env
       (case op
         ((=)
          (if (assignment-conversion ltype rtype type-recs)
              ltype
              (assignment-error op ltype rtype src)))
         ((+= *= /= %= -= <<= >>= >>>= &= ^= or=)
          (bin-op-type-check op ltype rtype src level type-recs)
          ltype))
       (if (and (access? l-exp)
                (local-access? (access-name l-exp)))
           (add-set-to-env (id-string (local-access-name (access-name l-exp)))
                           (type/env-e rtype/env))
           (type/env-e rtype/env)))))      
  
  ;check-final: expression bool bool string -> void
  (define (check-final expr ctor? static-init? c-class env)
    (let ((access (access-name expr))
          (class (car c-class)))
      (cond
        ((local-access? access)
         (let* ((name (local-access-name access))
                (properties (var-type-properties (lookup-var-in-env (id-string name) env)))
                (settable? (properties-settable? properties))
                (static? (properties-static? properties)))
           (when (properties-final? properties)
             (when (not (properties-field? properties)) (assign-final-error 'local name class))
             (cond
               ((and ctor? settable? (not static?)) (void))
               ((and ctor? settable? static?) (assign-final-error 'static-in-ctor name class))
               ((and ctor? (not settable?)) (assign-final-error 'cannot-set-ctor name class))
               ((and static-init? settable?) (void))
               ((and static-init? (not settable?)) (assign-final-error 'cannot-set-static name class))
               (else (assign-final-error (if static? 'static 'field) name class))))))
        ((field-access? access)
         (let* ((name (field-access-field access))
                (obj (field-access-object access))
                (v-acc (field-access-access access))
                (init? (var-access-init? v-acc))
                (static? (var-access-static? v-acc)))
           (when (var-access-final? v-acc)
             (if (and (or (this-expr? obj) (and static-init? (not obj))) 
                      (equal? (var-access-class v-acc) class))
                 (cond
                   ((and ctor? (not init?) (not static?)) (void))
                   ((and ctor? (not init?) static?) (assign-final-error 'static-in-ctor name class))
                   ((and ctor? init? static?) (assign-final-error 'static-ctor-already-set name class))
                   ((and ctor? init? (not static?)) (assign-final-error 'field-already-set name class))
                   ((and static-init? (not init?)) (void))
                   ((and static-init? init?) (assign-final-error 'static-already-set name class))
                   (else (assign-final-error (if static? 'static 'field) name class)))
                 (assign-final-error (if static? 'static 'field) name class))))))))
  
  ;this-expr: expr -> bool
  (define (this-expr? expr)
    (and (special-name? expr)
         (equal? "this" (special-name-name expr))))
  
  ;check-test-exprs: exp (exp env -> type/env) env symbol type-records -> type/env
  (define (check-test-exprs exp check-sub-expr env level type-recs)
    (cond
      ((check-expect? exp)
       (check-test-expect (check-expect-test exp)
                          (check-expect-actual exp)
                          (check-expect-range exp)
                          check-sub-expr
                          env
                          level
                          (check-expect-ta-src exp)
                          (expr-src exp)
                          type-recs))
      ((check-catch? exp)
       (check-test-catch (check-catch-test exp)
                         (check-catch-exn exp)
                         check-sub-expr
                         env
                         (expr-src exp)
                         type-recs))
      ((check-by? exp)
       (check-test-by exp
                      (check-by-test exp)
                      (check-by-actual exp)
                      (check-by-compare exp)
                      check-sub-expr
                      env
                      level
                      (expr-src exp)
                      type-recs))
      ((check-rand? exp)
       (check-test-rand (check-rand-test exp)
                        (check-rand-range exp)
                        check-sub-expr
                        env
                        level
                        (check-rand-ta-src exp)
                        type-recs))
      ((check-mutate? exp)
       (check-test-mutate (check-mutate-mutate exp)
                          (check-mutate-check exp)
                          check-sub-expr
                          env
                          (expr-src exp)
                          type-recs))))
  
  ;check-test-expr: exp exp (U #f exp) (exp env -> type/env) env symbol src src type-records-> type/env
  (define (check-test-expect test actual range check-e env level ta-src src type-recs)
    (let* ((test-te (check-e test env))
           (test-t (type/env-t test-te))
           (actual-te (check-e actual (type/env-e test-te)))
           (actual-t (type/env-t actual-te))
           (range-te (if range (check-e range (type/env-e actual-te)) actual-te))
           (range-t (when range (type/env-t range-te)))
           (res (make-type/env 'boolean (type/env-e range-te))))
      (when (eq? test-t 'void)
        (check-type-error 'void level test-t actual-t (expr-src test)))
      (when (eq? actual-t 'void)
        (check-type-error 'void level test-t actual-t (expr-src actual)))
      (when (and range (not (prim-numeric-type? range-t)))
        (check-range-error (expr-src range) range-t)) 
      (cond
        ((and (eq? 'boolean test-t)
              (eq? 'boolean actual-t)) res)
        ((and (prim-numeric-type? test-t)
              (prim-numeric-type? actual-t))
         (if (or (and (prim-integral-type? test-t)
                      (prim-integral-type? actual-t))
                 range)
             res
             (check-double-error test-t actual-t 
                                 (expr-src test) (expr-src actual))))
        ((and (memq level '(advanced full))
              (reference-type? test-t) (reference-type? actual-t))
         (cond
           ((castable? actual-t test-t type-recs) res)
           (else (check-type-error 'cast level test-t actual-t ta-src))))
        ((and (memq level '(advanced full))
              (or (array-type? test-t) (array-type? actual-t)))
         (cond
           ((castable? actual-t test-t type-recs) res)
           (else
            (check-type-error 'cast level test-t actual-t ta-src))))
        ((and (eq? level 'beginner) (reference-type? test-t) (reference-type? actual-t))
         (if (or (is-eq-subclass? actual-t test-t type-recs)
                 (implements? actual-t test-t type-recs))
             res
             (check-type-error 'iface level test-t actual-t ta-src)))
        ((and (reference-type? test-t) (reference-type? actual-t)) 
         (if (or (is-eq-subclass? actual-t test-t type-recs)
                 (implements? actual-t test-t type-recs))
             res
             (check-type-error 'subtype level test-t actual-t ta-src)))
        (else
         (check-type-error (if (memq level '(advanced full)) 'cast 'subtype)
                           level
                           test-t actual-t ta-src)))))
  
  ;check-test-catch: expr type-spec (expr env -> type-env) env src type-records -> type/env
  (define (check-test-catch test type check-e env src type-recs)
    (let ([catch-type (type-spec-to-type type #f 'full type-recs)])
      (unless (is-eq-subclass? catch-type throw-type type-recs)
        (check-catch-error catch-type (type-spec-src type)))
      (when (reference-type? catch-type)
        (send type-recs add-req (make-req (ref-type-class/iface catch-type) (ref-type-path catch-type))))
      (let* ([new-env (add-exn-to-env catch-type env)]
             [test-type (check-e test new-env)])
        (make-type/env 'boolean (restore-exn-env (type/env-e test-type) env)))))
      
  ;check-test-by: expr expr (U symbol id) (expr env -> type-env) env symbol src type-records -> type/env
  (define (check-test-by exp test actual by check-e env level src type-recs)
    (let* ([test-et (check-e test env)]
           [actual-et (check-e actual (type/env-e test-et))]
           [test-type (type/env-t test-et)]
           [actual-type (type/env-t actual-et)]
           [new-env (type/env-e actual-et)])
      (cond
        [(eq? '== by)
         (unless 
             (or (and (prim-numeric-type? test-type)
                      (prim-numeric-type? actual-type))
                 (and (boolean? test-type)
                      (boolean? actual-type))
                 (and
                  (reference-type? test-type)
                  (reference-type? actual-type)
                  (castable? actual-type test-type type-recs))
                 (and
                  (reference-type? test-type)
                  (reference-type? actual-type)
                  (castable? test-type actual-type type-recs)))
           (check-by-==-error test-type actual-type src))]
        [else
         (unless (and (reference-type? test-type) 
                      (reference-type? actual-type))
           (check-by-error 'not-obj test-type actual-type #f src))
         (unless (or (dynamic-val? test-type)
                     (eq? 'dynamic test-type))
           (let* ([class-rec (send type-recs get-class-record test-type)]
                  [methods (get-method-records by class-rec type-recs)])
             (cond
               [(null? methods)
                (check-by-error 'no-such-method test-type #f by src)]
               [else 
                (let ([meth (resolve-overloading methods 
                                                 (list actual-type)
                                                 (lambda ()
                                                   (check-by-error 'no-arg-count
                                                                   test-type #f by src))
                                                 (lambda ()
                                                   (check-by-error 'conflict
                                                                   test-type actual-type
                                                                   by src))
                                                 (lambda ()
                                                   (check-by-error 'no-match
                                                                   test-type actual-type
                                                                   by src))
                                                 type-recs)])
                  (when meth 
                    (unless (eq? (method-record-rtype meth) 'boolean)
                      (check-by-error 'not-boolean test-type actual-type by src))
                    (set-check-by-compare! exp meth)))])
             (make-type/env 'boolean new-env)))])))
  
  ;check-test-rand: exp exp (exp env -> type/env) env symbol src type-records -> type/env
  (define (check-test-rand actual expt-range check-e env level src type-recs)
    (let* ([actual-te (check-e actual env)]
           [actual-t (type/env-t actual-te)]
           [expt-range-te (check-e expt-range (type/env-e actual-te))]
           [er-t (type/env-t expt-range-te)]
           [res (make-type/env 'boolean (type/env-e expt-range-te))])
      (when (eq? actual-t 'void)
        (check-rand-type-error 'void level actual-t er-t (expr-src actual)))
      (when (eq? er-t 'void)
        (check-rand-type-error 'void level actual-t er-t (expr-src expt-range)))
      (when (not (array-type? er-t))
        (check-rand-type-error 'not-array level actual-t er-t (expr-src expt-range)))
      (let ([er-a-t
             (cond
               [(eq? (array-type-dim er-t) 1) (array-type-type er-t)]
               [else (make-array-type (array-type-type er-t) (sub1 (array-type-dim er-t)))])])
        (cond
          ((and (eq? 'boolean actual-t)
                (eq? 'boolean er-a-t)) res)
          ((and (prim-numeric-type? actual-t)
                (prim-numeric-type? er-a-t))
           res)
          ((and (memq level '(advanced full))
                (reference-type? actual-t) (reference-type? er-a-t))
           (cond
             ((castable? er-a-t actual-t type-recs) res)
             (else (check-rand-type-error 'cast level actual-t er-a-t src))))
          ((and (memq level '(advanced full))
                (or (array-type? actual-t) (array-type? er-a-t)))
           (cond
             ((castable? er-a-t actual-t type-recs) res)
             (else
              (check-rand-type-error 'cast level actual-t er-a-t src))))
          (else
           (check-rand-type-error (if (memq level '(advanced full)) 'cast 'subtype)
                                  level
                                  actual-t er-a-t src))))))
 
  
  ;check-test-mutate: exp exp (exp env -> type/env) env src type-records -> type/env
  (define (check-test-mutate mutatee check check-sub-expr env src type-recs)
    (unless (or (call? mutatee)
                (assignment? mutatee)
                (class-alloc? mutatee)
                (post-expr? mutatee)
                (pre-expr? mutatee))
      (check-mutate-kind-error (expr-src mutatee)))
    (let* ((mutatee-type (check-sub-expr mutatee env))
           (checker-type (check-sub-expr check (type/env-e mutatee-type))))
      (unless (eq? 'boolean (type/env-t checker-type))
        (check-mutate-check-error (type/env-t checker-type) (expr-src check)))
      (make-type/env 'boolean (type/env-e checker-type))))
  
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Expression Errors

  ;;Binop errors
  ;;bin-op-prim-error: symbol symbol symbol type type src -> void
  (define (bin-op-prim-error side op expect left right src)
    (let ((ext-out (get-expected expect))
          (rt (type->ext-name right))
          (lt (type->ext-name left))
          (op (if (eq? op 'oror) (string->symbol "||") op)))
      (raise-error
       op
       (case side
         ((right) (format "Right hand side of ~a should be of type ~a, but given ~a." op ext-out rt))
         ((left) (format "Left hand side of ~a should be of type ~a, but given ~a." op ext-out lt))
         (else (format "~a expects arguments of type ~a, but given ~a and ~a." op ext-out lt rt)))
       op src)))
  
  ;bin-op-beginner-error symbol type type src -> void
  (define (bin-op-beginner-error op left right src)
    (let ((rt (type->ext-name right))
          (lt (type->ext-name left)))
      (raise-error op
                   (format "~a only compares integer, character, or boolean values. ~a to ~a is not allowed"
                           op rt lt)
                   op src)))
  
  ;bin-op-equality-error symbol symbol type type src -> void
  (define (bin-op-equality-error type op left right src)
    (let ((rt (type->ext-name right))
          (lt (type->ext-name left)))
      (raise-error 
       op
       (case type
         ((both) 
          (format "~a expects one argument to be castable to the other, neither ~a nor ~a can be" op lt rt))
         (else 
          (format "~a expects its arguments to be equivalent types, given non-equivalent ~a and ~a" 
                  op lt rt)))
       op src)))

  ;bin-op-bitwise-error symbol type type src -> void
  (define (bin-op-bitwise-error op left right src)
    (let ((lt (type->ext-name left))
          (rt (type->ext-name right))
          (prim-list "long, int, short, byte or char")
          (op (if (eq? op 'or) (symbol->string "|") op)))
      (raise-error 
       op 
       (cond
         ((prim-integral-type? left) 
          (format "~a expects the right hand side to be a ~a when the left is ~a. Given ~a"
                  op prim-list lt rt))
         ((prim-integral-type? right)
          (format "~a expects the left hand side to be a ~a when the left is ~a. Given ~a"
                  op prim-list rt lt))
         ((eq? left 'boolean)
          (format "~a expects the right hand side to be a ~a when the left is ~a. Given ~a"
                  op "boolean" lt rt))
         ((eq? right 'boolean)
          (format "~a expects the left hand side to be a ~a when the right is ~a. Given ~a"
                  op "boolean" rt lt))
         (else
          (format "~a expects its arguments to both be either booleans, or ~a. Given ~a and ~a"
                  op prim-list lt rt)))
       op src)))

  ;;check-access errors
  
  ;variable-not-found-error: symbol id src -> void
  (define (variable-not-found-error kind var src)
    (let ((name (id->ext-name var)))
      (raise-error 
       name
       (case kind
         ((not-found) (format "Reference to undefined identifier ~a." name))
         ((class-name) (format "Class named ~a cannot be used as a variable, which is how it is used here." name))
         ((method-name) 
          (let ((line1
                 (format "Method named ~a cannot be used as a variable, which is how it is used here." name))
                (line2 "A call to a method should be followed by () and any arguments to the method"))
            (format "~a~n~a" line1 line2))))
       name src)))
  
  ;field-lookup-error: symbol symbol type src -> void
  (define (field-lookup-error kind field exp src)
    (let ((t (type->ext-name exp)))
      (raise-error
       field
       (case kind
         ((not-found) (format "Field ~a not found for object with type ~a." field t))
         ((class-name)
          (format "Class named ~a is being erroneously accessed as a field" field))
         ((method-name)
          (let ((line1
                 (format "Method ~a is being erroneously accessed as a field for class ~a." field t))
                (line2 "A call to a method should be followed by () and any arguments to the method"))
            (format "~a~n~a" line1 line2)))
         ((array)
          (format "~a only has a length field, attempted to access ~a" t field))
         ((primitive)
          (format "Attempted to access field ~a on ~a; this value does not have fields." field t)))
       field src)))

  ;unusable-var-error: symbol src -> void
  (define (unusable-var-error name src)
    (raise-error 
     name
     (format "Field ~a cannot be used in this class, as two or more parents contain a field with this name." name)
     name src))

  ;unset-var-error: symbol src -> void
  (define (unset-var-error name src)
    (raise-error name
                 (format "Local variable ~a was not set along all paths reaching this point, and cannot be used."
                         name)
                 name src))     
  ;access-before-defined: symbol src -> void
  (define (access-before-define name src)
    (raise-error name
                 (format "Field ~a cannot be accessed before its declaration." name)
                 name src))
  
  ;assign-twice: symbol src -> void
  (define (assign-twice name src)
    (raise-error name
                 (format "Field ~a has been initialized and cannot be initialized again." name)
                 name src))
  
  (define (access-before-assign name src)
    (raise-error name
                 (format "Field ~a cannot be accessed before it is initialized." name)
                 name src))
  
  ;not-static-field-access-error symbol symbol src -> void
  (define (not-static-field-access-error name level src)
    (raise-error 
     name
     (case level
       ((beginner intermediate intermediate+access) 
        (format "Field ~a cannot be retrieved from a class, ~a can only be accessed from an instance of the class."
                name name))
       ((advanced full)
        (format "Field ~a accessed as though static; ~a is not a static field" name name)))
     name src))
  
  ;beginner-field-access-error: symbol src -> void
  (define (beginner-field-access-error name src)
    (raise-error
     name
     (format "Field ~a from the current class accessed as a variable. Fields should be accessed with 'this'." name)
     name src))
  
  ;illegal-field-access: symbol symbol symbol string src -> void
  (define (illegal-field-access kind field level class src)
    (raise-error
     field
     (if (or (eq? kind 'private) (memq level '(beginner intermediate)))
         (format "field ~a not found for object with type ~a" field class)
         (case kind
           ((protected) 
            (format "field ~a of class ~a can only be accessed by ~a, subclasses of ~a, or fellow package members"
                    field class class class))
           ((package) (format "field ~a of class ~a can only be accessed by ~a and fellow package members of ~a" 
                       field class class class))))
     field src))
  
  ;restricted-field-access: id (list string) src -> void
  (define (restricted-field-access-err field class src)
    (let ((n (id->ext-name field)))
      (raise-error n (format "field ~a from ~a may not be used" n (car class))
                   n src)))
  
  ;;special-name errors
  ;special-error: src bool -> void
  (define (special-error src interactions?)
    (raise-error 'this 
                 (format "use of 'this' is not allowed in ~a"
                         (if interactions? "the interactions window" "static code"))
                 'this src))
  
  ;super-special-error: src bool -> void
  (define (super-special-error src interact?)
    (raise-error 'super
                 (format "use of 'super' is not allowed in ~a"
                         (if interact? "the interactions window" "static code"))
                 'super src))
  
  ;;Call errors

  ;prim-call-error type id src symbol -> void
  (define (prim-call-error exp name src level)
    (let ((n (id->ext-name name))
          (t (type->ext-name exp)))
      (raise-error
       n
       (if (eq? exp 'null)
           (format "Attempted to call method ~a directly on null. The null value does not have any methods." n)
           (format "Attempted to call method ~a on ~a which does not have methods. ~nOnly values with ~a types have methods"
                   n t
                   (case level 
                     ((beginner intermediate intermediate+access) "class or interface")
                     (else "class, interface, or array"))))
       n src)))
  
  ;no-method-error: symbol symbol type id src -> void
  (define (no-method-error kind sub-kind exp name src)
    (let ((t (type->ext-name exp))
          (n (id->ext-name name)))
      (raise-error 
       n
       (case sub-kind
         ((not-found) (format "~a does not contain a method named ~a"
                              (case kind
                                ((class) t)
                                ((super) "This class's super class")
                                ((this) "The current class"))
                              n))
         ((class-name)
          (let ((line1 
                 (format "Class ~a is inappropriately being used as a method." n))
                (line2
                 "Parenthesis typically follow the class name when creating an instance, perhaps 'new' was forgotten."))
          (format "~a~n~a" line1 line2)))
         ((field-name)
          (format 
           "Field ~a is being inappropriately used as a method, parentheses are not used in interacting with a field."
           n)))                     
       n src)))
  
  ;close-to-keyword-error: symbol id src -> void
  (define (close-to-keyword-error kind name src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (case kind
                     ((method) 
                      (string-append
                       (format "No method ~a for this call can be found. ~a resembles a reserved word.~n"
                               n n)
                       "Perhaps it is miscapitalized or misspelled"))
                     ((field)
                       (string-append
                        (format "This unknown variable, ~a, is similar to a reserved word.~n" n)
                        "Perhaps it is miscaptialzed or misspelled")))
                   n src)))

  ;class-as-object-call: level string id src -> void
  (define (class-as-object-call level class meth src)
    (let ((n (id->ext-name meth))
          (c (string->symbol class)))
      (raise-error n
                   (case level
                     ((beginner intermediate intermediate+access) (format "Attempt to use class or interface ~a as an object to call method ~a" c n))
                     ((advanced) (format "Attempt to use method ~a from class ~a as though it were static" n c)))
                   c src)))
  
  ;beginner-method-access-error: id src -> void
  (define (beginner-method-access-error name src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (format "A call to method ~a requires a target object, such as 'this'." n)
                   n src)))

  
  ;restricted-method-call id (list string) src -> void
  (define (restricted-method-call name class src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (format "Method ~a from ~a may not be called." n (car class))
                   n src)))
  
  ;ctor-called-error: type id src -> void
  (define (ctor-called-error exp name src)
    (let ((t (if exp (type->ext-name exp) "the current class"))
          (n (id->ext-name name)))
      (raise-error n
                   (format "Constructor ~a from ~a cannot be used as a method." n t)
                   n src)))
  
  ;non-static-called-error: id (list string) src bool -> void
  (define (non-static-called-error name class src level)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (if (memq level '(advanced full))
                       (format "Non-static method ~a from ~a cannot be called directly from a static context."
                               n (car class))
                       (format "Method ~a from ~a cannot be called here." n (car class)))
                   n src)))
  
  ;interaction-call-error
  (define (interaction-call-error name src level)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (string-append (format "Method ~a cannot be called in the interactions window.~n" n)
                                  (format "Only ~a methods or methods on objects may be called here." 
                                          (if (memq level '(beginner intermediate intermediate+access)) "certain library" "static")))
                   n src)))

  
  (define (illegal-ctor-call name src level)
    (let ((n (string->symbol name)))
      (raise-error n (format "Calls to ~a may only occur in ~a"
                             n
                             (if (memq level `(full advanced)) "other constructors" "the super constructor"))
                   n src)))

  ;method-arg-error symbol (list type) (list type) id type src -> void
  (define (method-arg-error kind args atypes name exp-type src)
    (let ((n (id->ext-name name))
          (e (get-call-type exp-type))
          (givens (map type->ext-name args))
          (expecteds (map type->ext-name atypes))
          (awitht "arguments with types"))
      (raise-error n
                   (case kind
                     ((implicit-ctor)
                      (format 
                       "This constructor must contain a call to the super class's constructor which expects ~a ~a ~a"
                       (length expecteds) awitht expecteds))
                      ((number)
                       (format "method ~a from ~a expects ~a ~a ~a. Given ~a ~a ~a"
                               n e (length expecteds) awitht expecteds (length givens) awitht givens))
                     ((type)
                      (format "method ~a from ~a expects ~a ~a, but given a ~a instead of ~a for one argument"
                              n e awitht (map type->ext-name (cdr atypes)) (car givens) (type->ext-name (car atypes)))))
                   n src)))

  ;teaching-call-error: symbol bool id (list type) type src (list method-record) -> void
  (define (teaching-call-error kind ctor? name args exp-type src methods)
    (let* ((method-args (map method-record-atypes (remove-overridden methods)))
           (non-array-type-list (filter (lambda (arg-list)
                                          (andmap (lambda (a) (not (array-type? a))) arg-list)) method-args))
           (predominant-number (get-most-occuring-length non-array-type-list))
           (type-lists (get-string-of-types (filter (lambda (a) (= (length a) predominant-number)) non-array-type-list))))
      (let* ((n (id->ext-name name))
             (e (get-call-type exp-type))
             (givens (get-string-of-types (list args)))
             (front (if ctor? "constructor for" (format "method ~a from" n))))
        (raise-error n
                     (case kind
                       ((number)
                        (format "~a ~a expects ~a arguments with type(s) ~a. Given ~a"
                                front e predominant-number type-lists givens))
                       (else
                        (format "~a ~a expects arguments with type(s) ~a. Given ~a"
                                front e type-lists givens)))
                     n src))))
  
  ;remove-overridden: (list method-record) -> (list method-record)
  (define (remove-overridden methods)
    (letrec ((remove?
              (lambda (method methods)
                  (and (not (null? methods))
                       (or (and
                            (= (length (method-record-atypes method))
                               (length (method-record-atypes (car methods))))
                            (andmap (lambda (x) x) (map type=? 
                                                        (method-record-atypes method) 
                                                        (method-record-atypes (car methods)))))
                           (remove? method (cdr methods))))))
             (remove
              (lambda (methods)
                (cond
                  ((null? methods) methods)
                  ((remove? (car methods) (cdr methods))
                   (remove (cdr methods)))
                  (else (cons (car methods) (remove (cdr methods))))))))
      (remove methods)))
  
  ;get-most-occuring-lenght: (list (list type)) -> number
  (define (get-most-occuring-length args)
    (let* ((lengths (map length args))
           (max-length (apply max lengths))
           (vec (make-vector (add1 max-length) 0))
           (loc 0))
      (let loop ((l lengths))
        (unless (null? l)
          (vector-set! vec (car l) (add1 (vector-ref vec (car l))))
          (loop (cdr l))))
      (let loop ((i 0) (max-loc 0) (max 0))
        (if (= i (vector-length vec))
            (set! loc max-loc)
            (if (> (vector-ref vec i) max)
                (loop (add1 i) i (vector-ref vec i))
                (loop (add1 i) max-loc max))))
      loc))
        
  ;get-string-of-types: (list (list type)) -> string
  (define (get-string-of-types types)
    (let ((out (if (= 1 (length (car types)))
                   (apply string-append 
                          (map (lambda (a) (format "~a, or " (type->ext-name (car a)))) types))
                   (apply string-append
                          (map (lambda (a) (format "(~a), or "
                                                   (let ((internal 
                                                          (apply string-append 
                                                                 (map (lambda (aI) 
                                                                        (format "~a, " (type->ext-name aI))) a))))
                                                     (if (< (string-length internal) 2)
                                                         internal
                                                         (substring internal 0 (- (string-length internal) 2))))))
                               types)))))
      (substring out 0 (- (string-length out) 5))))                                                         
  
  ;call-abstract-error: symbol id type src -> void
  (define (call-abstract-error level name exp src)
    (let ((n (id->ext-name name))
          (t (get-call-type exp)))
      (raise-error n
                   (if (memq level '(beginner))
                       (format "You maynot call method ~a from ~a" n t)
                       (format "super.~a(...) may not be called as ~a is abstract in ~a." n n t))
                   n src)))
  
  ;call-access-error: symbol symbol id type src -> void
  (define (call-access-error kind level name exp src)
    (let ((n (id->ext-name name))
          (t (get-call-type exp)))
    (raise-error n
                 (if (memq level '(beginner intermediate abstract))
                     (format "~a does not contain a method named ~a" t n)
                     (case kind
                       ((pro) 
                        (format "method ~a from ~a may only be called by ~a, a subclass, or package member of ~a" 
                                n t t t))
                       ((pri) (format "~a does not contain a visible method named ~a" t n))
                       ((pac) (format "method ~a from ~a may only be called by ~a or a package member of ~a" 
                                      n t t t))))
                 n src)))

  ;call-arg-error: symbol id (list type) type src -> void
  (define (call-arg-error kind name args exp src)
    (let* ((n (id->ext-name name))
           (t (get-call-type exp))
           (call-type (if (and (special-name? name)
                               (string=? "super" (special-name-name name)))
                          "super constructor for"
                          (format "method ~a from" n)))
           (as (map type->ext-name args)))
      (raise-error n
                   (case kind
                     ((number)
                      (format "~a ~a has no definition with ~a argument~a. Given ~a"
                              call-type t (length as) (if (> (length as) 1) "s" "") as))
                     ((no-match)
                      (format "~a ~a has no definition with compatible types as the given types: ~a"
                              call-type t as))
                     ((conflict)
                      (format "~a ~a has multiple compatible definitions with given arguments: ~a"
                              call-type t as)))
                   n src)))
  
  ;thrown-error: string id type src -> void
  (define (thrown-error thrown name exp src)
    (let ((n (id->ext-name name))
          (t (get-call-type exp)))
      (raise-error n
                   (format "called method ~a from ~a throws exception ~a, which is not caught or listed as thrown"
                           n t thrown)
                   n src)))
      
  ;beginner-call-error: id src -> void
  (define (beginner-call-error name src)
    (let ((n (id->ext-name name)))
      (raise-error n (format "method ~a cannot be called in ProfessorJ Beginner" n) n src)))
  
  ;;Class Alloc errors

  ;class-alloc-error: symbol type src -> void
  (define (class-alloc-error kind type src)
    (let ((cl (type->ext-name type)))
      (raise-error cl
                   (case kind
                     ((abstract) (format "class ~a is abstract. Abstract classes may not be instantiated." cl))
                     ((interface) (format "~a is an interface. interfaces may not be instantiated." cl)))
                   cl src)))

  ;ctor-arg-error symbol (list type) (list type) type src -> void
  (define (ctor-arg-error kind args atypes name src)
    (let ((n (type->ext-name name))
          (givens (get-string-of-types (list args)))
          (expecteds (get-string-of-types (list atypes)))
          (awitht "arguments with types"))
      (raise-error n
                   (case kind
                     ((number)
                      (format "Constructor for ~a expects ~a ~a ~a. Given ~a ~a ~a"
                              n (length atypes) awitht expecteds (length args) awitht givens))
                     ((type)
                      (format "Constructor for ~a expects ~a ~a, but given a ~a instead of ~a for one argument"
                              n awitht (get-string-of-types (list (cdr atypes))) (type->ext-name (car args))
                              (type->ext-name (car atypes)))))
                   n src)))

  ;ctor-overload-error: symbol type (list type) src -> void
  (define (ctor-overload-error kind name args src)
    (let ((n (type->ext-name name))
          (as (map type->ext-name args)))
      (raise-error 
       n
       (case kind
         ((number)
          (format "No constructor for ~a exists with ~a arguments. Given ~a"
                  n (length as) as))
         ((no-match)
          (format "No constructor for ~a exists with compatible types as the given types: ~a"
                  n as))
         ((conflict)
          (format "Multiple constructors for ~a exist with compatible definitions for the given arguments: ~a"
                  n as)))
       n src)))
  
  ;class-access-error: symbol type src -> void
  (define (class-access-error kind level name src)
    (let ((n (type->ext-name name)))
      (raise-error n
                   (case kind
                     ((pro) (format "This constructor for ~a may only be used by ~a and its subclasses~a"
                                    n n (if (memq level '(beginner intermediate)) "" " and package members")))
                     ((pri) (format "This constructor for ~a may only be used by ~a"
                                    n n))
                     ((pac) (format "This constructor for ~a may~a"
                                    n (if (memq level '(beginner intermediate))
                                          " not be used" (format " only be used by ~a and package members" n)))))
                 n src)))

  ;ctor-throws-error: string type src -> void
  (define (ctor-throws-error thrown name src)
    (let ((n (type->ext-name name)))
      (raise-error n
                   (format "Constructor for ~a throws exception ~a, which is not caught or listed as thrown"
                           n thrown)
                   n src)))
  
  ;;Array Alloc error
  ;array-size-error: type type src -> void
  (define (array-size-error array dim src)
    (let ((a (type->ext-name array))
          (d (type->ext-name dim)))
      (raise-error a
                   (format "Allocation of array of ~a requires an integer for the size. Given ~a"
                           a d)
                   a src)))

  ;Array Alloc Init error
  ;array-dim-error: type int int src -> void
  (define (array-dim-error type dim g-dim src)
    (let ((t (type->ext-name (make-array-type type dim)))
          (given (type->ext-name (make-array-type type g-dim))))
      (raise-error t
                   (format "Expected an array of type ~a~a, found an array of type ~a~a" t given)
                   t src)))
  
  ;;Conditional Expression errors

  ;condition-error: type src -> void
  (define (condition-error type src)
    (let ((t (type->ext-name type)))
      (raise-error '?
                   (format "? requires that the first expression have type boolean. Given ~a" t)
                   '? src)))

  ;condition-mismatch-error: type type src -> void
  (define (condition-mismatch-error then else src)
    (raise-error 
     '?
     (format 
      "? requires that the then and else branches have equivalent types: given ~a and ~a which are not equivalent"
      (type->ext-name then) (type->ext-name else))
     '? src))
    
  ;;Array Access errors
  ;illegal-array-access: type src -> void
  (define (illegal-array-access type src)
    (let ((n (type->ext-name type)))
      (raise-error 
       n
       (format "Expression of type ~a accessed as if it were an array, only arrays may be accessed with [N]" n)
       n src)))
  
  ;array-access-error: type type src -> void
  (define (array-access-error array idx src)
    (let ((n (type->ext-name array))
          (i (type->ext-name idx)))
      (raise-error n
                   (format "~a should be indexed with an integer, given ~a" n i)
                   n src)))
  
  ;;Unary error
  ;unary-error: symbol symbol type src -> void
  (define (unary-error op expect type src)
    (raise-error op
                 (format "~a expects a ~a, given ~a"
                         op (get-expected expect) (type->ext-name type))
                 op src))
  
  ;;Cast errors
  ;cast-error: symbol type type src -> void
  (define (cast-error kind cast exp src)
    (raise-error 
     'cast
     (case kind
       ((from-prim)
        (let ((line1 (format "Illegal cast from primitive, ~a, to class or interface ~a."
                             (type->ext-name exp) (type->ext-name cast)))
              (line2 "Non-class or interface types may not be cast to class or interface types"))
          (format "~a~n~a" line1 line2)))
       ((from-ref)
        (let ((line1 (format "Illegal cast from class or interface ~a to primitive, ~a."
                             (type->ext-name exp) (type->ext-name cast)))
              (line2 "Class or interface types may not be cast to non-class or interface types"))
          (format "~a~n~a" line1 line2)))
       ((incompatible)
        (format "Illegal cast from class or interface ~a to class or interface ~a, incompatible types"
                (type->ext-name exp) (type->ext-name cast)))
       ((incompatible-prim)
         (format "Illegal cast from ~a to ~a, incompatible types" (type->ext-name exp) (type->ext-name cast))))
     'cast src))
  
  ;;Instanceof errors
  ;instanceof-error: symbol type type src -> void
  (define (instanceof-error kind inst exp src)
    (let ((i (type->ext-name inst))
          (e (type->ext-name exp)))
      (raise-error 
       'instanceof
       (case kind
         ((not-related-type)
          (let ((line1 "instanceof requires that its expression be related to the given type")
                (line2 (format "~a is not a subtype of ~a, and ~a is not a subtype of ~a" e i i e)))
            (format "~a~n~a" line1 line2)))
         ((not-class)
          (format 
           "instanceof requires its expression to be compared to a class or interface: Given ~a which is neither"
           i))
         ((not-ref)
          (format "instanceof requires the expression, compared to ~a, to be a class or interface: Given ~a"
                  i e))
         ((not-related-array)
          (let ((line1 "instanceof requires that its expression be related to the given type")
                (line2 (format "~a is not a subtype of ~a, and ~a is not a subtype of ~a" e i i e)))
            (format "~a~n~a" line1 line2)))
         ((not-array)
          (format "instancof requires its expression to be an array when compared to ~a. Given ~a" i e))
         ((not-reforarray)
          (format "instanceof requires the expression, compared to ~a, to be a class, interface or array: Given ~a" i e)))
       'instanceof src)))
  
  ;;Assignment errors
  ;illegal-assignment: src -> void
  (define (illegal-assignment src)
    (raise-error '= "Assignment is only allowed in the constructor." '= src))
  
  ;ctor-illegal-assignment: id src -> void
  (define (ctor-illegal-assignment name src)
    (raise-error '= 
                 (format "Field ~a has already been initialized and cannot be reassigned."
                         (id->ext-name name))
                 '= src))
  
  (define (assignment-error op ltype rtype src)
    (raise-error op
                 (format "~a requires that the right hand type be equivalent to or a subtype of ~a: given ~a"
                         op (type->ext-name ltype) (type->ext-name rtype))
                 op src))
  
  (define (illegal-beginner-assignment)
    "Assignment statements are only allowed in constructors")
  (define (assignment-convert-fail op d ltype rtype)
    (format "~a requires that the right hand type be equivalent to or a subtype of ~a: given ~a" 
            op ltype rtype))

  ;assign-final-error: symbol id string -> void
  (define (assign-final-error kind name class)
    (let* ((n (id->ext-name name))
           (already-set 
            (lambda (static?) (format "final~afield ~a has already been set" (if static? " static " " ") n)))
           (in-ctor
            (lambda (static?) (format "final~afield ~a may not be set in ~a's constructor" 
                                      (if static? " static " " ") n class))))
      (raise-error n
                   (case kind
                     ((local) (format "final parameter ~a may not be set" n))
                     ((static-in-ctor) (in-ctor #t))
                     ((cannot-set-ctor) (in-ctor #f))
                     ((cannot-set-static) (format "final field ~a may not be set in ~a's static initialization" n class))
                     ((static-ctor-already-set) 
                      (format "~a. Further, it may not be set in ~a's constructor" (already-set #t) class))
                     ((static-already-set) (already-set #t))
                     ((field-already-set) (already-set #f))
                     ((static) (format "final field ~a may only be set in the containing class's static initialization" n))
                     ((field) (format "final field ~a may only be set in the containing class's constructor" n)))
                   n (id-src name))))
   
  ;implicit import error
  ;class-lookup-error: string src -> void
  (define (class-lookup-error class src)
    (when (path? class) (set! class (path->string class)))
    (raise-error (string->symbol class)
                 (format "Implicit import of class ~a failed as this class does not exist at the specified location"
                         class)
                 (string->symbol class) src))
  
    (define (check-range-error src type)
    (raise-error
     'check
     (format "Within clause of 'check' must specify a range with a number, found ~a."
             (type->ext-name type))
     'within
     src))
  
  (define (check-double-error test-type actual-type test-src actual-src)
    (let ((check-fault? (prim-integral-type? actual-type)))
      (raise-error
       (if check-fault? 'check 'expect)
       (format "When ~a of a 'check' expression is a ~a, the expression must specify a range with 'within'."
               (if check-fault?
                   "the expression to check"
                   "the expected expression")
               (type->ext-name
                (if check-fault? test-type actual-type)))
       'check (if check-fault? test-src actual-src)
       )))
  
  (define (check-type-error kind level test-type actual-type ta-src)
    (raise-error
     'check
     (cond
       ((and (eq? kind 'void) (eq? test-type 'void))
        "The test of a 'check' expression must produce a value. Current expression does not.")
       ((and (eq? kind 'void) (eq? actual-type 'void))
        "The expected result of a 'check' expression must be a value. Current expression is not a value.")
       (else
        (string-append (format "A 'check' expression compares the test and expected expressions.~n")
                       (format "Found ~a which is not comparable to ~a.~a"
                               (type->ext-name actual-type)
                               (type->ext-name test-type)
                               (if (not (eq? level 'full))
                                   ""
                                   " The expected expression must be castable to the test type.")))        
        #;(string-append
           (format "In a 'check' expression, the type of the expected expression must be ~a the tested expression.~n"
                   (if (eq? kind 'cast) "castable to" "a subtype of"))
           (format "Found ~a, which is not ~a ~a, the type of the tested expression."
                   (type->ext-name actual-type)
                   (case kind
                     ((cast)  "castable to")
                     ((iface subtype) "a subtype of"))
                   (type->ext-name test-type)
                   ))))
     'check ta-src
     ))
  
  (define (check-rand-type-error kind level actual-type expt-type src)
    (raise-error
     'check
     (cond
       [(and (eq? kind 'void) (eq? actual-type 'void))
        "The test of a 'check' expression must produce a value. Current expression does not."]
       [(and (eq? kind 'void) (eq? expt-type 'void))
        "The expected result of a 'check' 'within' expression must be an array of values. Current expression is not a value."]
       [(eq? kind 'not-array)
        (string-append "The expected result of a 'check' 'within' expression must be an array of possible values.\n"
                       (format "Found ~a, which is not appropriate in this expression." (type->ext-name expt-type)))]
       [else
        (string-append "A 'check' 'within' expession compares the test expression with an array of possible answers.\n"
                       (format "Found an array of ~a which is not comparable to ~a."
                               (type->ext-name expt-type)
                               (type->ext-name actual-type)))])
     'within src))

  (define (check-by-==-error t-type a-type src)
    (raise-error
     'check
     (string-append "In a 'check' expression with '==', the type of the expected and actual expression must be castable to each other~n"
                    (format "Given ~a and ~a, which are not comparable."
                            (type->ext-name t-type) (type->ext-name a-type)))
     'by
     src))

  (define (check-by-error kind t-type a-type by src)
    (let ([by (if (id? by) (id-string by) by)])
      (raise-error
       'check
       (case kind
         [(not-obj) 
          (string-append "In a 'check' expression with 'by', the type of the expected value must be an interface or class~n"
                         (format "Exepected value is of ~a type, which is not allowed."
                                 (type->ext-name t-type)))]
         [(no-such-method) 
          (format "Class or interface ~a does not have a method ~a to compare with in this 'check'."
                  (type->ext-name t-type) by)]        
       [(no-arg-count) 
        (format "Class or interface ~a does not have a method ~a accepting one argument for this 'check'"
                (type->ext-name t-type) by)]
       [(conflict) 
        (format "Multiple methods in ~a could accept the argument ~a for comparison in this 'check'."
                (type->ext-name t-type) (type->ext-name a-type) by)]
       [(no-match) 
        (format "No ~a method in ~a expects a ~a for comparison in this 'check'."
                by (type->ext-name t-type) (type->ext-name a-type))]
       [(not-bool)
        (format "Method ~a accepting ~a in ~a does not return a boolean and cannot do the comparison in this 'check'."
                by (type->ext-name a-type) (type->ext-name t-type))])
       'by src)))
  
  ;check-catch-error: type src -> void
  (define (check-catch-error name src)
    (raise-error
     'check
     (format "check catch expects a subtype of Throwable to catch, found ~a, which is not allowed."
             (type->ext-name name))
     'catch src))     
  
  ;check-mutate-kind-error: src -> void
  (define (check-mutate-kind-error src)
    (raise-error
     '->
     "The preceeding expression in a mutation test must be allowable as a statement. This expression is not."
     '-> src))
  
  ;check-mutate-check-error: type src -> void
  (define (check-mutate-check-error type src)
    (raise-error
     '->
     (format "The expression following -> in a mutation test must return a boolean; found expresstion returning ~a."
             (type->ext-name type))
     '-> src))

  
  (define check-location (make-parameter #f))
  
  (define raise-error (make-error-pass check-location))
      
  )
