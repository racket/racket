(module to-scheme mzscheme
  (require "ast.ss"
           "types.ss"
           "name-utils.scm"
           "graph-scc.ss"
           "parameters.ss"
           mzlib/class
           mzlib/list
           mzlib/etc
           (prefix int-set: (lib "integer-set.ss"))
           )
  
  (provide translate-program translate-interactions (struct compilation-unit (contains code locations depends)))
  
  ;(make-compilation-unit (list string) (list syntax) (list location) (list (list string)))
  (define-struct compilation-unit (contains code locations depends) (make-inspector))
  
  ;File takes java AST as defined by ast.ss and produces
  ;semantically (hopefully) equivalent scheme code
  
  ;NOTE! Abstract classes are treated no differently than any class.
  
  ;Parameters for getting to the source of other classes
  (define classes (make-parameter null))
  
  ;Parameters for information about each class
  (define class-name (make-parameter "interactions"))
  (define loc (make-parameter #f))
  (define interactions? (make-parameter #f))
  (define class-override-table (make-parameter null))
  (define parent-name (make-parameter "Object"))
  (define module-name (make-parameter ""))
  (define module-require (make-parameter ""))
  
  ;Parameters for inforamtion about the types
  (define types (make-parameter null))
  (define current-depth (make-parameter 0))
  (define current-local-classes (make-parameter null))
    
  (define stx-for-original-property (read-syntax #f (open-input-string "original")))
  (define (stx-for-source) stx-for-original-property)
  (define (create-syntax oddness sexpression source)
    (datum->syntax-object (or oddness (syntax-location) (stx-for-source)) sexpression source stx-for-original-property))
  (define (make-syntax oddness sexpression source)
    (datum->syntax-object (or oddness (syntax-location) (stx-for-source)) sexpression source))
  
  ;-------------------------------------------------------------------------------------------------------------
  
  ;Type abbreviation
  
  ;The value of this will vary based on information I have at a given time, 
  ;as well as whether location is from source file or not
  ;SrcList  => boolean
  ;         |  (list symbol int)
  ;         |  (list symbol int int int)
  
  
  ;------------------------------------------------------------------------------------------------------------
  ;Helper functions
  ;Functions which are used throughout the transformation
  
  ;build-identifier: (U string symbol (list string)) -> symbol
  (define (build-identifier name)
    (cond
      ((symbol? name) name)
      ((string? name) (string->symbol name))
      ;PROBLEM might not be best method
      ((pair? name) (string->symbol (apply string-append (map (lambda (s) (string-append s ".")) name))))
      (else 
       (error 'build-identifier (format "Given ~s" name))
       name)))
  
  ;build-static-name: string symbol -> string
  (define (build-static-name name . args)
    (format "~a-~a" (if (null? args) (class-name) (car args)) name))
  
  ;build-src: src -> SrcList
  (define (build-src src) 
    (if (not src)
        src
        (if (and (= (src-line src) 0)
                 (= (src-col src) 0)
                 (= (src-pos src) 0)
                 (= (src-span src) 0))
            #f
            (list (or (src-file src) (loc)) (src-line src) (src-col src) (src-pos src) (src-span src)))))
  
  ;get-defualt-value: type-spec -> syntax
  (define (get-default-value type)
    (let ((name (type-spec-name type)))
      (if (> (type-spec-dim type) 0)
          (make-syntax #f 'null #f)
          (cond
            ((memq name '(float double)) (make-syntax #f 0.0 #f))
            ((prim-numeric-type? name) (make-syntax #f 0 #f))
            ((eq? 'char name) (make-syntax #f '#\space #f))
            ((eq? 'boolean name) (make-syntax #f '#f #f))
            (else (make-syntax #f 'null #f))))))
  
  ;create-get-name: string string? -> symbol
  (define (create-get-name name . args)
    (build-identifier (format "~a-~a-get" (if (null? args) (class-name) (car args))  name)))
  
  ;create-set-name: string string? -> symbol
  (define (create-set-name name . args)
    (build-identifier (format "~a-~a-set!" (if (null? args) (class-name) (car args)) name)))
  
  ;Methods to determine member restrictions
  ;make-mod-test: symbol -> (list -> bool)
  (define (make-mod-test acc) (lambda (m) (memq acc m)))
  (define public? (make-mod-test 'public))
  (define private? (make-mod-test 'private))
  (define protected? (make-mod-test 'protected))
  (define static? (make-mod-test 'static))
  (define abstract? (make-mod-test 'abstract))
  (define final? (make-mod-test 'final))
  
  ;get-class-name: (U name type-spec) -> syntax
  (define (get-class-name name)
    (if (type-spec? name)
        (set! name (type-spec-name name)))
    (if (null? (name-path name))
        (translate-id (id-string (name-id name))
                      (id-src (name-id name)))
        (create-syntax #f
                       (build-identifier (get-class-string name))
                       (build-src (name-src name)))))
  
  ;get-class-string: name -> string
  (define (get-class-string name)
    (format "~a~a" (apply string-append (map (lambda (s)
                                               (string-append s "."))
                                             (map id-string (name-path name))))
            (id-string (name-id name))))
  
  ;build-var-name: string -> string
  (define (build-var-name id) (format "~a~~f" id))
  
  ;build-generic-name: string string -> string
  (define (build-generic-name class name) (format "~a-~a~~generic" class name))
  
  ;build-method-name: string (list type) -> string
  (define (mangle-method-name id types)
    (letrec ((parm-name
              (lambda (t)
                (format "-~a"
                        (cond
                          ((symbol? t) t)
                          ((ref-type? t) 
                           (string-append (apply string-append (map (lambda (p) (string-append p "."))
                                                                    (ref-type-path t)))
                                          (ref-type-class/iface t)))
                          ((array-type? t)
                           (string-append (let ((s (parm-name (array-type-type t))))
                                            (substring s 1 (string-length s)))
                                          (format "~a" (array-type-dim t))))
                          (else (error 'mangle-method-name (format "Internal Error: given unexptected type ~a" t))))))))
      (format "~a~a" id (apply string-append (map parm-name types)))))
  
  ;mangle-private-method: string (list type) -> string
  (define (mangle-private-method name args)
    (string-append "private-" (mangle-method-name name args)))
  
  ;constructor? string -> bool
  (define (constructor? name) 
    (equal? name (class-name)))
  
  ;build-constructor-name: string (list type) -> string
  (define (build-constructor-name class-name args)
    (mangle-method-name (format "~a-constructor" class-name) args))
  
  ;-------------------------------------------------------------------------------------------------------------------------
  ;Translation
  
  ;translate-interactions: ast location type-records boolean-> syntax
  (define (translate-interactions prog location type-recs gen-reqs?)
    (loc location)
    (interactions? #t)
    (types type-recs)
    (class-name "interactions")
    (let ((reqs (send type-recs get-class-reqs))
          (syn (cond 
                 ((pair? prog)
                  (send type-recs set-class-reqs null)
                  (make-syntax #f 
                               `(begin ,@(map (lambda (f)
                                                (translate-interactions f location type-recs gen-reqs?))
                                              prog))
                               #f))
                 ((field? prog) 
                  (create-syntax #f
                                 `(begin ,@(translate-field `(private)
                                                            (field-type-spec prog)
                                                            (field-name prog)
                                                            (and (var-init? prog) prog)
                                                            (if (var-init? prog)
                                                                (var-init-src prog)
                                                                (var-decl-src prog))
                                                            #f))
                                 #f))
                 ((statement? prog) (translate-statement prog type-recs))
                 ((expr? prog) (translate-expression prog))
                 (else 
                  (error 'translate-interactions "Internal Error: translate-interactions given ~a" prog)))))
      (if (or (null? reqs) (not gen-reqs?))
          syn
          (make-syntax #f
                       `(begin (require ,@(remove-dup-syntax (translate-interact-require reqs type-recs)))
                               ,syn)
                       #f))))
  
  ;translate-program: package type-records -> (list compilation-unit) 
  (define (translate-program program type-recs)
    (types type-recs)
    (interactions? #f)
    (let* ((package-path (if (package-name program)
                             (append (map id-string (name-path (package-name program)))
                                     (list (id-string (name-id (package-name program)))))
                             null))
           (full-defs (if (null? (packages)) (package-defs program) (append (packages) (package-defs program))))
           (dependent-defs (find-dependent-defs full-defs type-recs)))
      (classes full-defs)
      (map (lambda (defs)
             (let*-values (((ordered-defs) (order-defs defs))
                           ((translated-defs reqs) (translate-defs ordered-defs type-recs)))
               (make-compilation-unit (map (lambda (def) (id-string (def-name def))) ordered-defs)
                                      translated-defs
                                      (map def-file ordered-defs)
                                      reqs)))
           dependent-defs)))
  
  ;get-package: definition type-records -> (list string)
  (define (get-package def type-recs)
    (send type-recs set-location! (def-file def))
    (send type-recs lookup-path (id-string (def-name def)) (lambda () (error 'internal-error))))
  
  ;find-dependent-defs: (list defs) -> (list (list defs))
  (define (find-dependent-defs defs type-recs)
    (let* ((for-each-def (lambda (defs thunk) (for-each thunk defs)))
           (find 
            (lambda (req)
              (letrec ((walker 
                        (lambda (defs)
                          (and (not (null? defs))
                               (if (and (equal? (req-path req)
                                                (get-package (car defs) type-recs))
                                        (equal? (req-class req)
                                                (id-string (def-name (car defs)))))
                                   (car defs)
                                   (walker (cdr defs)))))))
                (walker defs))))
           (get-requires
            (lambda (def)
              (filter (lambda (x) x) (map find (def-uses def)))))
           )
      (get-scc defs get-requires for-each)
      #;(get-strongly-connected-components defs for-each-def get-requires)))
  
  ;get-strongly-connected-components: GRAPH (GRAPH (NODE -> void) -> void) (NODE -> (list NODE)) -> (list (list NODE))
  (define (get-strongly-connected-components graph for-each-node get-connected-nodes)
    
    (let ((marks (make-hash-table))
          (strongly-connecteds null)
          (cur-cycle-length 0)
          (current-cycle null))
      
      (letrec ((already-in-cycle? 
                (lambda (n) (eq? 'in-a-cycle (hash-table-get marks n))))
               (in-current-cycle?
                (lambda (n) (hash-table-get current-cycle n (lambda () #f))))
               (current-cycle-memq
                (lambda (nodes) (ormap in-current-cycle? nodes)))
               (add-to-current-cycle
                (lambda (n) 
                  (set! cur-cycle-length (add1 cur-cycle-length))
                  (hash-table-put! current-cycle n #t)))
               (retrieve-current-cycle
                (lambda () (hash-table-map current-cycle (lambda (key v) key))))
               
               ;; componetize : NODE (list NODE) bool -> void
               (componentize 
                (lambda (node successors member?)
                  (unless (already-in-cycle? node)
                    (printf "componentize ~a ~a ~a~n" 
                            (id-string (def-name node))
                            (map id-string (map def-name successors)) 
                            (map id-string (map def-name (retrieve-current-cycle)))
                            )
                    (let ((added? #f)
                          (cur-length cur-cycle-length)
                          (old-mark (hash-table-get marks node)))
                      (when (and (not member?) (current-cycle-memq successors))
                        (set! added? #t)
                        (add-to-current-cycle node))
                      (hash-table-put! marks node 'in-progress)
                      (for-each 
                       (lambda (successor)
                         (unless (or (in-current-cycle? successor)
                                     (eq? 'in-progress (hash-table-get marks successor)))
                           (componentize successor (get-connected-nodes successor) #f)))
                       successors)
                      (printf "finished successors for ~a~n" (id-string (def-name node)))
                      (when (not (= cur-length cur-cycle-length))
                        (add-to-current-cycle node))
                      
                      (if (or added? (= cur-length cur-cycle-length))
                          (hash-table-put! marks node old-mark)
                          (componentize node successors #f)))))))
        
        (for-each-node graph (lambda (n) (hash-table-put! marks n 'no-info)))
        
        (for-each-node graph
                       (lambda (node)
                         #;(hash-table-for-each 
                          marks 
                          (lambda (key val) (printf "~a -> ~a~n" (eq-hash-code key) val)))
                         #;(printf "Working on ~a~n~n" (eq-hash-code node))
                         #;(printf "node: ~a successors: ~a" (def-name node) (map def-name (get-connected-nodes node)))
                         (when (eq? (hash-table-get marks node) 'no-info)
                           (set! current-cycle (make-hash-table))
                           (add-to-current-cycle node)
                           (set! cur-cycle-length 0)
                           (printf "calling componetice ~a~n" (id-string (def-name node)))
                           (for-each (lambda (node) (componentize node (get-connected-nodes node) #f))
                                     (get-connected-nodes node))
                           (set! strongly-connecteds 
                                 (cons (retrieve-current-cycle) strongly-connecteds))
                           (printf "~a~n~n" (map id-string (map def-name (car strongly-connecteds))))
                           (hash-table-for-each
                            current-cycle
                            (lambda (n v) (hash-table-put! marks n 'in-a-cycle))))))
        
        strongly-connecteds)))
  
  ;order-defs: (list def) -> (list def)
  (define (order-defs defs)
    (reverse
     (let loop ((ordered-defs null)
                (local-defs defs))
       (cond
         ((null? local-defs) ordered-defs)
         ((add-def? (car local-defs) local-defs ordered-defs)
          (loop (cons (car local-defs) ordered-defs)
                (cdr local-defs)))
         (else
          (loop ordered-defs (append (cdr local-defs) (list (car local-defs)))))))))
    
  ;add-def? def (list def) (list def) -> bool
  (define (add-def? def local-defs ordered-defs)
    (andmap (lambda (e)
              (satisfied-extend? e local-defs ordered-defs))
            (append (header-extends (def-header def))
                    (header-implements (def-header def)))))
  
  ;satisified-extend? id (list def) (list def) -> bool
  (define (satisfied-extend? extend local-defs ordered-defs)
    (or (null? extend)
        (not (member (id-string (name-id extend)) 
                     (map id-string (map def-name local-defs))))
        (member (id-string (name-id extend))
                (map id-string (map def-name ordered-defs)))))
  
  ;make-composite-name: string -> string
  (define (make-composite-name d)
    (build-identifier (string-append d "-composite")))
  
  ;translate-defs: (list def) type-records -> (values (list syntax) (list reqs))
  (define (translate-defs defs type-recs)
    (let ((sorted-d-list (sort (map (compose id-string def-name) defs) string<?)))                                   
      (module-name (make-composite-name (car sorted-d-list))))
    (module-require 
     (if (to-file) 
         (let ((location (build-path (begin (send type-recs set-location! (def-file (car defs)))
                                            (send type-recs get-compilation-location) "compiled")
                                     (string-append (symbol->string (module-name)) "_ss.zo"))))
           (for-each 
            (lambda (def) (send type-recs set-composite-location (id-string (def-name def)) location))
            defs)
           `(file ,(path->string (build-path (string-append (symbol->string (module-name)) ".ss")))))
         #`(quote #,(module-name))))
    (let* ((translated-defs 
            (map (lambda (d)
                   (cond
                     ((class-def? d) (translate-class d type-recs #f 0))
                     ((test-def? d) (translate-class d type-recs #t 0))
                     (else (translate-interface d type-recs))))
                 defs))
           (group-reqs (apply append (map (lambda (d) (map (lambda (r) (list (def-file d) r)) (def-uses d)))
                                          defs)))
           (reqs (filter-reqs group-reqs defs type-recs)))
      (values (if (> (length translated-defs) 1)
                  (cons (make-syntax #f `(module ,(module-name) mzscheme
                                           (require mzlib/class
                                                    (prefix javaRuntime: profj/libs/java/runtime)
                                                    (prefix c: mzlib/contract)
                                                    ,@(remove-dup-syntax (translate-require reqs type-recs)))
                                           ,@(apply append (map car translated-defs)))
                                     #f)
                        (map cadr translated-defs))
                  (list (make-syntax #f 
                                     `(module ,(build-identifier (regexp-replace "-composite" 
                                                                                 (symbol->string (module-name)) 
                                                                                 ""))
                                        mzscheme
                                        (require mzlib/class
                                                 (prefix javaRuntime: profj/libs/java/runtime)
                                                 #;(prefix javaRuntime: (lib "runtime.scm" "profj" "libs" "java"))
                                                 (prefix c: mzlib/contract)
                                                 ,@(remove-dup-syntax
                                                    (translate-require (map (lambda (r) (list (def-file (car defs)) r))
                                                                            (def-uses (car defs)))
                                                                       type-recs)))
                                        ,@(car (car translated-defs)))
                                     #f)))
              (filter (lambda (req) (not (member req reqs)))
                      (map (lambda (r-pair) (cadr r-pair)) group-reqs)))))
  
  ;filter-reqs: (list (list location req)) (list def) type-records -> (list req)
  (define (filter-reqs reqs defs type-recs)
    (if (null? reqs)
        null
        (if (or (reference (car reqs) defs type-recs)
                (req-member (car reqs) (cdr reqs)))
            (filter-reqs (cdr reqs) defs type-recs)
            (cons (car reqs) (filter-reqs (cdr reqs) defs type-recs)))))
  
  ;reference: (list req location) (list def) type-records -> bool
  (define (reference req defs type-recs)
    (and (not (null? defs))
         (or (and (equal? (req-path (cadr req)) (get-package (car defs) type-recs))
                  (equal? (get-leading-name (req-class (cadr req)))
                          (id-string (def-name (car defs)))))
             (reference req (cdr defs) type-recs))))
  
  ;req-member: (list location req) (list (list location req)) -> bool
  (define (req-member req reqs)
    (and (not (null? reqs))
         (or (equal? (cadr req) (cadr (car reqs)))
             (req-member req (cdr reqs)))))
  
  ;remove-dup-syntax: (list syntax) -> (list syntax)
  (define (remove-dup-syntax syn)
    (letrec ((remove 
              (lambda (duped syn)
                (if (null? syn)
                    null
                    (if (eq? duped (car syn))
                        (remove duped (cdr syn))
                        (cons (car syn) (remove duped (cdr syn)))))))
             (remove-dups
              (lambda (syn)
                (if (null? syn)
                    null
                    (if (memq (car syn) (cdr syn))
                        (cons (car syn) (remove-dups (remove (car syn) (cdr syn))))
                        (cons (car syn) (remove-dups (cdr syn))))))))
      (remove-dups syn)))
  
  ;translate-interact-require: (list reg) type-record -> (list syntax)
  (define (translate-interact-require reqs type-recs)
    (if (null? reqs)
        null
        (let ((req (car reqs)))
          (cons (begin (send type-recs set-location! 'interactions)
                       (send type-recs get-require-syntax
                             (send type-recs require-prefix?
                                   (cons (req-class req) (req-path req))
                                   (lambda () #f))
                             (cons (req-class req) (req-path req))
                             (lambda () #f)))
                (translate-interact-require (cdr reqs) type-recs)))))
  
  ;translate-require: (list (list location req)) type-records -> (list syntax)
  (define (translate-require reqs type-recs)
    (cond
      ((null? reqs) null)
      ((member (cadr (car reqs))
               (list (make-req "Class" '("java" "lang"))
                     (make-req "PrintStream" '("java" "io"))
                     (make-req "PrintWriter" '("java" "io"))))
       (translate-require (cdr reqs) type-recs))
      (else
       (let* ((req (cadr (car reqs)))
              (err (lambda () 
                     (error 'translate-require 
                            (format "Internal Error: (make-req ~a ~a) not found" 
                                    (req-class req) (req-path req))))))
         (cons (begin (send type-recs set-location! (car (car reqs)))
                      (send type-recs get-require-syntax 
                            (send type-recs require-prefix? 
                                  (cons (req-class req) (req-path req)) 
                                  err)
                            (cons (req-class req) (req-path req))
                            err))
               (translate-require (cdr reqs) type-recs))))))
  
  ;translate-implements: (list name) -> (list syntax)
  (define (translate-implements imp)
    (map (lambda (i)
           (let* ((id (name-id i))
                  (st (id-string id))
                  (path (name-path i)))
             (if (null? path)
                 (translate-id st (id-src id))
                 (create-syntax #f (build-identifier (append (map id-string path) (list st)))
                                (build-src (name-src i))))))
         imp))
  
  ;translate-class: class-def type-records boolean int -> (list syntax syntax)
  (define (translate-class class type-recs test? depth)
    ;Let's grab onto the enclosing class-specific info incase depth > 0
    (let ((old-class-name (class-name))
          (old-parent-name (parent-name))
          (old-inner-class (inner-class))
          (old-override-table (class-override-table)))
      (unless (> depth 0)
        (loc (def-file class)) (send type-recs set-location! (loc)))
      (when (> depth 0) (inner-class #t))
      
      (let*-values (((header) (def-header class))
                    ((kind) (def-kind class))
                    ((closure-args) (def-closure-args class))
                    ((parent parent-src extends-object?)
                     (if (null? (header-extends header))
                         (if (not test?) 
                             (values "Object" #f #t)
                             (values "TestBase" #f #t))
                         (let-values (((p p-s) (get-parent (header-extends header))))
                           (values p p-s
                                   (class-record-object? 
                                    (send type-recs get-class-record 
                                          (name->type (car (header-extends header)) #f #f 'full type-recs)))))))
                    ((class*) (create-syntax #f 'class* (build-src (def-key-src class))))
                    ((class-members) (separate-members (def-members class)))
                    ((methods) (separate-methods (members-method class-members) (make-accesses null null null null null null)))
                    ((fields) (separate-fields (members-field class-members) (make-accesses null null null null null null))))
        
        ;set class specific parameters - old ones are safe
        (class-name (id-string (header-id header)))
        (parent-name parent)
        (class-override-table (make-hash-table))
            
        (let* ((class (translate-id (class-name) (id-src (header-id header))))
               (overridden-methods (get-overridden-methods (append (accesses-public methods) 
                                                                   (accesses-package methods) 
                                                                   (accesses-protected methods))))
               (restricted-methods (make-method-names ;(append (accesses-package methods)
                                    (accesses-protected methods);)
                                    overridden-methods))
               (make-gen-name 
                (lambda (m)
                  (build-generic-name (class-name)
                                      ((if (constructor? (id-string (method-name m))) build-constructor-name mangle-method-name)
                                       (id-string (method-name m))
                                       (method-record-atypes (method-rec m))))))
               #;(providable-generics 
                (map make-gen-name 
                     (append (accesses-public methods)
                             (accesses-package methods)
                             (accesses-protected methods))))
               (private-generics (map make-gen-name (accesses-private methods)))
               (names-for-dynamic (generate-dynamic-names (append (accesses-public methods)
                                                                  (accesses-package methods)
                                                                  (accesses-protected methods))
                                                          overridden-methods))
               #;(dynamic-method-defs (generate-dyn-method-defs names-for-dynamic))
               #;(p~ (printf "about to call class-record-methods : ~a ~a ~n" (class-name) (string? (class-name))))
               (wrapper-classes (append (generate-wrappers (class-name)
                                                           (parent-name)
                                                           (filter
                                                            (lambda (m) (not (or (private? (method-record-modifiers m))
                                                                                 (static? (method-record-modifiers m)))))
                                                            (begin0
                                                              (class-record-methods (send type-recs get-class-record (list (class-name))))
                                                              #;(printf "finished class-record-methods~n")))
                                                           (append (accesses-public fields) (accesses-package fields)
                                                                   (accesses-protected fields)))
                                        (generate-contract-defs (class-name))))
               (static-method-names (make-static-method-names (accesses-static methods) type-recs))
               (static-field-names (make-static-field-names (accesses-static fields)))
               (static-field-setters (make-static-field-setters-names 
                                      (filter (lambda (f) (not (final? 
                                                                (map modifier-kind (field-modifiers f)))))
                                              (accesses-static fields))))
               (field-getters/setters (make-field-accessor-names (append (accesses-public fields)
                                                                         (accesses-package fields)
                                                                         (accesses-protected fields))))
               (provides `(provide ,(build-identifier (class-name))
                                   ,@(map build-identifier (list (format "guard-convert-~a" (class-name))
                                                                 (format "convert-assert-~a" (class-name))
                                                                 (format "wrap-convert-assert-~a" (class-name))
                                                                 (format "dynamic-~a/c" (class-name))
                                                                 (format "static-~a/c" (class-name))))
                                   ;,@restricted-methods
                                   ,@(map build-identifier static-method-names)
                                   ,@(map build-identifier static-field-names)
                                   ,@static-field-setters
                                   #;,@(map build-identifier providable-generics)
                                   ,@field-getters/setters)))

          (let ((class-syntax
                 `(,(unless (or (memq 'private (map modifier-kind (header-modifiers header)))
                                 (eq? 'anonymous kind)
                                 (eq? 'statement kind))
                       provides)
                    ,@(if (null? (accesses-private methods))
                          null
                          (list
                           (create-local-names (make-method-names (accesses-private methods) null))))
                    #;(if (null? restricted-methods)
                          null
                          (list (create-local-names (append (make-method-names (accesses-private methods) null)
                                                            restricted-methods))))
                    (define ,class
                      (,class* ,(if extends-object?
                                    (translate-id parent parent-src)
                                    `(Object-Mix ,(translate-id parent parent-src)))
                               ,(translate-implements (header-implements header))
                               
                               (super-instantiate ())
                               
                               ,@(if (> depth 0)
                                     `((init-field 
                                        ,@(let loop ((d depth))
                                            (cond
                                              ((= d 0) null)
                                              (else 
                                               (cons (string->symbol (format "encl-this-~a~~f" d))
                                                     (loop (sub1 d))))))))
                                     null)
                               
                             ,@(if (null? closure-args)
                                   null
                                   `((init-field 
                                      ,@(let loop ((args 
                                                    (map id-string closure-args)))
                                          (cond
                                            ((null? args) null)
                                            (else (cons (string->symbol (format "~a~~f" (car args)))
                                                        (loop (cdr args)))))))))
                             ,@(let ((translated-fields (map 
                                                         (lambda (f)
                                                           (translate-field (map modifier-kind (field-modifiers f))
                                                                            (field-type-spec f)
                                                                            (field-name f)
                                                                            (and (var-init? f) f)
                                                                            (if (var-init? f)
                                                                                (var-init-src f)
                                                                                (var-decl-src f))
                                                                            #f))
                                                         (reverse (filter (lambda (f)
                                                                            (not (memq 'static (map modifier-kind (field-modifiers f)))))
                                                                          (members-field class-members))))))
                                 (append
                                  (map car translated-fields)
                                  (map cadr translated-fields)))
                             
                             
                             ,@(create-private-setters/getters (accesses-private fields))
                             
                             ,@(generate-inner-makers (members-inner class-members) 
                                                      depth type-recs)
                             
                             
                             ,@(map (lambda (m) (translate-method (method-type m)
                                                                  (map modifier-kind (method-modifiers m))
                                                                  (method-name m)
                                                                  (method-parms m)
                                                                  (method-body m)
                                                                  (method-all-tail? m)
                                                                  (method-src m)
                                                                  (> depth 0)
                                                                  depth
                                                                  (method-rec m)
                                                                  type-recs))
                                    (append (accesses-public methods)
                                            (accesses-package methods)
                                            (accesses-protected methods)
                                            (accesses-private methods)))
                             
                             ;,@dynamic-method-defs
                             
                             (define/override (my-name) ,(class-name))
                             
                             ,@(let ((non-static-fields
                                      (append (accesses-public fields)
                                              (accesses-package fields)
                                              (accesses-protected fields)
                                              (accesses-private fields))))
                                 (if (null? non-static-fields)
                                     null
                                     `((define/override (field-names)
                                         (append (super field-names)
                                                 (list ,@(map 
                                                          (lambda (n) (id-string (field-name n)))
                                                          non-static-fields
                                                          ))))
                                       (define/override (field-values)
                                         (append (super field-values)
                                                 (list ,@(map 
                                                          (lambda (n) (build-identifier (build-var-name (id-string (field-name n)))))
                                                          non-static-fields)))))))
                             
                             (define field-accessors ,(build-field-table create-get-name 'get fields))
                             (define field-setters ,(build-field-table create-set-name 'set fields))
                             (define private-methods
                               ,(if (null? (accesses-private methods))
                                    '(make-hash-table)
                                    (build-method-table (accesses-private methods) private-generics)))
                             
                             ,@(if test?
                                   (cons
                                    (let ((test-methods (filter test-method? (accesses-public methods))))
                                      `(define/override (testMethods)
                                         ,(if (null? test-methods)
                                              '(super testMethods)
                                              `(append (list 
                                                        ,@(map 
                                                           (lambda (testcase)
                                                             `(list ,(id-string (method-name testcase))
                                                                    (lambda ()
                                                                      (send this ,(build-identifier 
                                                                                   (id-string (method-name testcase)))))))
                                                           (filter test-method? (accesses-public methods))))
                                                       (super testMethods)))))
                                    (if (null? (test-header-tests header))
                                        null
                                        (let* ((test-classes 
                                                (map id-string
                                                     (map name-id (test-header-tests header))))
                                               (class-defs 
                                                (filter (lambda (d) (member (id-string (def-name d)) test-classes))
                                                        (classes)))
                                               (class/methods-list
                                                (map (lambda (d)
                                                       (cons (id-string (def-name d))
                                                             (filter (lambda (m)
                                                                       (and (method? m) (method-src m))) (def-members d))))
                                                     class-defs))
                                               (tested-methods
                                                (map (lambda (c/m)
                                                       (cons (car c/m)
                                                             (map (lambda (m) (id-string (method-name m))) (cdr c/m))))
                                                     class/methods-list))
                                               (tested-methods-expr-srcs
                                                (map (lambda (c/m)
                                                       (cons (car c/m)
                                                             (map 
                                                              (lambda (m)
                                                                (let ([srcs (get-srcs (method-body m))])
                                                                  (srcs->spans srcs)))
                                                              (cdr c/m))))
                                                     class/methods-list))
                                               #;(class/lookup-funcs
                                                (map (lambda (c)
                                                       (let* ((m-name (lambda (m) (id-string (method-name m))))
                                                              (m-start (lambda (m) (src-pos (method-src m))))
                                                              (m-stop (lambda (m)
                                                                        (+ (m-start m) (src-span (method-src m))))))                                                       
                                                         `(let ((methods-covered ',(map (lambda (m) `(,(m-name m) #f))
                                                                                        (cdr c)))                                                              
                                                                (srcs ',(map (lambda (m)
                                                                               `(,(m-name m) ,(get-srcs (method-body m))))
                                                                             (cdr c))))
                                                            (list ,(car c)
                                                                  methods-covered
                                                                  (lambda (x)
                                                                    (cond
                                                                      ,@(map 
                                                                         (lambda (m)
                                                                           `((and (< ,(m-start m) x) (< x ,(m-stop m)))
                                                                             (let ((m-list (assq ,(m-name m) srcs)))
                                                                               (unless (null? (car (cdr m-list)))
                                                                                 (set-cdr! m-list (list (,remove x (car (cdr m-list)))))
                                                                                 (when (null? (car (cdr m-list)))
                                                                                   (set-cdr! (assq ,(m-name m) methods-covered) (list #t)))))))
                                                                         (cdr c))))))))
                                                     class/methods-list)))
                                          (list 
                                           `(define/override (testedClasses)
                                              (append (list ,@test-classes) (super testedClasses)))
                                           `(define/override (testedMethods-dynamic class-name)
                                              (or (assq class-name (list ,@tested-methods))
                                                  (super testedMethods-dynamic class-name)))
                                           `(define/override (testMethodsSrcs-dynamic class-name)
                                              (or (assq class-name (list ,@tested-methods-expr-srcs))
                                                  (super testedMethodsSrcs-dynamic class-name)))
                                           #;`(define/override (testCoverage-boolean-int report? src)
                                                   (let ((class/lookups (list ,@class/lookup-funcs)))
                                                     (if report?
                                                         (append (map (lambda (c) (list (car c) (cadr c)))
                                                                      class/lookups)
                                                                 (super testCoverage-boolean-int report? src))
                                                         (begin
                                                           (for-each (lambda (c) ((caddr c) src)) class/lookups)
                                                           (super testCoverage-boolean-int report? src)))))))))
                                   null)
                                                          
                             ,@(map (lambda (i) (translate-initialize (initialize-static i)
                                                                      (initialize-block i)
                                                                      (initialize-src i)
                                                                      type-recs))
                                    (members-init class-members))
                             
                             ))
                          
                          ,@wrapper-classes
                          
                          #;,@(create-generic-methods (append (accesses-public methods)
                                                            (accesses-package methods)
                                                            (accesses-protected methods)
                                                            (accesses-private methods)))
                          
                          ,@(create-field-accessors field-getters/setters
                                                    (append (accesses-public fields)
                                                            (accesses-package fields)
                                                            (accesses-protected fields)))
                          ,@(map (lambda (def) (translate-class def type-recs #f (add1 depth)))
                                 (members-inner class-members))
                          ,@(create-static-methods (append static-method-names
                                                           (make-static-method-names 
                                                            (accesses-private-static methods) 
                                                            type-recs))
                                                   (append (accesses-static methods)
                                                           (accesses-private-static methods))
                                                   type-recs)
                          ,@(create-static-fields (append static-field-names
                                                          (make-static-field-names (accesses-private-static fields)))
                                                  (append (accesses-static fields)
                                                          (accesses-private-static fields)))
                          ,@(create-static-setters static-field-setters
                                                   (filter (lambda (f) (not (final? 
                                                                             (map modifier-kind (field-modifiers f)))))
                                                           (accesses-static fields)))
                          ,@(map (lambda (i) (translate-initialize (initialize-static i)
                                                                   (initialize-block i)
                                                                   (initialize-src i)
                                                                   type-recs))
                                 (members-static-init class-members))
                          
                          )
                  ))
            
            ;reset the old class-specific info if in inner-class
            (begin0
              (if (> depth 0)
                  (cons 'begin class-syntax)
                  (list class-syntax 
                        (make-syntax 
                         #f
                         `(module ,(build-identifier (class-name)) mzscheme (require ,(module-require)) ,provides)
                         #f)))
              (when (> depth 0)
                (class-name old-class-name)
                (parent-name old-parent-name)
                (inner-class old-inner-class)
                (class-override-table old-override-table))))))))
  
  ;generate-contract-defs: string -> (list sexp)
  (define (generate-contract-defs class-name)
    `((define ,(build-identifier (string-append "dynamic-" class-name "/c"))
        (c:flat-named-contract ,class-name 
                               (lambda (v) (is-a? v ,(build-identifier (string-append "convert-assert-" class-name))))))
      (define ,(build-identifier (string-append "static-" class-name "/c"))
        (c:flat-named-contract ,class-name
                               (lambda (v) (is-a? v ,(build-identifier (string-append "guard-convert-" class-name))))))))

  ;generate-wrappers: string (list method-record) (list field) -> (list sexp)
  (define (generate-wrappers class-name super-name methods fields)
    (let* (;these methods will be used to detect when a method is now overloaded when it wasn't in the super class
           (wrapped-methods-initial
            (filter (lambda (m)
                      (and (not (eq? (method-record-rtype m) 'ctor))
                           (not (method-record-override m)))) methods))
           (wrapped-methods
            (filter 
             (lambda (m) (equal? (car (method-record-class m)) class-name))
             wrapped-methods-initial))
           (add-ca 
            (lambda (name) (build-identifier (string-append "convert-assert-" name))))
           (add-gc 
            (lambda (name) (build-identifier (string-append "guard-convert-" name))))
           (class-text
            (lambda (name super-name from-dynamic? extra-methods)
              `(define ,name
                 (class ,super-name
                   (init w* p* n* s* c*)
                   (define wrapped-obj  null)
                   (set! wrapped-obj w*)
                   (super-instantiate (w* p* n* s* c*))
                  
                   ,@(generate-wrapper-fields fields from-dynamic?)
                   
                   ,@(generate-wrapper-methods
                      (filter (lambda (m) (and (not (eq? (method-record-rtype m) 'ctor))
                                               (not (object-method? m))))
                              wrapped-methods) #f from-dynamic?)
                   ,@extra-methods
                   ))))
           (dynamic-callables (refine-method-list wrapped-methods-initial class-name)))
      (list 
       `(define (,(build-identifier (string-append "wrap-convert-assert-" class-name)) obj p n s c)
          (let ((raise-error
                 (lambda (method-name num-args)
                   (raise (make-exn:fail
                           (format "~a broke the contract with ~a here, expected an object with a method ~a accepting ~a args"
                                   n p method-name num-args)
                           c)))))
            (and ,@(map method->check/error
                        (filter (lambda (m) (not (eq? 'ctor (method-record-rtype m)))) wrapped-methods))))
          #;(c:contract ,(methods->contract (filter (lambda (m) (not (eq? 'ctor (method-record-rtype m))))
                                                    methods)) obj p n s)
          (make-object ,(add-ca class-name) obj p n s c))
       (class-text (add-ca class-name) (add-ca super-name) #t null)
       (class-text (add-gc class-name) (add-gc super-name) #f 
                   (generate-wrapper-methods dynamic-callables #t #f)))))
    
  ;generate-wrapper-fields: (list field) boolean -> sexp
  (define (generate-wrapper-fields fields from-dynamic?)
    (apply append
           (map (lambda (field)
                  (let* ((name (id-string (field-name field)))
                         (dynamic-access-body
                          (lambda (guard-body scheme-body)
                            `(if (is-a? wr* guard-convert-Object)
                                 ,guard-body
                                 ,scheme-body)))
                         (get-name (create-get-name name))
                         (set-name (create-set-name name))
                         (get-call `(,get-name wr*))
                         (set-call `(lambda (new-val) (,set-name wr* new-val))))

                    (list
                     `(define/public (,(build-identifier  (format "~a-wrapped" get-name)))
                        (let ([wr* wrapped-obj]
                              [pb* (send this pos-blame*)]
                              [nb* (send this neg-blame*)]
                              [sr* (send this src*)]
                              [cc* (send this cc-marks*)])
                          ,(convert-value 
                            (if from-dynamic? 
                                (assert-value 
                                 (dynamic-access-body get-call `(get-field wr* (quote ,(build-identifier name))))
                                 (field-type field) #t 'field name)
                                get-call) (field-type field) from-dynamic?)))
                     (if (memq 'final (field-modifiers field))
                         null
                         `(define/public (,(build-identifier (format "~a-wrapped" set-name)) new-val)
                            (let ([wr* wrapped-obj]
                                  [pb* (send this pos-blame*)]
                                  [nb* (send this neg-blame*)]
                                  [sr* (send this src*)]
                                  [cc* (send this cc-marks*)])
                              (,(if from-dynamic?
                                    (dynamic-access-body set-call 
                                                         `(lambda (new-val) 
                                                            (define set-field null)
                                                            (set-field wr* (quote ,(build-identifier name)) new-val)))
                                    set-call)
                               ,(convert-value (if (not from-dynamic?)
                                                   (assert-value 'new-val (field-type field) #t 'field name)
                                                   'new-val) (field-type field) from-dynamic?))))))))
                fields)))
                
  ;generate-wrapper-methods: (list method-record) boolean boolean -> (list sexp)
  ;When is dynamic-callable?, will define methods callable from a dynamic context
  (define (generate-wrapper-methods methods dynamic-callable? from-dynamic?)
    (map (lambda (method)
           (let* ((call-name (mangle-method-name (method-record-name method)
                                                 (method-record-atypes method)))
                  (define-name (if dynamic-callable? (java-name->scheme (method-record-name method)) call-name))
                  (list-of-args (map (lambda (a) (gensym "arg-")) (method-record-atypes method))))
             (cond 
               ((and dynamic-callable? (equal? define-name call-name))
                `(void))
               (from-dynamic?
                `(define/public (,(build-identifier define-name) ,@list-of-args)
                   (let ([wr* wrapped-obj]
                         [pb* (send this pos-blame*)]
                         [nb* (send this neg-blame*)]
                         [sr* (send this src*)]
                         [cc* (send this cc-marks*)])
                     ,(convert-value 
                       (assert-value 
                        `(send wr* ,(build-identifier call-name)
                               ,@(map (lambda (arg type) 
                                        (convert-value (assert-value arg type #f) type #f))
                                      list-of-args (method-record-atypes method)))
                        (method-record-rtype method) from-dynamic? 
                        'method-ret (method-record-name method))
                       (method-record-rtype method)
                       from-dynamic?))))
               (else
                `(define/public (,(build-identifier define-name) . args)
                   (let ([wr* wrapped-obj]
                         [pb* (send this pos-blame*)]
                         [nb* (send this neg-blame*)]
                         [sr* (send this src*)]
                         [cc* (send this cc-marks*)])
                     (unless (= (length args) ,(length list-of-args))
                       (raise 
                        (make-exn:fail:contract:arity
                         (format "~a broke the contract with ~a here, method ~a of ~a called with ~a args, instead of ~a"
                                 nb* pb* ,(method-record-name method) ,(class-name) (length args) ,(length list-of-args))
                         cc*)))
                     (let (,@(map (lambda (arg type ref)
                                    `(,arg ,(convert-value (assert-value `(list-ref args ,ref) type #t 'method-arg (method-record-name method)) type #t)))
                                  list-of-args (method-record-atypes method) (list-from 0 (length list-of-args))))
                       ,(convert-value `(send wr* ,(build-identifier call-name)
                                              ,@list-of-args) (method-record-rtype method) #f))))))))
           methods))
  
  (define (list-from from to)
    (cond
      ((= from to) null)
      (else (cons from (list-from (add1 from) to)))))
    
  ;methods->contract: (list method-record) -> sexp
  (define (methods->contract methods)
    `(c:object-contract ,@(map (lambda (m)
                               `(,(build-identifier (mangle-method-name (method-record-name m)
                                                                        (method-record-atypes m)))
                                 (c:-> ,@(map (lambda (a) 'c:any/c) (method-record-atypes m)) c:any/c)))
                             methods)))
  
  ;method->check/error: method-record -> sexp
  (define (method->check/error method)
    (let* ((name (method-record-name method))
           (m-name (mangle-method-name name (method-record-atypes method)))
           (num-args (length (method-record-atypes method))))
    `(or (object-method-arity-includes? obj
                                        (quote ,(build-identifier m-name))
                                        ,num-args)
         (raise-error ,name ,num-args))))
  
  ;convert-value: sexp type boolean -> sexp
  (define (convert-value value type from-dynamic?)
    (cond
      ((symbol? type)
       (case type
         ((int byte short long float double char boolean dynamic void null) value)
         ((string) (if from-dynamic?
                       `(make-java-string ,value)
                       `(send ,value get-mzscheme-string)))))
      ((dynamic-val? type) value)
      ((array-type? type) value
       #;(if from-dynamic?
           `(wrap-convert-assert-array ,value pb* nb* sr* cc*)
           `(make-object guard-convert-array ,value pb* nb* sr* cc*)))
      ((ref-type? type) 
       (cond 
         ((and (equal? string-type type) from-dynamic?) `(make-java-string ,value))
         ((equal? string-type type) `(send ,value get-mzscheme-string))
         ((member type (list 
                        (make-ref-type "Class" '("java" "lang"))
                        (make-ref-type "PrintStream" '("java" "io"))
                        (make-ref-type "PrintWriter" '("java" "io")))) value)
         (from-dynamic? `(,(build-identifier (string-append "wrap-convert-assert-" (ref-type-class/iface type)))
                           ,value pb* nb* sr* cc*))
         (else `(make-object ,(build-identifier (string-append "guard-convert-" (ref-type-class/iface type)))
                  ,value pb* nb* sr* cc*))))
      (else value)))
  
  ;assert-value: sexp type boolean -> sexp
  (define assert-value 
    (opt-lambda (value type from-dynamic? (kind 'unspecified) (name #f))
      (cond
        ((symbol? type)
         (let ((check
                (lambda (ok?)
                  `(let ((v-1 ,value))
                     (if (,ok? v-1) v-1
                         (raise 
                          (make-exn:fail
                           ,(case kind
                              ((unspecified)
                               `(format "~a broke the contract with ~a here, type-mismatch expected ~a given ~a"
                                        nb* pb* (quote ,type) v-1))
                              ((field)
                               `(format "~a broke the contract with ~a here, type-mismatch for field ~a of class ~a: expected ~a given ~a"
                                        nb* pb* ,name ,(class-name) (quote ,type) v-1))
                              ((method-arg)
                               `(format "~a broke the contract with ~a here, type-mismatch for method argument of ~a in class ~a: expected ~a given ~a"
                                        nb* pb* ,name ,(class-name) (quote ,type) v-1))
                              ((method-ret)
                               `(format "~a broke the contract with ~a here, type-mismatch for method return of ~a in ~a: expected ~a given ~a"
                                        nb* pb* ,name ,(class-name) (quote ,type) v-1)))
                           cc*)))))))
           (case type
             ((int byte short long) (check 'integer?))
             ((float double) (check 'real?))
             ((char) (check 'char?))
             ((string) (check 'string?))
             ((boolean) (check 'boolean?))
             ((dynamic) value)
             ((null) value))))
        ((and (ref-type? type) (equal? string-type type))
         (assert-value value 'string from-dynamic? kind name))
        (else value))))
  
  ;Removes from the list all methods that are not callable from a dynamic context
  ;refine-method-list: (list method-record) string -> (list method-record)
  (define (refine-method-list methods class)
    (letrec ((refine
              (lambda (methods)
                (cond 
                  ((null? methods) methods)
                  ((method-record-override (car methods))
                   (refine (cdr methods)))
                  ((eq? 'ctor (method-record-rtype (car methods)))
                   (refine (cdr methods)))
                  (else
                   (let ((overloaded-removed 
                          (filter (lambda (m) (not (equal? (method-record-name (car methods))
                                                           (method-record-name m))))
                                  (cdr methods))))
                     (if (> (length (cdr methods))
                            (length overloaded-removed))
                         (refine overloaded-removed)
                         (cons (car methods) (refine (cdr methods))))))))))
      (filter (lambda (m) (equal? (car (method-record-class m)) class))
              (refine methods))))

  
  ;generate-dynamic-names: (list method) (list method)-> (list (list string method))
  (define (generate-dynamic-names methods overridden-methods)
    (map (lambda (method)
           (list (java-name->scheme (id-string (method-name method)))
                 method))
         (refine-method-list-old methods overridden-methods)))
 
  ;refine-method-list-old: (list method) (list method) -> (list method)
  (define (refine-method-list-old methods overridden-methods)
    (if (null? methods) 
        methods
        (let ((overloaded-removed 
               (filter (lambda (method)
                         (not (equal? (id-string (method-name (car methods)))
                                      (id-string (method-name method)))))
                       (cdr methods))))
          (cond
            ((> (length (cdr methods))
                (length overloaded-removed))
             (refine-method-list-old overloaded-removed overridden-methods))
            ((memq (car methods) overridden-methods)
             (refine-method-list-old (cdr methods) overridden-methods))
            ((eq? 'ctor (method-record-rtype (method-rec (car methods))))
             (refine-method-list-old (cdr methods) overridden-methods))
            (else (cons (car methods) (refine-method-list-old (cdr methods) overridden-methods)))))))
    
  ;generate-dyn-method-defs: (list (list string method)) -> (list syntax)
  (define (generate-dyn-method-defs methods)
    (map (lambda (name-method)
           (let ((args (map (lambda (arg)
                              (build-identifier (id-string (var-decl-name arg))))
                            (method-parms (cadr name-method)))))
             (create-syntax 
              #f
              `(define/public (,(build-identifier (car name-method)) ,@args)
                 (,(build-identifier (mangle-method-name (id-string (method-name (cadr name-method)))
                                                         (method-record-atypes (method-rec (cadr name-method)))))
                   ,@args))
              #f)))
         (filter (lambda (name-method)
                   (not (equal? (car name-method) (id-string (method-name (cadr name-method))))))
                 methods)))
  
  ;build-method-table: (list method) (list symbol) -> sexp
  (define (build-method-table methods generics)
    `(let ((table (make-hash-table)))
       (for-each (lambda (method generic)
                   (hash-table-put! table (string->symbol method) generic))
                 (list ,@(map (lambda (m)
                               (mangle-method-name (id-string (method-name m))
                                                   (method-record-atypes (method-rec m))))
                             methods))
                 (list ,@generics))
       table))
  
  ;build-field-table: (string->string) symbol accesses -> sexp
  (define (build-field-table maker type fields)
    `(let ((table (make-hash-table)))
       (for-each (lambda (field field-method)
                   (hash-table-put! table (string->symbol field) field-method))
                 ,@(let ((non-private-fields (map (lambda (n) (id-string (field-name n)))
                                                  (append (accesses-public fields)
                                                          (accesses-package fields)
                                                          (accesses-protected fields))))
                         (private-fields (map (lambda (n) (id-string (field-name n)))
                                              (accesses-private fields))))
                     (list `(list ,@(append non-private-fields private-fields))
                           `(list ,@(append
                                     (map (lambda (n) (build-identifier (maker n))) non-private-fields)
                                     (map (if (eq? 'get type)
                                              (lambda (n) 
                                                `(lambda (class-obj)
                                                   (send class-obj ,(build-identifier (maker n)))))
                                              (lambda (n)
                                                `(lambda (class-obj new-val)
                                                   (send class-obj ,(build-identifier (maker n)) new-val))))
                                          private-fields))))))
       table))

                      
  ;generate-inner-makers: (list def) int type-records -> (list syntax)
  (define (generate-inner-makers defs depth type-recs)
    (apply append
           (map (lambda (d) (build-inner-makers d depth type-recs)) 
                (filter (lambda (d) (not (memq (def-kind d) '(anonymous statement)))) defs))))
  
  ;build-inner-makers: def int type-records -> (list syntax)
  (define (build-inner-makers def depth type-recs)
    (let* ((class-name (id-string (def-name def)))
           (ctor-name (string-append "construct-" class-name))
           (parms (map method-parms (get-ctors (def-members def) type-recs))))
      (map (build-inner-maker class-name ctor-name depth type-recs) parms)))
  
  ;build-inner-maker: string string int type-records -> ((list field) -> syntax)
  (define (build-inner-maker class-name ctor-name depth type-recs)
    (lambda (parms)
      (let ((translated-parms (translate-parms parms))
            (encls-this (reverse (let loop ((d depth))
                                   (cond
                                     ((= d 0) null)
                                     (else (cons (string->symbol (format "encl-this-~a~~f" d))
                                                 (loop (sub1 d))))))))
            (parm-types (map field-type #;(lambda (p) (type-spec-to-type (field-type-spec p) #f 'full type-recs)) parms)))
        (make-syntax #f
                     `(define/public (,(build-identifier (mangle-method-name ctor-name parm-types)) ,@translated-parms)
                        (let ((temp-obj (make-object ,(build-identifier class-name)
                                          this ,@encls-this)))
                          (send temp-obj ,(build-identifier (build-constructor-name class-name parm-types))
                                ,@translated-parms)
                          temp-obj))
                     #f))))

  ;get-ctors: (list member) -> (list method)
  (define (get-ctors members type-recs)
    (filter
     (lambda (member)
       (and (method? member)
            (eq? 'ctor (type-spec-to-type (method-type member) #f 'full type-recs))))
     members))  
  
  ;Code to separate different member types for easier access
  ;(make-accesses (list member) (list member) (list member) ...)
  (define-struct accesses (private protected static public package private-static))
  ;(make-members (list method) (list field) (list init) (list init) (list def) (list def))
  (define-struct members (method field static-init init nested inner))
  
  ;update: ('a 'b -> void) 'a ('b -> (list 'a)) 'b) -> 'b 
  ;Allows a set! to be passed in and applied
  (define (update set add-on access struct)
    (set struct (cons add-on (access struct)))
    struct)
  
  ;separate-members: (list member) -> members
  (define (separate-members members)
    (letrec ((my-members (make-members null null null null null null))
             (separate
              (lambda (m h)
                (cond
                  ((null? m) h)
                  ((method? (car m))
                   (separate (cdr m) (update set-members-method! (car m) members-method h)))
                  ((field? (car m))
                   (separate (cdr m) (update set-members-field! (car m) members-field h)))
                  ((initialize? (car m))
                   (separate (cdr m)
                             (if (initialize-static (car m))
                                 (update set-members-static-init! (car m) members-static-init h)
                                 (update set-members-init! (car m) members-init h))))
                  ((def? (car m))
                   (separate (cdr m)
                             (if (or (interface-def? (car m))
                                     (memq 'static (map modifier-kind (header-modifiers (def-header (car m))))))
                                 (update set-members-nested! (car m) members-nested h)
                                 (update set-members-inner! (car m) members-inner h))))
                  (else (error 'separate "not something expected: ~e" (car m)))))))
      (separate members my-members)))
  
  ;make-access-separator: ('a -> (list symbol)) -> ((list 'a) accesses -> accesses)
  (define (make-access-separator get-modifiers)
    (letrec ((separate
              (lambda (m h)
                (if (null? m) h
                    (separate (cdr m)
                              (let* ((current (car m))
                                     (modifiers (map modifier-kind (get-modifiers current))))
                                (cond
                                  ((private? modifiers)
                                   (if (static? modifiers)
                                       (update set-accesses-private-static! current accesses-private-static h) 
                                       (update set-accesses-private! current accesses-private h)))
                                  ((static? modifiers) (update set-accesses-static! current accesses-static h))
                                  ((protected? modifiers) (update set-accesses-protected! current accesses-protected h))
                                  ((public? modifiers) (update set-accesses-public! current accesses-public h))
                                  (else (update set-accesses-package! current accesses-package h)))))))))
      separate))
  
  ;separate-methods: (list method) accesses -> accesses
  (define separate-methods (make-access-separator method-modifiers))  
  ;separate-fields: (list field) accesses -> accesses
  (define separate-fields (make-access-separator field-modifiers))
  
  ;get-parent: name -> (values string src)
  (define get-parent
    (lambda (parent)
      (when (= (length parent) 1) (set! parent (car parent)))
      (when (null? parent) (set! parent (make-name (make-id "Object" #f) null #f)))
      (if (null? (name-path parent))
          (values (id-string (name-id parent)) (id-src (name-id parent)))
          (values (string-append (apply string-append (map (lambda (p)
                                                             (format "~s." p))
                                                           (map id-string (name-path parent))))
                                 (id-string (name-id parent)))
                  (name-src parent)))))
  
  ;create-local-names: (list symbol) -> syntax
  (define (create-local-names names)
    (create-syntax #f  `(define-local-member-name ,@names) #f))
  
  ;translate-parents: (list name) -> (list syntax)
  (define (translate-parents extends)
    (map (lambda (n)
           (if (null? (name-path n))
               (translate-id (id-string (name-id n))
                             (id-src (name-id n)))
               (create-syntax #f (build-identifier (append (map id-string (name-path n))
                                                           (list (id-string (name-id n)))))
                              (build-src (name-src n)))))
         extends))
  
  (define (get-srcs stmt) 
    (cond
      [(ifS? stmt)
       (append (get-expr-srcs (ifS-cond stmt))
               (get-srcs (ifS-then stmt))
               (get-srcs (ifS-else stmt)))]
      [(throw? stmt)
       (get-expr-srcs (throw-expr stmt))]
      [(return? stmt)
       (get-expr-srcs (return-expr stmt))]
      [(while? stmt)
       (append (get-expr-srcs (while-cond stmt))
               (get-srcs (while-loop stmt)))]
      [(doS? stmt)
       (append (get-srcs (doS-loop stmt))
               (get-expr-srcs (doS-cond stmt)))]
      [(for? stmt)
       (get-srcs (for-loop stmt))]
      [(try? stmt)
       (append (get-srcs (try-body stmt))
               (apply append
                      (map (compose get-srcs catch-body) (try-catches stmt))))
       ]
      [(block? stmt)
       (apply append (map get-srcs (block-stmts stmt)))]
      [(statement-expression? stmt) (get-expr-srcs stmt)]
      [else null]))
  
  (define (get-expr-srcs expr)
    (cond
      ((not (expr-src expr)) null)
      ((bin-op? expr) (cons (src-pos (expr-src expr))
                            (append (get-expr-srcs (bin-op-left expr))
                                    (get-expr-srcs (bin-op-right expr)))))
      ((access? expr)
       (if (or (local-access? (access-name expr))
               (not (field-access-object (access-name expr))))
           (list (src-pos (expr-src expr)))
           (cons (src-pos (expr-src expr))
                 (get-expr-srcs (field-access-object (access-name expr))))))
      ((call? expr)
       (cons (src-pos (expr-src expr))
             (append
              (if (call-expr expr)
                  (get-expr-srcs (call-expr expr))
                  null)
              (apply append
                     (map get-expr-srcs (call-args expr))))))
      ((class-alloc? expr)
       (cons (src-pos (expr-src expr))
             (apply append (map get-expr-srcs (class-alloc-args expr)))))
      ((array-alloc? expr)
       (cons (src-pos (expr-src expr))
             (apply append
                    (map get-expr-srcs (array-alloc-size expr)))))
      ((cond-expression? expr)
       (cons (src-pos (expr-src expr))
             (append (get-expr-srcs (cond-expression-cond expr))
                     (get-expr-srcs (cond-expression-then expr))
                     (get-expr-srcs (cond-expression-else expr)))))
      ((array-access? expr)
       (cons (src-pos (expr-src expr))
             (append (get-expr-srcs (array-access-name expr))
                     (get-expr-srcs (array-access-index expr)))))
      ((post-expr? expr)
       (cons (src-pos (expr-src expr))
             (get-expr-srcs (post-expr-expr expr))))
      ((pre-expr? expr)
       (cons (src-pos (expr-src expr))
             (get-expr-srcs (pre-expr-expr expr))))
      ((unary? expr)
       (cons (src-pos (expr-src expr))
             (get-expr-srcs (unary-expr expr))))
      ((cast? expr)
       (cons (src-pos (expr-src expr))
             (get-expr-srcs (cast-expr expr))))
      ((instanceof? expr)
       (cons (src-pos (expr-src expr))
             (get-expr-srcs (instanceof-expr expr))))
      ((assignment? expr)
       (cons (src-pos (expr-src expr))
             (get-expr-srcs (assignment-right expr))))
      (else (list (src-pos (expr-src expr))))))
  
  (define (srcs->spans srcs)
    (cond
      [(null? srcs) (int-set:make-range)]
      [else (int-set:union (int-set:make-range (src-pos (car srcs))
                                               (+ (src-pos (car srcs)) (src-span (car srcs))))
                           (srcs->spans (cdr srcs)))]))
  
  ;translate-interface: interface-def type-records-> (list syntax)
  (define (translate-interface iface type-recs)
    (let* ((header (def-header iface))
           (name (build-identifier (id-string (header-id header))))
           (syntax-name (translate-id (id-string (header-id header))
                                      (id-src (header-id header))))
           (source (build-src (def-src iface)))
           (interface (create-syntax #f 'interface (build-src (def-key-src iface))))
           (members (separate-members (def-members iface))))
      
      (loc (def-file iface))
      (class-name (id-string (header-id header)))
      (send type-recs set-location! (loc))
      
      (let* ((static-field-names (map build-identifier (make-static-field-names (members-field members))))
             (provides `(provide ,name ,@static-field-names
                                 ,@(map build-identifier (list (format "guard-convert-~a" (class-name))
                                                               (format "convert-assert-~a" (class-name))
                                                               (format "wrap-convert-assert-~a" (class-name))
                                                               (format "dynamic-~a/c" (class-name))
                                                               (format "static-~a/c" (class-name)))))))
        
        (list `(,provides
                (define ,syntax-name (,interface ,(translate-parents (header-extends header))
                                                 ,@(make-iface-method-names (members-method members))))
                ,@(create-static-fields static-field-names (members-field members))
                ,@(append (generate-wrappers (class-name)
                                             "Object"
                                             (append
                                              (class-record-methods 
                                               (send type-recs get-class-record (list (class-name))))
                                              (class-record-methods
                                               (send type-recs get-class-record (list "Object" "java" "lang"))))
                                             null)
                          (generate-contract-defs (class-name)))
                )
              (make-syntax #f `(module ,name mzscheme (require ,(module-require)) ,provides) #f)))))

  ;-----------------------------------------------------------------------------------------------------------------
  ;Member translation functions
  
  ;translate-inner-class: def type-records int -> (U syntax (list syntax syntax))

  
  ;------------------------------------------------------------
  ;;Method translation functions
  
  ;override?: symbol type-records -> bool
  (define (override? method-name type-recs)
    (let* ((internal-error 
            (lambda () (error 'override "Internal Error class or it's parent not in class record table")))
           (class-record 
            (get-record (send type-recs get-class-record 
                              (make-ref-type (class-name)
                                             (send type-recs lookup-path (class-name) (lambda () null)))
                              #f
                              internal-error) type-recs))
           (parent-record (send type-recs get-class-record  
                                (car (class-record-parents class-record)) #f internal-error)))
      (memq method-name
            (map (lambda (m) (string->symbol (mangle-method-name (method-record-name m)
                                                                 (method-record-atypes m))))
                 (class-record-methods parent-record)))))
  
  ;get-overridden-names: (list method) -> (list method)
  (define (get-overridden-methods methods) 
    (filter (lambda (m) 
              (let ((mname (id-string (method-name m))))
                (and (method-record-override (method-rec m))
                     (hash-table-put! (class-override-table) 
                                      (build-identifier 
                                       ((if (constructor? mname) build-constructor-name mangle-method-name)
                                        mname
                                        (method-record-atypes (method-rec m)))) #t))))
            methods))
  
  ;create-generic-methods: (list method) -> (list syntax)
  (define (create-generic-methods methods)
    (map (lambda (method)
           (let* ((m-name (id-string (method-name method)))
                  (name ((if (eq? 'ctor (method-record-rtype (method-rec method)))
                             build-constructor-name mangle-method-name)
                         m-name
                         (method-record-atypes (method-rec method)))))
             (make-syntax #f `(define ,(build-identifier (build-generic-name (class-name) name))
                                (generic ,(build-identifier (class-name)) ,(build-identifier name)))
                          (build-src (method-src method)))))
         methods))
  
  ;make-iface-method-names: (list method) -> (list symbol)
  (define (make-iface-method-names methods)
    (letrec ((mangle-name (lambda (method)
                            (build-identifier
                             (mangle-method-name (method-record-name (method-rec method))
                                                 (method-record-atypes (method-rec method))))))
             (maker
              (lambda (methods)
                (cond
                  ((null? methods) methods)
                  ((method-record-override (method-rec (car methods)))
                   (maker (cdr methods)))
                  (else (cons (mangle-name (car methods)) (maker (cdr methods))))))))
      (maker methods)))
  
  ;make-method-names: (list methods) (list methods) -> (list symbol)
  (define (make-method-names methods minus-methods)
    (if (null? methods)
        null
        (if (memq (car methods) minus-methods)
            (make-method-names (cdr methods) minus-methods)
            (cons 
             (build-identifier ((if (constructor? (id-string (method-name (car methods))))
                                    build-constructor-name
                                    mangle-private-method)
                                (id-string (method-name (car methods)))
                                (method-record-atypes (method-rec (car methods)))))
             (make-method-names (cdr methods) minus-methods)))))
  
  
  ;translate-method: type-spec (list symbol) id (list parm) statement bool 
  ;                  src bool int method-record type-records -> syntax
  (define (translate-method type modifiers id parms block all-tail? src inner? depth rec type-recs)
    (let* ((final (final? modifiers))
           (ctor? (eq? 'ctor (method-record-rtype rec)))
           (priv? (private? modifiers))
           (method-string ((cond
                             [ctor? build-constructor-name]
                             [priv? mangle-private-method]
                             [else mangle-method-name])
                           (id-string id)
                           (method-record-atypes rec)))
           (method-name (translate-id method-string (id-src id)))
           (over? (method-record-override rec))
           (definition (cond
                         ((and over? final) 'define/override-final)
                         (over? 'define/override)
                         (final 'define/public-final)
                         (else 'define/public))))
      (unless (static? modifiers) (current-depth depth))
      (current-local-classes null)
      (create-syntax #f
                     `(,definition ,method-name 
                        ,(translate-method-body method-string parms block modifiers type 
                                                all-tail? ctor? inner? depth type-recs))
                     (build-src src))))
  
  ;make-static-method-names: (list method) type-recs -> (list string)
  (define (make-static-method-names methods type-recs)
    (map (lambda (m)
           (build-static-name 
            ((if (memq 'private (method-record-modifiers (method-rec m)))
                 mangle-private-method mangle-method-name)
             (id-string (method-name m))
             (method-record-atypes (method-rec m)))))
         methods))
  
  ;create-static-methods: (list string) (list method) type-records -> (list syntax)
  (define (create-static-methods names methods type-recs)
    (if (null? names)
        null
        (let ((name (car names))
              (method (car methods)))
          (cons (create-syntax #f
                               `(define ,(translate-id name (id-src (method-name method)))
                                  ,(translate-method-body name
                                                          (method-parms method)
                                                          (method-body method)
                                                          (map modifier-kind (method-modifiers method))
                                                          (method-type method)
                                                          (method-all-tail? method)
                                                          #f
                                                          #f
                                                          0
                                                          type-recs))
                               (build-src (method-src method)))
                (create-static-methods (cdr names) (cdr methods) type-recs)))))
  
  (define static-method (make-parameter #f))
  (define inner-class (make-parameter #f))
  
  ;translate-method-body: string (list field) statement (list symbol) type-spec bool bool bool int type-record -> syntax
  (define (translate-method-body method-name parms block modifiers rtype all-tail? ctor? inner? depth type-recs)
    (let ((parms (translate-parms parms))
          (void? (eq? (type-spec-name rtype) 'void))
          (native? (memq 'native modifiers))
          (static? (memq 'static modifiers))
          (native-method-name 
           (build-identifier 
            (string-append (regexp-replace "private-" method-name "") "-native"))))
            
      (static-method static?)
      (make-syntax #f
                   (cond
                     ((and block void?)
                      `(lambda ,parms 
                         (let/ec return-k
                           ,(translate-statement block type-recs)
                           (void))))
                     ((and block (not void?) all-tail?)
                      `(lambda ,parms
                         ,(translate-statement block type-recs)))
                     ((and block (not void?))
                      `(lambda ,parms
                         (let/ec return-k
                           ,(translate-statement block type-recs))))
                     ((and (not block) (memq 'abstract modifiers))
                      `(lambda ,parms (void)))
                     ((and (not block) native? void? (not static?))
                      `(lambda ,parms
                         (,native-method-name
                           this field-accessors field-setters private-methods ,@parms)
                         (void)))
                     ((and (not block) native? (not static?))
                      `(lambda ,parms
                         (,native-method-name
                           this field-accessors field-setters private-methods ,@parms)))
                     ((and (not block) native? void? static?)
                      `(lambda ,parms
                         (,native-method-name ,@parms)
                         (void)))
                     ((and (not block) native? static?)
                      `(lambda ,parms
                         (,native-method-name ,@parms))))
                   #f)))
  
  ;translate-parms: (list field) -> (list syntax)
  (define (translate-parms parms)
    (map (lambda (parm)
           (translate-id (build-var-name (id-string (field-name parm)))
                         (id-src (field-name parm))))
         parms))

  ;----------------------------------------------------------------
  ;Field translation functions
  
  ;make-field-accessor-names: (list fields) -> (list symbol)
  (define (make-field-accessor-names fields)
    (if (null? fields)
        null
        (let ((name (id-string (field-name (car fields)))))
          (append (cons (create-get-name name)
                        (if (final? (map modifier-kind (field-modifiers (car fields))))
                            null
                            (list (create-set-name name))))
                  (make-field-accessor-names (cdr fields))))))
  
  (define (create-field-accessors names fields)
    (if (null? fields)
        null
        (let* ((field (car fields))
               (class (build-identifier (class-name)))
               (field-name (id-string (field-name field)))
               (quote-name (build-identifier (build-var-name field-name)))
               (getter (car names))
               (setter (cadr names))
               (final (final? (map modifier-kind (field-modifiers field)))))
          (append (cons (make-syntax #f
                                     `(define ,getter
                                        (let ((normal-get (class-field-accessor ,class ,quote-name)))
                                          (lambda (obj)
                                            (cond
                                              ((is-a? obj ,class) (normal-get obj))
                                              (else 
                                               (send obj 
                                                     ,(build-identifier (format "~a-wrapped" getter))))))))
                                     #f)
                        (if (not final)
                            (list 
                             (make-syntax #f 
                                          `(define ,setter
                                             (let ((normal-set (class-field-mutator ,class ,quote-name)))
                                               (lambda (obj val)
                                                 (if (is-a? obj ,class)
                                                     (normal-set obj val)
                                                     (send obj ,(build-identifier (format "~a-wrapped" setter)) val)))))
                                          #f))
                            null))
                  (create-field-accessors (if final (cdr names) (cddr names)) (cdr fields))))))
  
  (define (make-static-field-setters-names fields) 
    (map (lambda (f) (create-set-name (id-string (field-name f)))) fields))
  
  (define (create-static-setters names fields)
    (if (null? names)
        null
        (let ((name (car names))
              (field (car fields)))
          (cons (make-syntax #f
                             `(define (,name my-val)
                                (set! ,(build-identifier (build-var-name (build-static-name (id-string (field-name field))))) 
                                      my-val))
                             #f)
                (create-static-setters (cdr names) (cdr fields))))))
  
  ;create-private-setters/getters fields -> (list syntax)
  (define (create-private-setters/getters fields)
    (if (null? fields)
        null
        (let* ((field (car fields))
               (s-name (id-string (field-name field)))
               (name (build-identifier (build-var-name s-name)))
               (getter (create-get-name s-name))
               (setter (create-set-name s-name)))
          (append (list (make-syntax #f `(define/public (,getter my-val) ,name) (build-src (id-src (field-name field))))
                        (make-syntax #f `(define/public (,setter m-obj my-val) (set! ,name my-val))
                                     (build-src (id-src (field-name field)))))
                  (create-private-setters/getters (cdr fields))))))
  
  ;make-static-fiel-names: (list field) -> (list string)
  (define (make-static-field-names fields)
    (map (lambda (f) (build-static-name (build-var-name (id-string (field-name f))))) fields))

  ;create-static-fields: (list string) (list field) -> (list syntax)
  (define (create-static-fields names fields)
    (if (null? names)
        null
        (let ((name (car names))
              (f (car fields)))
          (cons (make-syntax #f
                             `(define ,(translate-id name (id-src (field-name f))) 
                                ,(cadr (translate-field-body (and (var-init? f) f) (field-type-spec f))))
                             (build-src (if (var-init? f) (var-init-src f) (var-decl-src f))))
                (create-static-fields (cdr names) (cdr fields))))))
  
  ;translate-field: (list symbol) type-spec id (U #f var-init) src bool -> (list syntax)
  (define (translate-field access type name init? src static?)
    (let ((values (translate-field-body init? type))
          (field-name (translate-id (build-var-name (if static? (build-static-name (id-string name)) (id-string name)))
                                    (id-src name))))
      (list (if (or static? (private? access))
                (make-syntax #f `(define ,field-name ,(car values)) (build-src src))
                (make-syntax #f `(field (,field-name ,(car values))) (build-src src)))
            (create-syntax #f `(set! ,field-name ,(cadr values)) #f))))
  
  ;translate-field-body (U bool var-init) type-spec -> (list syntax)
  (define (translate-field-body init? type)
    (let ((default-val (get-default-value type)))
      (list 
       default-val
       (cond 
         (init?
          (let ((actual-type (if (array-init? (var-init-init init?))
                                 'dynamic ;Problem: array type needed here
                                 (expr-types (var-init-init init?))))             
                (body-syntax (if (array-init? (var-init-init init?))
                                 (initialize-array (array-init-vals (var-init-init init?))
                                                   type)
                                 (translate-expression (var-init-init init?)))))
            (cond
              ((or (eq? 'dynamic (field-type init?))
                   (dynamic-val? (field-type init?)))
               (make-syntax #f (guard-convert-value body-syntax actual-type) body-syntax))
              ((and (memq (field-type init?) '(float double))
                    (memq actual-type '(long int byte short)))
               (make-syntax #f `(exact->inexact ,body-syntax) body-syntax))
              ((and (eq? actual-type 'char) (memq (field-type init?) '(byte short int long)))
               (make-syntax #f `(char->integer ,body-syntax) body-syntax))
              ((and (eq? actual-type 'char) (memq (field-type init?) '(float double)))
               (make-syntax #f `(exact->inexact (char->integer ,body-syntax)) body-syntax))
              (else body-syntax))))
         (else default-val)))))
  
  ;translate-initialize: bool block src string type-records -> syntax
  (define (translate-initialize static? body src type-recs)
    (translate-block (block-stmts body) (block-src body) type-recs))
  
  
  ;-------------------------------------------------------------------------------------------------------------------------
  ;translate-statement
  ;translates a Java statement into a Scheme expresion. 
  ;raises an error if it has no implementation for a statement type
  
  ;Converted
  ;translate-statement: statement string type-records -> syntax
  (define (translate-statement statement type-recs)
    (cond
      ((ifS? statement)
       (translate-if (translate-expression (ifS-cond statement))
                     (translate-statement (ifS-then statement) type-recs)
                     (if (ifS-else statement)
                         (translate-statement (ifS-else statement) type-recs)
                         'void)
                     (ifS-key-src statement)
                     (ifS-src statement)))
      ((throw? statement)
       (translate-throw (translate-expression (throw-expr statement))
                        (throw-key-src statement)
                        (throw-src statement)))
      ((return? statement)
       (translate-return (if (return-expr statement)
                             (translate-expression (return-expr statement))
                             (make-syntax #f '(void) #f))
                         (and (return-expr statement)
                              (expr-types (return-expr statement)))
                         (return-exp-type statement)
                         (return-in-tail? statement)
                         (return-src statement)))
      ((while? statement)
       (translate-while (translate-expression (while-cond statement))
                        (translate-statement (while-loop statement) type-recs)
                        (while-src statement)))
      ((doS? statement)
       (translate-do (translate-statement (doS-loop statement) type-recs)
                     (translate-expression (doS-cond statement))
                     (doS-src statement)))
      ((for? statement)
       (translate-for (for-init statement)
                      (translate-expression (for-cond statement))
                      (map translate-expression (for-incr statement))
                      (translate-statement (for-loop statement) type-recs)
                      (for-src statement)
                      type-recs))
      ((try? statement)
       (translate-try (translate-statement (try-body statement) type-recs)
                      (try-catches statement)
                      (and (try-finally statement)
                           (translate-statement (try-finally statement) type-recs)) 
                      (try-key-src statement)
                      (try-src statement)
                      type-recs))
      ((switch? statement)
       (translate-switch (translate-expression (switch-expr statement))
                         (switch-cases statement)
                         (switch-src statement)
                         type-recs))
      ((block? statement)
       (translate-block (block-stmts statement) (block-src statement) type-recs))        
      ((def? statement)
       (current-local-classes (cons statement (current-local-classes)))
       (create-syntax #f '(void) #f))
      ((break? statement)
       (translate-break (break-label statement)  (break-src statement)))
      ((continue? statement)
       (translate-continue (continue-label statement) (continue-src statement)))
      ((label? statement)
       (translate-label (label-label statement)
                        (translate-statement (label-stmt statement) type-recs)
                        (label-src statement)))
      ((synchronized? statement)
       (translate-synchronized (translate-expression (synchronized-expr statement))
                               (translate-statement (synchronized-stmt statement) type-recs)
                               (synchronized-src statement)))        
      ((statement-expression? statement)
       (translate-expression statement))
      (else
       (error 'translate-statement (format "translate-statement given unsupported: ~s" statement)))))
  
  
  ;Converted
  ;translate-if: syntax syntax syntax src src -> syntax
  (define (translate-if if? then else key src)
    (create-syntax #f `(,(create-syntax #f `if (build-src key)) ,if? ,then ,else) (build-src src)))
  
  ;Converted
  ;translate-throw: syntax src src -> syntax
  (define translate-throw
    (lambda (expr key src)
      (create-syntax #f `(let* ((obj ,expr)
                                (exn (make-java:exception
                                      (send (send obj |getMessage|) get-mzscheme-string)
                                      (current-continuation-marks) obj)))
                           (send obj set-exception! exn)
                           (,(create-syntax #f 'raise (build-src key)) exn))
                     (build-src src))))
  
  ;return -> call to a continuation 
  ;Presently a no-op in the interactions window, although this is incorrect for advanced and full
  ;translate-return: syntax type type bool src -> syntax
  (define (translate-return expr expr-type exp-type in-tail? src)
    (let ((expr (cond
                  ((and expr-type (eq? 'dynamic exp-type))
                   (guard-convert-value expr expr-type))
                  ((and expr-type
                        (memq exp-type '(float double))
                        (memq expr-type '(long int short byte)))
                   (make-syntax #f `(exact->inexact ,expr) expr))
                  (else expr))))
      (if (or (interactions?) in-tail?)
          (make-syntax #f expr #f)
          (make-syntax #f `(return-k ,expr) (build-src src)))))
  
  ;translate-while: syntax syntax src -> syntax
  (define (translate-while cond body src)
    (make-syntax #f `(let/ec loop-k
                       (let loop ((dummy #f))
                         (when ,cond
                           ,body
                           (loop #f))))
                 (build-src src)))

  ;translate-do: syntax syntax src -> syntax
  (define (translate-do body cond src)
    (make-syntax #f `(let/ec loop-k
                       (let loop ((dummy #f))
                         ,body
                         (when ,cond (loop #f))))
                 (build-src src)))
  
  ;translate-for: (U (list statement) (list field)) syntax (list syntax) syntax src type-records-> syntax
  (define (translate-for init condi incr body src type-recs)
    (let ((loop `(let/ec loop-k
                   (let loop ((continue? #f))
                     (when continue? ,@(if (null? incr) '((void)) incr))
                     (when ,condi 
                       ,body
                       ,@incr
                       (loop #f)))))
          (source (build-src src)))
      (if (and (pair? init) (field? (car init)))
          (make-syntax #f `(letrec (,@(map (lambda (var)
                                             `(,(translate-id (build-var-name (id-string (field-name var)))
                                                              (id-src (field-name var)))
                                                ,(cond
                                                   ((var-init? var)
                                                    (let ((actual-type
                                                           (if (array-init? (var-init-init var))
                                                               'dynamic ;Problem: need array-type here
                                                               (expr-types (var-init-init var))))
                                                          (var-value
                                                           (if (array-init? (var-init-init var))
                                                               (initialize-array (array-init-vals (var-init-init var)) 
                                                                                 (field-type-spec var))
                                                               (translate-expression (var-init-init var)))))
                                                      (if (or (eq? 'dynamic (field-type var))
                                                              (dynamic-val? (field-type var)))
                                                          (make-syntax #f (guard-convert-value var-value actual-type) var-value)
                                                          var-value)))
                                                   (else (get-default-value (field-type-spec var))))))
                                           init))
                             ,loop) source)
          (make-syntax #f `(begin
                             ,@(map (lambda (s) (translate-statement s type-recs)) init)
                             ,loop)
                       source))))
  
  ;Converted
  ;initialize-array: (list (U expression array-init)) type-spec-> syntax
  (define (initialize-array inits type)
    (cond
      ((null? inits) 
       (make-syntax #f `(make-java-array ,(translate-type-spec type) 0 null) #f))
;       (error 'initialize-array "Given empty list"))
      ((array-init? (car inits))
       (make-syntax #f
                    `(make-java-array ,(translate-type-spec type)
                                      0
                                      (reverse (list ,@(map (lambda (a) (initialize-array (array-init-vals a)
                                                                                          (make-type-spec (type-spec-name type)
                                                                                                          (sub1 (type-spec-dim type))
                                                                                                          (type-spec-src type))))
                                                            inits))))
                    (build-src (array-init-src (car inits)))))
      (else
       (make-syntax #f
                    `(make-java-array ,(translate-type-spec type)
                                      0
                                      (reverse (list ,@(map translate-expression inits))))
                    (build-src (if (name? (car inits)) (name-src (car inits)) (expr-src (car inits))))))))
  
  ;Converted
  ;translate-try: syntax (list catch) (U syntax boolean) src src type-records-> syntax
  (define translate-try
    (lambda (block catches finally key src type-recs)
      (let* ((handle (create-syntax #f 'with-handlers (build-src key)))
             (handlers (make-syntax #f `(,handle [ ,@(make-predicates catches type-recs) ]
                                         ,block)
                                    (build-src src))))
        (if finally
            (make-syntax #f
                         `(dynamic-wind void
                                        (lambda () ,handlers)
                                        (lambda () ,finally))
                         #f)
            handlers))))
  
  ;Converted
  ;make-predicates: (list catch) type-records-> (list syntax)
  (define make-predicates
    (lambda (catches type-recs)
      (map (lambda (catch)
             (let* ((catch-var (catch-cond catch))
                    (var-src (var-decl-src catch-var))
                    (class-name (get-class-name (field-type-spec catch-var)))
                    (isRuntime? (descendent-Runtime? (field-type-spec catch-var) type-recs))
                    (type 
                     (if isRuntime?
                         (make-syntax #f `exn? (build-src var-src))
                         (make-syntax #f 
                                      `(exception-is-a? ,class-name)
                                      (build-src var-src))))
                    (parm (translate-id (build-var-name (id-string (field-name catch-var)))
                                        (id-src (field-name catch-var))))
                    (block (make-syntax #f
                                        `(lambda (,parm)
                                           ,(translate-statement (catch-body catch) type-recs))
                                        (build-src (catch-src catch)))))
               (make-syntax #f `(,type                       
                                 ,(if isRuntime?
                                      `(lambda (exn)
                                         (if (javaException:supported-runtime-exception? exn)
                                             (,block (javaException:exception-to-class exn))
                                             (raise exn)))
                                      block))
                            (build-src (catch-src catch)))))
           catches)))
  
  ;Determines if the given type represents a class that is a descendent of the RuntimeException class
  ;descendent-Runtime?: type-spec type-records -> bool
  (define descendent-Runtime?
    (lambda (type type-recs)
      (let ((class-record (send type-recs get-class-record (type-spec-to-type type #f 'full type-recs) #f
                                (lambda () (error 'descendent-Runtime "Internal Error: class record is not in table")))))
        (member `("java" "lang" "RuntimeException") (class-record-parents class-record)))))
  
  ;Converted
  ;translate-switch: syntax (list CaseStatements) src type-records -> syntax
  (define translate-switch
    (lambda (expr cases src type-recs)
      (make-syntax #f
                   `(case ,expr
                      ,@(map (lambda (case) 
                               (if (eq? (caseS-constant case) 'default)
                                   (if (null? (caseS-body case))
                                       `(else (void))
                                       `(else ,(translate-block (caseS-body case) (caseS-src case) type-recs)))
                                   `((,(translate-expression (caseS-constant case))
                                      ,(translate-block (caseS-body case) (caseS-src case) type-recs)))))
                             cases))
                   (build-src src))))
  
  ;Converted
  ;translate-block: (list (U Statement (U var-decl var-init))) src type-recs -> syntax
  (define translate-block
    (lambda (statements src type-recs)
      ;(list (U Statement (U var-decl var-init))) -> (list syntax)
      (letrec ((translate
                (lambda (statements)
                  (if (null? statements)
                      null
                      (let ((statement (car statements)))
                        (if (field? statement)
                            (translate-var (car statements) (cdr statements))
                            (cons (translate-statement statement type-recs)
                                  (translate (cdr statements))))))))
               (translate-var
                (lambda (var statements)
                  (let* ((is-var-init? (var-init? var))
                         (id (translate-id (build-var-name (id-string (field-name var))) (id-src (field-name var)))))
                    (list (make-syntax #f 
                                       `(letrec
                                            ((,id ,(cond
                                                     (is-var-init?
                                                      (let ((actual-type (if (array-init? (var-init-init var))
                                                                             'dynamic ;Problem: need array type here
                                                                             (expr-types (var-init-init var))))
                                                            (var-value (if (array-init? (var-init-init var))
                                                                           (initialize-array (array-init-vals (var-init-init var))
                                                                                             (field-type-spec var))
                                                                           (translate-expression (var-init-init var)))))
                                                        (if (or (eq? 'dynamic (field-type var))
                                                                (dynamic-val? (field-type var)))
                                                            (guard-convert-value var-value actual-type)
                                                            var-value)))
                                                     (else (get-default-value (field-type-spec var))))))
                                          ,@(if (null? statements)
                                                (list `(void))
                                                (translate statements)))
                                       (build-src (if is-var-init?
                                                      (var-init-src var)
                                                      (var-decl-src var)))))))))
        (if (null? statements)
            (make-syntax #f `void (build-src src))
            (make-syntax #f `(begin ,@(translate statements)) (build-src src))))))
  
  ;translate-break: (U id #f) src -> syntax
  (define (translate-break id src)
    (if (not id)
        (make-syntax #f `(loop-k (void)) (build-src src))
        (make-syntax #f `(,(translate-id (string-append (id-string id "-k")) (id-src id)) void) (build-src src))))
  
  ;Converted
  ;translate-continue: (U string #f) src -> syntax
  (define (translate-continue id src)
    (if (not id)
        (make-syntax #f `(loop-k (loop #t)) (build-src src))
        (make-syntax #f `(,(translate-id (string-append (id-string id) "-k") (id-src id)) 
                           (,(build-identifier (string-append (id-string id) "-continue"))))
                     (build-src src))))
  
  ;translate-label: id syntax src -> syntax
  ;NOTE: probably does not have correct behavior
  (define translate-label
    (lambda (label stmt src)
      (make-syntax #f
                   `(let/ec ,(translate-id (string-append (id-string label) "-k") (id-src label))
                      (letrec ((,(build-identifier (string-append (id-string label) "-continue"))
                                (lambda () ,stmt)))
                        (,(build-identifier (string-append (id-string label) "-continue")))))
                   (build-src src))))
  
  ;translate-synchronized: syntax syntax src -> syntax
  ;PROBLEM! Does nothing
  (define translate-synchronized
    (lambda (expr stmt src)
      (make-syntax #f
                   `(begin ,expr ,stmt)
                   (build-src src))))

  ;------------------------------------------------------------------------------------------------------------------
  ;translate-contract
  ;translates types into contracts

  ;type->contract: type boolean -> sexp
  (define (type->contract type from-dynamic? . stop?)
    (cond
      ((dynamic-val? type) 
       (if (null? stop?)
           (type->contract (dynamic-val-type type) from-dynamic?)
           (type->contract (dynamic-val-type type) from-dynamic? #t)))
      ((symbol? type)
       (case type
         ((int short long byte) 'integer?)
         ((double float) '(c:and/c number? (c:or/c inexact? integer?)))
         ((boolean) 'boolean?)
         ((char) 'char?)
         ((null) 'null?)
         ((string String) 
          (if from-dynamic?
              `string?
              `(c:is-a?/c ,(if (send (types) require-prefix? '("String" "java" "lang") (lambda () #f))
                               'java.lang.String 'String))))
         ((dynamic void) 'c:any/c)))
      ((ref-type? type)
       (if (equal? type string-type)
           (type->contract 'string from-dynamic?)
           `(c:or/c (c:is-a?/c object%) string?)))
      ((unknown-ref? type)
       (if (not (null? stop?))
           `(c:or/c (c:is-a?/c object%) string?)
           (cond
             ((method-contract? (unknown-ref-access type))
              `(c:object-contract (,(string->symbol (java-name->scheme (method-contract-name (unknown-ref-access type))))
                                 ,(type->contract (unknown-ref-access type) from-dynamic?))))
             ((field-contract? (unknown-ref-access type))
              `(c:object-contract (field ,(build-identifier (string-append (field-contract-name (unknown-ref-access type)) "~f"))
                                       ,(type->contract (field-contract-type (unknown-ref-access type)) from-dynamic?)))))))
      ((method-contract? type)
       `(c:-> ,@(map (lambda (a) (type->contract a from-dynamic?)) (method-contract-args type))
              ,(type->contract (method-contract-return type) from-dynamic? #t)))
      ((not type) 'c:any/c)
      ))
  
  ;guard-convert-value syntax type -> sexp
  (define (guard-convert-value val type)
    (cond
      ((dynamic-val? type) val)
      ((symbol? type)
       (case type
         ((int short long byte float double boolean char dynamic void null) val)
         ((string String) `(send ,val get-mzscheme-string))))
      ((ref-type? type)
       (if (equal? type string-type)
           `(send ,val get-mzscheme-string)
           (let ((prefix (if (send (types) require-prefix? (cons (ref-type-class/iface type) (ref-type-path type))
                                   (lambda () #f))
                             (apply string-append (map (lambda (s) (string-append s ".")) (ref-type-path type)))
                             "")))
             `(make-object ,(build-identifier (string-append prefix "guard-convert-" (ref-type-class/iface type)))
                ,val (quote ,(string->symbol (class-name))) '|| #`,val (current-continuation-marks)))))
      (else val)))
  ;convert-assert-value: syntax type -> sexp
  (define (convert-assert-value val type) 
    (cond
      ((dynamic-val? type) (convert-assert-value val (dynamic-val-type type)))
      ((symbol? type)
       (case type
         ((int short long byte float double boolean char dynamic void) val)
         ((string String)
          `(let ((val ,val))
             (if (string? val)
                 (make-java-string val)
                 (raise (make-exn:fail (format "~a broke infered contract here: expected String received ~a"
                                               ,(class-name) val)
                                       (current-continuation-marks))))))))
      ((unknown-ref? type)
       `(let ((val ,val))
          (if (string? val)
              (make-java-string val)
              (c:contract ,(type->contract type #t) val '|| (quote ,(string->symbol (class-name)))))))
      ((ref-type? type)
       (cond
         ((equal? type string-type)
          (convert-assert-value val 'string))
         (else 
          (let ((prefix (if (send (types) require-prefix? (cons (ref-type-class/iface type) (ref-type-path type))
                                  (lambda () #f))
                            (apply string-append (map (lambda (s) (string-append s ".")) (ref-type-path type)))
                            "")))
            `(,(build-identifier (string-append prefix "wrap-convert-assert-" (ref-type-class/iface type)))
               ,val (quote ,(string->symbol (class-name))) '||  #`,val (current-continuation-marks))))))
      (else val)))
    
  ;------------------------------------------------------------------------------------------------------------------------
  ;translate-expression
  ;translates a Java expression into a Scheme expression.
  ;raises an error if it has no implementation for an expression type
  
  (define (translate-expression expr)
    (let ((translated-expr (translate-expression-unannotated expr)))
      (if (and (not (to-file)) (coverage?) (not (check? expr)) (expr-src expr))
          (make-syntax #f `(begin0 ,translated-expr
                                   (cond 
                                     ((namespace-variable-value 'current~test~object% #f (lambda () #f))
                                      => (lambda (test)
                                           (send test analyze-position (quote ,(src->list (expr-src expr))))))))
                       #f)
          translated-expr)))
  
  ;translate-expression: Expression -> syntax
  (define (translate-expression-unannotated expr)
    (cond
      ((literal? expr) (translate-literal (expr-types expr)
                                          (literal-val expr)
                                          (expr-src expr)))
      ((bin-op? expr) (translate-bin-op (bin-op-op expr)
                                        (translate-expression (bin-op-left expr))
                                        (expr-types (bin-op-left expr))
                                        (translate-expression (bin-op-right expr))
                                        (expr-types (bin-op-right expr))
                                        (bin-op-key-src expr)
                                        (expr-src expr)
                                        (expr-types expr)))
      ((access? expr) (translate-access (access-name expr)
                                        (expr-types expr)
                                        (expr-src expr)))
      ((special-name? expr) (translate-special-name (special-name-name expr)
                                                    (expr-src expr)))
      ((specified-this? expr) (translate-specified-this (specified-this-var expr) (expr-src expr)))
      ((call? expr) (translate-call (call-expr expr)
                                    (call-method-name expr)
                                    (map translate-expression (call-args expr))
                                    (map expr-types (call-args expr))
                                    (call-method-record expr)
                                    (expr-types expr)
                                    (expr-src expr)))
      ((class-alloc? expr) (translate-class-alloc (class-alloc-name expr)
                                                  (map expr-types (class-alloc-args expr))
                                                  (map translate-expression (class-alloc-args expr))
                                                  (expr-src expr)
                                                  (class-alloc-class-inner? expr)
                                                  (class-alloc-local-inner? expr)
                                                  (class-alloc-ctor-record expr)))
      ((inner-alloc? expr) (translate-inner-alloc (translate-expression (inner-alloc-obj expr))
                                                  (inner-alloc-name expr)
                                                  (map translate-expression (inner-alloc-args expr))
                                                  (expr-src expr)
                                                  (inner-alloc-ctor-record expr)))
      ((array-alloc? expr)(translate-array-alloc (array-alloc-name expr)
                                                 (map translate-expression (array-alloc-size expr))
                                                 (expr-src expr)))
      ((array-alloc-init? expr)(translate-array-alloc-init (array-alloc-init-name expr)
                                                           (array-alloc-init-dim expr)
                                                           (array-alloc-init-init expr)
                                                           (expr-src expr)))
      ((cond-expression? expr) (translate-cond (translate-expression (cond-expression-cond expr))
                                               (translate-expression (cond-expression-then expr))
                                               (translate-expression (cond-expression-else expr))
                                               (expr-src expr)))
      ((array-access? expr) (translate-array-access (translate-expression (array-access-name expr))
                                                    (translate-expression (array-access-index expr))
                                                    (expr-src expr)))
      ((post-expr? expr) (translate-post-expr (translate-expression-unannotated (post-expr-expr expr))
                                              (post-expr-expr expr)
                                              (post-expr-op expr)
                                              (post-expr-key-src expr)
                                              (expr-src expr)))
      ((pre-expr? expr) (translate-pre-expr (pre-expr-op expr)
                                            (translate-expression-unannotated (pre-expr-expr expr))
                                            (pre-expr-expr expr)
                                            (pre-expr-key-src expr)
                                            (expr-src expr)))
      ((unary? expr) (translate-unary (unary-op expr)
                                      (translate-expression (unary-expr expr))
                                      (unary-key-src expr)
                                      (expr-src expr)))
      ((cast? expr) (translate-cast (cast-type expr)
                                    (translate-expression (cast-expr expr))
                                    (expr-types (cast-expr expr))
                                    ;(expr-types expr)
                                    (expr-src expr)))
      ((instanceof? expr) (translate-instanceof (translate-expression (instanceof-expr expr))
                                                (instanceof-type expr)
                                                (expr-src expr)))
      ((assignment? expr) (translate-assignment (assignment-left expr)
                                                (assignment-op expr)
                                                (translate-expression (assignment-right expr))
                                                (assignment-right expr)
                                                (expr-types expr)
                                                (assignment-key-src expr)
                                                (expr-src expr)))
      ((check? expr) (translate-check expr))
      (else
       (error 'translate-expression (format "Translate Expression given unrecognized expression ~s" expr)))))
  
  ;All of the following functions translate Java Expressions into syntax.
  ;Straightforward unless otherwise noted
  
  ;translate-literal: symbol value src -> syntax
  (define (translate-literal type value src)
    (let ((make-string  `(let ((temp-obj (make-object String)))
                         (send temp-obj make-mzscheme-string ,value)
                         temp-obj))
          (make-image
           (lambda ()
             `(let  ((temp-obj (make-object ,(if (send (types) require-prefix?
                                                       '("Image" "graphics") (lambda () #f))
                                                 'graphics.Image
                                                 'Image))))
                     (send temp-obj Image-constructor-dynamic ,value)
                     temp-obj))))
      (create-syntax #f 
                     (case type
                       ((float double) (if (inexact? value)
                                           value
                                           (exact->inexact value)))
                       ((char int long boolean) value)
                       ((String string) make-string)
                       ((image) (make-image))
                       ((null) 'null)
                       (else
                        (cond
                          ((eq? type string-type) make-string)
                          ((and (equal? "Image" (ref-type-class/iface type))
                                (equal? '("graphics") (ref-type-path type)))
                           (make-image))
                          (else
                           (error 'translate-literal (format "Translate literal given unknown type: ~s" type))))))
                   (build-src src))))
  
  ;;make-is-test sym -> (type -> bool)
  (define (make-is-test kind)
    (lambda (type)
      (if (dynamic-val? type)
        (eq? (dynamic-val-type type) kind)
        (eq? type kind))))
  
  ;;is-string? type -> bool
  (define is-string? (make-is-test 'string))
  ;;is-int? type -> bool
  (define is-int? (make-is-test 'int))
  ;;is-char? type -> bool
  (define is-char? (make-is-test 'char))
  ;;is-double?
  (define (is-double? type)
    (or ((make-is-test 'float) type)
        ((make-is-test 'double) type)))  
  
  ;translate-bin-op: symbol syntax type syntax type src src type-> syntax
  (define (translate-bin-op op left left-type right right-type key src type)
    (let* ((source (build-src src))
           (key-src (build-src key))
           (op-syntax (create-syntax #f op key-src))
           (left (cond
                   ((is-char? left-type)
                    (make-syntax #f `(char->integer ,left) #f))
                   ((and (dynamic-val? type) (not (memq op '(== != & ^ or && oror))))
                    (create-syntax #f `(c:contract number? ,left (quote ,(string->symbol (class-name))) '||) left))
                   (else left)))
           (right (cond
                    ((is-char? right-type)
                     (make-syntax #f `(char->integer ,right) #f))
                    ((and (dynamic-val? type) (not (memq op '(== != & ^ or && oror))))
                     (create-syntax #f `(c:contract number? ,right (quote ,(string->symbol (class-name))) '||) right))
                    (else right)))
           (result
            (case op
              ;Mathematical operations
              ;PROBLEM! + and - do not take into account the possibility of overflow
              ((+)
               (create-syntax #f
                              (cond 
                                ((and (is-string-type? type) (is-string-type? left-type))
                                 `(send ,left concat-java.lang.String (javaRuntime:convert-to-string ,right)))
                                ((and (is-string-type? type) (is-string-type? right-type))
                                 `(send (javaRuntime:convert-to-string ,left) concat-java.lang.String ,right))
                                ((is-string-type? type)
                                 `(send (javaRuntime:convert-to-string ,left) concat-java.lang.String 
                                        (javaRuntime:convert-to-string ,right)))
                                (else
                                 `(,op-syntax ,left ,right))) source))
              ((- *)
               (create-syntax #f `(,op-syntax ,left ,right) source))
              ((/) 
               (let ((div-op                 
                      (cond
                        ((or (is-double? left-type) (is-double? right-type))
                         'javaRuntime:divide-float)
                        ((or (dynamic-val? left-type) (dynamic-val? right-type))
                         'javaRuntime:divide-dynamic)
                        (else
                         'javaRuntime:divide-int))))
                 (make-syntax #f `(,(create-syntax #f div-op key-src) ,left ,right) source)))
              ((%) (make-syntax #f `(,(create-syntax #f 'javaRuntime:mod key-src) ,left ,right) source))
              ;Shift operations
              ((<< >> >>>) 
               (make-syntax #f 
                            `(,(create-syntax #f 'javaRuntime:shift key-src) (quote ,op) ,left ,right) source))
              ;comparisons
              ((< > <= >=) (make-syntax #f `(,op-syntax ,left ,right) source))
              ((==) 
               (make-syntax #f
                            (cond
                              ((or (dynamic-val? left-type) (dynamic-val? right-type))
                               `(,(create-syntax #f 'javaRuntime:dynamic-equal? key-src) ,left ,right))
                              ((and (prim-numeric-type? left-type) (prim-numeric-type? right-type))
                               `(,(create-syntax #f '= key-src) ,left ,right))
                              (else
                                `(,(create-syntax #f 'javaRuntime:check-eq? key-src) ,left ,right))) source))
              ((!=) 
               (make-syntax #f `(,(create-syntax #f 'javaRuntime:not-equal key-src) ,left ,right) source))
              ;logicals
              ((& ^ or) 
               (make-syntax #f 
                            `(,(create-syntax #f 'javaRuntime:bitwise key-src) (quote ,op) ,left ,right) source))
              ;boolean
              ((&&) (make-syntax #f `(,(create-syntax #f 'javaRuntime:and key-src) ,left ,right) source))
              ((oror) (make-syntax #f `(,(create-syntax #f 'javaRuntime:or key-src) ,left ,right) source))
              (else
               (error 'translate-op (format "Translate op given unknown operation ~s" op))))))
      (if (dynamic-val? type)
          (make-syntax #f
                       (convert-assert-value 
                        (make-syntax #f `(c:contract ,(type->contract (dynamic-val-type type) #t) ,result 
                                                     (quote ,(string->symbol (class-name))) '||) source)
                        type)
                       source)
          result)))

  ;translate-access: (U field-access local-access) type src -> syntax
  (define (translate-access name type src)
    (cond
      ((local-access? name)
       (let ((var (translate-id (build-var-name (id-string (local-access-name name)))
                                (id-src (local-access-name name)))))
         (if (dynamic-val? type)
             (let ((local-syntax (cond 
                                   ((unknown-ref? (dynamic-val-type type))
                                    `(let ((val-1 ,var))
                                       (if (string? val-1)
                                           (make-java-string val-1)
                                           val-1)))
                                   (else var))))
               (make-syntax #f
                            (convert-assert-value 
                             (make-syntax #f
                                          `(c:contract ,(type->contract (dynamic-val-type type) #t)
                                                       ,local-syntax (quote ,(string->symbol (class-name))) '||)
                                          (build-src (id-src (local-access-name name))))
                             (dynamic-val-type type)) (build-src (id-src (local-access-name name)))))
             var)))
      ((field-access? name)
       (let* ((field-string (id-string (field-access-field name)))
              (field-src (id-src (field-access-field name)))
              (access (field-access-access name))
              (obj (field-access-object name))
              (cant-be-null? (never-null? obj))
              (expr (if obj (translate-expression obj))))
         (cond
           ((var-access-static? access)
            (let ((static-name (build-static-name field-string (var-access-class access)))
                  (obj-wrapper 
                   (lambda (s) (if obj (make-syntax #f `(begin ,expr ,s) (build-src field-src)) s))))
              (if (dynamic-val? type)
                  (let ((access-syntax 
                         (cond 
                           ((unknown-ref? (dynamic-val-type type))
                            `(let ((val-1 ,(translate-id static-name field-src)))
                               (if (string? val-1)
                                   (make-java-string val-1)
                                   val-1)))
                           (else (translate-id static-name field-src)))))
                    (make-syntax #f
                                 (obj-wrapper
                                  (convert-assert-value
                                   (make-syntax #f
                                                `(c:contract ,(type->contract (dynamic-val-type type) #t)
                                                             ,access-syntax
                                                             (quote ,(string->symbol (class-name))) '||)
                                                (build-src field-src))
                                   (dynamic-val-type type))) (build-src field-src)))
                  (obj-wrapper (translate-id (build-var-name static-name) field-src)))))
           ((eq? 'array (var-access-class access))
            (if cant-be-null?
                (make-syntax #f `(send ,expr ,(translate-id field-string field-src)) (build-src src))
                (make-syntax #f
                             `(if (null? ,expr)
                                  ,(create-syntax #f '(javaRuntime:nullError 'field (current-continuation-marks))
                                                 expr)
                                  (send ,expr ,(translate-id field-string field-src)))
                             (build-src src))))
           ((and (eq? (var-access-access access) 'private) #;(or (static-method) (inner-class)))
            (let* ((id (create-get-name field-string (var-access-class access)))
                   (getter `(send ,expr ,id ,expr))
                   (get-syntax  (if cant-be-null?
                                    (make-syntax #f getter (build-src src))
                                    (make-syntax #f `(if (null? ,expr)
                                                         ,(create-syntax 
                                                           #f
                                                           '(javaRuntime:nullError 'field (current-continuation-marks))
                                                           expr)
                                                         ,getter)
                                                 (build-src src)))))
              (if (dynamic-val? type)
                  (let ((access-syntax (cond 
                                         ((unknown-ref? (dynamic-val-type type))
                                          `(let ((val-1 ,get-syntax))
                                             (if (string? val-1)
                                                 (make-java-string val-1)
                                                 val-1)))
                                         (else get-syntax))))
                    (make-syntax #f
                                 (convert-assert-value
                                  (make-syntax #f
                                               `(c:contract ,(type->contract (dynamic-val-type type) #t)
                                                            ,access-syntax (quote ,(string->symbol (class-name))) '||)
                                               (build-src field-src))
                                  (dynamic-val-type type)) (build-src field-src)))
                  get-syntax)))
           (else
              (let* ((id (create-get-name field-string (var-access-class access)))
                     (get-syntax
                      (if cant-be-null?
                          (make-syntax #f `(,id ,expr) (build-src src))
                          (make-syntax #f
                                       `(let ([val~1 ,expr])
                                          (if (null? val~1)
                                              ,(create-syntax #f '(javaRuntime:nullError 'field (current-continuation-marks))
                                                             expr)
                                              (,id val~1)))
                                       (build-src src)))))
                (if (dynamic-val? type)
                   (let ((access-syntax (cond 
                                         ((unknown-ref? (dynamic-val-type type))
                                          `(let ((val-1 ,get-syntax))
                                             (if (string? val-1)
                                                 (make-java-string val-1)
                                                 val-1)))
                                         (else get-syntax))))
                    (make-syntax #f
                                 (convert-assert-value
                                  (make-syntax #f
                                               `(c:contract ,(type->contract (dynamic-val-type type) #t)
                                                            ,access-syntax (quote ,(string->symbol (class-name))) '||)
                                               (build-src field-src))
                                  (dynamic-val-type type)) (build-src field-src)))
                   get-syntax))))))))
  
  ;translate-special-name: string src -> syntax
  (define (translate-special-name name src)
    (let ((id (build-identifier name)))
      (make-syntax #f id (build-src src))))
  
  ;translate-specified-this: string src -> syntax
  (define (translate-specified-this var src)
    (make-syntax #f (build-identifier (string-append var "~f")) (build-src src)))
  
  ;translate-call: (U expression #f) (U special-name id) (list syntax) (list type) method-record type src-> syntax
  (define (translate-call expr method-name args arg-types method-record rtype src)
    (let ((cant-be-null? (never-null? expr))
          (expression (if expr (translate-expression expr) #f))
          (unique-name (gensym))
          (translated-args 
           (if (method-contract? method-record)
               (map (lambda (arg type)
                      (guard-convert-value arg type))
                    args arg-types)
               (map (lambda (arg type call-type)
                      (if (eq? 'dynamic call-type)
                          (guard-convert-value arg type)
                          arg))
                    args arg-types (method-record-atypes method-record)))))
      (cond
        ;Constructor case
        ((special-name? method-name)
         (let* ((name (if (equal? (special-name-name method-name) "super")
                          (parent-name)
                          (class-name)))
                (c-name (build-identifier (build-constructor-name name
                                                                  (method-record-atypes method-record))))
                (generic-c-name (build-identifier (build-generic-name name c-name))))
           #;(create-syntax #f
                          (cond
                            ((equal? (special-name-name method-name) "this")
                             `(,c-name ,@args))
                            ((equal? (parent-name) "Object")
                             `(send ,(if expr expression 'this) ,c-name ,@args))
                            ((and expr cant-be-null?)
                             `(send-generic ,expression ,generic-c-name ,@args))
                            ((not expr)
                             `(send-generic this ,generic-c-name ,@args))
                            (else
                             `(let ((,unique-name ,expression))
                                (if (null? ,unique-name)
                                    ,(create-syntax #f '(javaRuntime:nullError 'method (current-continuation-marks))
                                                   expression)
                                    (send-generic ,unique-name ,generic-c-name ,@args)))))
                          (build-src src))
           
           (if cant-be-null?
               (create-syntax #f `(send ,(if expr expression 'this) ,c-name ,@translated-args) (build-src src))
               (create-syntax #f 
                              `(let ((,unique-name ,expression))
                                 (if (null? ,unique-name)
                                     ,(create-syntax #f `(javaRuntime:nullError 'method (current-continuation-marks))
                                                     expression)
                                     (send ,unique-name ,c-name ,@translated-args)))
                              (build-src src)))))
          
        ;Normal case
        ((id? method-name)
         (let* ((static? (and (not (method-contract? method-record))
                              (memq 'static (method-record-modifiers method-record))))
                (temp (unless (method-contract? method-record)
                        (if (memq 'private (method-record-modifiers method-record))
                            (mangle-private-method (method-record-name method-record)
                                                   (method-record-atypes method-record))
                            (mangle-method-name (method-record-name method-record)
                                                (method-record-atypes method-record)))))
                (m-name (cond
                          ((method-contract? method-record)
                           (if (method-contract-prefix method-record)
                               (build-static-name
                                (java-name->scheme (method-contract-name method-record))
                                (method-contract-prefix method-record))
                               (java-name->scheme (method-contract-name method-record))))
                          (static?
                           (build-static-name temp (car (method-record-class method-record))))
                          (else temp)))
                (generic-name (unless (method-contract? method-record)
                                (build-generic-name (car (method-record-class method-record)) m-name))))
           (cond 
             ((special-name? expr)
              (let* ((over? (and (overridden? (string->symbol m-name)) (equal? "super" (special-name-name expr))))
                     (name (translate-id m-name (id-src method-name)))
                     (new-exp (cond
                                (static? (create-syntax #f `(,name ,@translated-args) (build-src src)))
                                (over? (create-syntax #f `(super ,name ,@translated-args) (build-src src)))
                                (else (create-syntax #f `(send this ,name ,@translated-args) (build-src src))))))
                (if (or (method-contract? method-record)
                        (dynamic-val? rtype))
                    (make-syntax #f (convert-assert-value new-exp (if (method-contract? method-record)
                                                                      (method-contract-return method-record)
                                                                      (dynamic-val-type rtype))) (build-src src))
                    new-exp)))
             ((not expr)
              (cond
                ((method-contract? method-record)
                 (make-syntax #f (convert-assert-value
                                  (create-syntax #f `((c:contract ,(type->contract method-record #t)
                                                                  ,(build-identifier m-name #;(java-name->scheme (method-contract-name method-record)))
                                                                  (quote ,(string->symbol (class-name))) '||)
                                                      ,@translated-args) (build-src src))
                                  (method-contract-return method-record))
                              (build-src src)))
                ((or static? (memq 'private (method-record-modifiers method-record)))
                 (let ((call-syn
                        (create-syntax #f `(,(translate-id m-name (id-src method-name)) ,@translated-args) (build-src src))))
                   (if (dynamic-val? rtype)
                       (make-syntax #f (convert-assert-value call-syn (dynamic-val-type rtype)) (build-src src))
                       call-syn)))
                (else
                 (let ((call-syn 
                        (create-syntax #f `(send this ,(translate-id m-name (id-src method-name)) ,@translated-args) 
                                       (build-src src))))
                   (if (dynamic-val? rtype)
                       (make-syntax #f (convert-assert-value call-syn (dynamic-val-type rtype)) (build-src src))
                       call-syn)))))
             (else
              (let* ((name (translate-id m-name (id-src method-name)))
                     (call
                      (cond
                        ((and cant-be-null? (not static?))
                         (create-syntax #f `(send ,expression ,name ,@translated-args) (build-src src)))
                        (static? (create-syntax #f `(begin ,expression (,name ,@translated-args)) (build-src src)))
                        (else
                         (create-syntax #f
                                        `(let ((,unique-name ,expression))
                                           (if (null? ,unique-name)
                                               (javaRuntime:nullError 'method 
                                                                      ,(create-syntax #f
                                                                                      '(current-continuation-marks) expression))
                                               (send ,unique-name ,name ,@translated-args)))
                                        (build-src src))))))
                (if (or (method-contract? method-record)
                        (dynamic-val? rtype))
                    (make-syntax #f (convert-assert-value call 
                                                          (if (method-contract? method-record)
                                                              (method-contract-return method-record)
                                                              (dynamic-val-type rtype))) (build-src src))
                    call))))))
        (else (error 'translate-call (format "Translate call given ~s as method-name" method-name))))))

  ;Add more checks perhaps to see in other cases if it can be null
  ;never-null? expression -> bool
  (define (never-null? expr)
    (cond
      ((not expr) #t)
      ((special-name? expr) #t)
      ((class-alloc? expr) #t)
      ((and (access? expr)
            (local-access? (access-name expr))
            (regexp-match "encl-this-" (id-string (local-access-name (access-name expr))))) #t)
      (else #f)))
  
  (define (overridden? name)
    (hash-table-get (class-override-table) name (lambda () #f)))
  
  ;translate-class-alloc: (U name id def) (list type) (list syntax) src bool bool method-record-> syntax
  (define (translate-class-alloc class-type arg-types args src inner? local-inner? ctor-record)
    (when (id? class-type) (set! class-type (make-name class-type null (id-src class-type))))
    (let* ((class-string 
            (if (def? class-type)
                (get-class-string (make-name (def-name class-type) null #f))
                (get-class-string class-type)))
           (class-id (if (def? class-type)
                         (def-name class-type)
                         (name-id class-type)))
           (default-name (translate-id class-string (id-src class-id)))
           (default-ctor (translate-id (build-constructor-name 
                                        (cond
                                          (local-inner?
                                           (regexp-replace "-[0-9]*" (method-record-name ctor-record) ""))
                                          ((and (name? class-type) (null? (name-path class-type)))
                                           class-string)
                                          (else (id-string (name-id class-type))))
                                        (method-record-atypes ctor-record))
                                       (id-src class-id))))
      (make-syntax
       #f
       (cond
         (local-inner?
          `(let ((new-o (make-object ,default-name 
                          ,@(if (static-method)
                                null
                                (let loop ((d (current-depth)))
                                  (cond
                                    ((= d 0) '(this))
                                    (else (cons (build-identifier (format "encl-this-~a~~f" d))
                                                (loop (sub1 d)))))))
                          ,@(let loop ((args (def-closure-args
                                               (if (def? class-type)
                                                   class-type
                                                   (find-inner class-string (current-local-classes))))))
                              (cond
                                ((null? args) null)
                                (else (cons (translate-id (string-append (id-string (car args)) "~f") #f)
                                            (loop (cdr args)))))))))
             (send new-o ,default-ctor ,@args)
             new-o))
         (inner?
          `(send this ,(translate-id (mangle-method-name (string-append "construct-" class-string)
                                                         (method-record-atypes ctor-record))
                                     (id-src class-id))
                 ,@args))
         (else `(let ((new-o (make-object ,default-name)))
                  (send new-o ,default-ctor ,@args)
                  new-o)))
       (build-src src))))
  
  ;find-inner: string (list def) -> def
  (define (find-inner name classes)
    (cond
      ((equal? name (id-string (def-name (car classes))))
       (car classes))
      (else (find-inner name (cdr classes)))))
        
  
  ;translate-inner-alloc: syntax id (list syntax) src method-record -> syntax
  (define (translate-inner-alloc obj class args src ctor-record)
    (make-syntax #f `(send ,obj ,(translate-id (mangle-method-name (string-append "construct-"
                                                                                  (get-class-string (make-name class null #f)))
                                                                   (method-record-atypes ctor-record))
                                               (id-src class))
                           ,@args)
                 (build-src src)))
  
  ;translate-array-alloc: type-spec (list syntax) src -> syntax
  (define (translate-array-alloc type sizes src)
    (create-array sizes (translate-type-spec type) src))
  
  ;create-array: (list syntax) type src -> syntax
  (define (create-array sizes type src)
    (cond
      ((null? sizes)
       (error 'create-array "Internal Error: create array given a null list"))
      ((null? (cdr sizes))
       (make-syntax #f `(make-java-array ,type ,(car sizes) null) (build-src src)))
      (else
       (make-syntax #f `(make-java-array ,type (list ,@sizes) null) (build-src src)))))

  ;translate-array-alloc-init: type-spec int array-init src
  (define (translate-array-alloc-init type dim init src)
    (initialize-array (array-init-vals init) type))
    
  ;translate-type-spec: type-spec -> syntax
  (define (translate-type-spec type)
    (make-syntax #f
                 `(make-runtime-type ,(if (symbol? (type-spec-name type))
                                          `(quote ,(type-spec-name type))
                                          (build-identifier 
                                           (if (null? (name-path (type-spec-name type)))
                                               (id-string (name-id (type-spec-name type)))
                                               (append (map id-string (name-path (type-spec-name type)))
                                                       (list (id-string (name-id (type-spec-name type))))))))
                                     ,(type-spec-dim type))
                 (build-src (type-spec-src type))))
  
  ;converted
  ;translate-array-access: syntax syntax src -> syntax
  (define translate-array-access
    (lambda (array index src)
      (make-syntax #f `(send ,array access ,index) 
                   (build-src src))))
  
  ;converted
  ;translate-cond: syntax syntax syntax src -> syntax
  (define translate-cond
    (lambda (if? then else src)
      (make-syntax #f `(if ,if? ,then ,else) (build-src src))))
  
  ;converted
  ;translate-post-expr: syntax expression symbol src src -> syntax
  (define (translate-post-expr expr exp op key src)
    (let ([setter (cond
                    [(and (field-access? (access-name exp))
                          (not (var-access-static? (field-access-access (access-name exp)))))
                     (create-set-name (id-string (field-access-field (access-name exp)))
                                   (var-access-class (field-access-access (access-name exp))))]
                    [else 'set!])])
      (make-syntax #f `(begin0
                         ,expr
                         (,setter ,expr ( ,(create-syntax #f (if (eq? op '++) 'add1 'sub1) (build-src key))
                                         ,expr)))
                   (build-src src))))
  
  ;converted
  ;translate-pre-expr: symbol syntax src src -> syntax
  (define (translate-pre-expr op expr exp key src)
    (let ([setter (cond
                    [(and (field-access? (access-name exp))
                          (not (var-access-static? (field-access-access (access-name exp)))))
                     (create-set-name (id-string (field-access-field (access-name exp)))
                                   (var-access-class (field-access-access (access-name exp))))]
                    [else 'set!])])
      (make-syntax #f
                   `(begin
                      (,setter ,expr (,(create-syntax #f (if (eq? op '++) 'add1 'sub1) (build-src key))
                                      ,expr))
                      ,expr)
                   (build-src src))))
  
  ;converted
  ;translate-unary: symbol syntax src src -> syntax
  (define translate-unary
    (lambda (op expr key src)
      (make-syntax #f  (case op
                         ((-) `(,(create-syntax #f '- (build-src key)) ,expr))
                         ((!) `(,(create-syntax #f 'not (build-src key)) ,expr))
                         ((~) `(,(create-syntax #f '- (build-src key)) (- ,expr) 1))
                         ((+) expr))
                   (build-src src))))
  
  ;translate-cast: type-spec syntax type src
  (define (translate-cast type expr expr-type src)
    (cond
      ((eq? 'dynamic (type-spec-name type))
       (make-syntax #f (guard-convert-value expr expr-type) (build-src src)))
      ((dynamic-val? expr-type)
       (make-syntax #f (convert-assert-value 
                        (create-syntax #f `(c:contract ,(type->contract expr-type #t) ,expr 
                                                       (quote ,(string->symbol (class-name))) '||)
                                       (build-src src)) expr-type)
                    (build-src src)))
    ((symbol? (type-spec-name type))
     (make-syntax #f `(javaRuntime:cast-primitive ,expr (quote ,(type-spec-name type)) ,(type-spec-dim type))
                  (build-src src)))
    (else
     (let*  ((class (get-class-name type))
             (ca-class (string->symbol (format "convert-assert-~a" (syntax-object->datum class))))
             (gc-class (string->symbol (format "guard-convert-~a" (syntax-object->datum class)))))
       (if (or (eq? (syntax-object->datum class) 'String)
               (eq? (syntax-object->datum class) 'java.lang.String))
           (make-syntax #f `(javaRuntime:cast-reference ,expr ,class null null ,(type-spec-dim type)
                                                        (quote ,(get-class-name type)))
                        (build-src src))
           (make-syntax #f `(javaRuntime:cast-reference ,expr ,class ,ca-class ,gc-class
                                                        ,(type-spec-dim type)
                                                        (quote ,(get-class-name type)))
                        (build-src src)))))))
  
  ;translate-instanceof: syntax type-spec src -> syntax
  (define (translate-instanceof expr type src)
    (if (> (type-spec-dim type) 0)
        (make-syntax #f
                     (if (symbol? (type-spec-name type))
                         `(javaRuntime:instanceof-array #t ,expr (quote ,(type-spec-name type)) ,(type-spec-dim type))
                         `(javaRuntime:instanceof-array #f ,expr ,(get-class-name type) ,(type-spec-dim type)))
                     (build-src src))
        (let ((syntax-type (get-class-name type)))
          (if (or (eq? (syntax-e syntax-type) 'Object) (eq? (syntax-e syntax-type) 'java.lang.Object))
              (make-syntax #f `(is-a? ,expr ObjectI) (build-src src))
              (make-syntax #f `(is-a? ,expr ,syntax-type) (build-src src))))))
  
  ;translate-assignment: (U access array-access) symbol syntax expression type src src -> syntax
  (define (translate-assignment name op expr assign-to type key src)
    (let ((expression (lambda (name) 
                        (let ((expanded-expr
                               (case op
                                 ((=) expr)
                                 ((*=) `(* ,name ,expr))
                                 ((/=) `(/ ,name ,expr))
                                 ((+=) `(+ ,name ,expr))
                                 ((-=) `(- ,name ,expr))
                                 ((>>=) `(javaRuntime:shift '>> ,name ,expr))
                                 ((<<=) `(javaRuntime:shift '<< ,name ,expr))
                                 ((>>>=) `(javaRuntime:shift '>>> ,name ,expr))
                                 ((%= &= ^= or=)
                                  (error 'translate-assignment "Only supports =, +=, -=, *=, & /= >>= <<= >>>= at this time")))))
                          (cond
                            ((or (eq? type 'dynamic) (dynamic-val? type))
                             (guard-convert-value (make-syntax #f expanded-expr (build-src src)) (expr-types assign-to)))
                            ((and (memq type '(float double))
                                  (memq (expr-types assign-to) '(long int short byte)))
                             `(exact->inexact ,expanded-expr))
                            ((and (eq? (expr-types assign-to) 'char)
                                  (memq type '(byte short int long)))
                             `(char->integer ,expanded-expr))
                            ((and (eq? (expr-types assign-to) 'char)
                                  (memq type '(float double)))
                             `(exact->inexact (char->integer ,expanded-expr)))
                            (else expanded-expr))))))
      (cond 
        ((array-access? name)
         (translate-array-mutation name expression assign-to src))
        ((access? name)
         (let* ((access (access-name name))
                (src-h (build-src src))
                (set-h 
                 (lambda (id)
                   (make-syntax #f `(begin (,(create-syntax #f 'set! (build-src key))
                                            ,id ,(expression id)) ,id) src-h))))
           (cond
             ((local-access? access)
              (set-h (translate-id (build-var-name (id-string (local-access-name access)))
                                   (id-src (local-access-name access)))))
             ((field-access? access)
              (let* ((field (id-string (field-access-field access)))
                     (field-src (id-src (field-access-field access)))
                     (vaccess (field-access-access access))
                     (obj (field-access-object access))
                     (expr (if obj (translate-expression obj))))
                (cond
                  ((var-access-static? vaccess)
                   (set-h (build-identifier (build-static-name (build-var-name field)
                                                               (build-identifier (var-access-class vaccess))))))
                  ((not obj) (set-h (translate-id (build-var-name field) field-src)))
                  (else
                   (let ((setter (if (and (var-access-final? vaccess)
                                          (not (eq? 'private (var-access-access vaccess))))
                                     (make-syntax #f 
                                                  `(lambda (my-dummy new-val)
                                                     ((class-field-mutator ,(build-identifier (var-access-class vaccess))
                                                                           ,(build-identifier field))
                                                      this new-val))
                                                  #f)
                                     (create-set-name field (var-access-class vaccess))))
                         (getter (create-get-name field (var-access-class vaccess)))
                         (name (gensym 'field-obj))
                         (new-val (gensym 'val)))
                     (make-syntax #f
                                  `(let* ((,name ,expr)
                                          (,new-val ,(expression `(,getter ,name))))
                                     (,setter ,name ,new-val)
                                     ,new-val)
                                  src-h))))))))))))
  
  ;translate-array-mutation: array-access (syntax -> (list symbol syntax syntax)) expression src -> syntax
  (define (translate-array-mutation array expression expr src)
    (let ((array-name (translate-expression (array-access-name array)))
          (array-index (translate-expression (array-access-index array)))
          (name (gensym 'my-expr))
          (index (gensym 'my-index))
          (new-val (gensym 'val)))
      (make-syntax #f
                   `(let* ((,name ,array-name)
                           (,index ,array-index)
                           (,new-val ,(expression `(send ,name access ,index))))
                      (send ,name set ,index ,new-val)
                      ,new-val)
                   (build-src src))))

  ;translate-check: expr -> syntax
  (define (translate-check expr)
    (cond
      ((check-expect? expr) (translate-check-expect (check-expect-test expr)
                                                    (check-expect-actual expr)
                                                    (check-expect-range expr)
                                                    (expr-src expr)))
      ((check-rand? expr) (translate-check-rand (check-rand-test expr)
                                                (check-rand-range expr)
                                                (expr-src expr)))
      ((check-catch? expr) (translate-check-catch (check-catch-test expr)
                                                  (check-catch-exn expr)
                                                  (expr-src expr)))
      ((check-by? expr) (translate-check-by (check-by-test expr)
                                            (check-by-actual expr)
                                            (check-by-compare expr)
                                            (expr-src expr)))
      ((check-mutate? expr) (translate-check-mutate (check-mutate-mutate expr)
                                                    (check-mutate-check expr)
                                                    (expr-src expr)))))

  
  ;translate-check: expression expression (U expression #f) src -> syntax
  (define (translate-check-expect test actual range src)
    (let ((t (make-syntax #f `(lambda () ,(translate-expression test)) #f))
          (a (translate-expression actual))
          (r (when range (translate-expression range)))
          (extracted-info (checked-info test)))
      (make-syntax #f 
                   `(,(if (not range) 'javaRuntime:compare 'javaRuntime:compare-within)
                      ,@(if range (list t a r) (list t a))
                      ,extracted-info (quote ,(src->list src))
                      (namespace-variable-value 'current~test~object% #f 
                                                (lambda () #f))
                      ,(testcase-ext?))
                   (build-src src))))
  
  ;translate-check-rand: expression expression src -> syntax
  (define (translate-check-rand test range src)
    (let ([t (make-syntax #f `(lambda () ,(translate-expression test)) #f)]
          [r (translate-expression range)]
          [extracted-info (checked-info test)])
      (make-syntax #f
                   `(javaRuntime:compare-rand ,t ,r ,extracted-info (quote ,(src->list src))
                                              (namespace-variable-value 'current~test~object% #f
                                                                        (lambda () #f))
                                              )
                   (build-src src))))
  
  ;translate-check-catch: expression type-spec src -> syntax
  (define (translate-check-catch test catch src)
    (let ((t (create-syntax #f `(lambda () ,(translate-expression test)) #f))
          (n (get-class-name catch)))
      (make-syntax #f
                   `(javaRuntime:check-catch ,t ,(symbol->string (syntax-object->datum n)) ,n ,(checked-info test) 
                                             (quote ,(src->list src))
                                             (namespace-variable-value 'current~test~object% #f
                                                                       (lambda () #f)))
                   (build-src src))))
  
  ;translate-check-by: expression expression (U '== method-record) src -> syntax
  (define (translate-check-by test actual comp src)
    (let ([t (create-syntax #f `(lambda () ,(translate-expression test)) #f)]
          [a (translate-expression actual)]
          [info (checked-info test)])
      (make-syntax #f
                   `(javaRuntime:check-by ,t ,a 
                                            ,(if (method-record? comp)
                                                 (create-syntax #f `(lambda (test-v a)
                                                                      (send test-v 
                                                                            ,(build-identifier 
                                                                              (mangle-method-name 
                                                                               (method-record-name comp)
                                                                               (method-record-atypes comp)))
                                                                            a))
                                                                (build-src src))
                                                 'eq?)
                                            ,info
                                            ,(if (method-record? comp) (method-record-name comp) "==")
                                            (quote ,(src->list src))
                                            (namespace-variable-value 'current~test~object% #f (lambda () #f)))
                   (build-src src))))
  
  ;translate-check-mutate: expression expression src -> syntax
  (define (translate-check-mutate mutatee check src)
    (let ((t (create-syntax #f `(lambda () ,(translate-expression mutatee)) #f))
          (c (create-syntax #f `(lambda () ,(translate-expression check)) #f)))
      (make-syntax #f
                   `(javaRuntime:check-mutate ,t ,c ,(checked-info mutatee) (quote ,(src->list src))
                                              (namespace-variable-value 'current~test~object% #f
                                                                        (lambda () #f)))
                   (build-src src))))
  
  (require "error-messaging.ss")
  
  ;checked-info: expression -> (list sym string...)
  (define (checked-info exp)
    (cond
      ((access? exp)
       (cond
         ((field-access? (access-name exp))
          (let ((field (access-name exp)))
            `(list (quote
                    ,(if (var-access-static? (field-access-access field)) 'static-field 'field))
                   ,(var-access-class (field-access-access field))
                   ,(id-string (field-access-field field)))))
         (else 
          `(list (quote var)
                 ,(id-string (local-access-name (access-name exp)))))))
      ((class-alloc? exp)
       `(list (quote alloc)
              (quote ,(type->ext-name (expr-types exp)))
              (list ,@(map (lambda (t) `(quote ,t))
                           (map type->ext-name
                                (map expr-types
                                     (class-alloc-args exp)))))))
      ((call? exp)
       `(list (quote call)
              (quote ,(if (call-expr exp)
                          (type->ext-name (expr-types (call-expr exp)))
                          'no-exp))
              ,(id-string (call-method-name exp))
              (list ,@(map (lambda (t) `(quote ,t))
                           (map type->ext-name
                                (map expr-types
                                     (call-args exp)))))))
      ((instanceof? exp)
       `(list (quote instanceof) (quote ,(type-spec->ext-name (instanceof-type exp)))))
      ((array-access? exp)
       '(list (quote array)))
      ((unary? exp)
       '(list (quote unary) (quote (unary-op exp))))
      ((assignment? exp)
       `(list (quote assignment) 
              ,@(checked-info (assignment-left exp))))
      (else '(list (quote value)))))
  
  (define (type-spec->ext-name t)
    (format "~a~a"
            (cond
              ((name? (type-spec-name t))
               (id-string (name-id (type-spec-name t))))
              ((symbol? (type-spec-name t))
               (type-spec-name t)))
            (if (= 0 (type-spec-dim t))
                ""
                "[]")))
  
  (define (src->ext-name src)
    (format "~a:~a:~a" (src-file src) (src-line src) (src-col src)))
  
  ;translate-id: string src -> syntax
  (define translate-id
    (lambda (id src)
      (create-syntax #f (build-identifier id) (build-src src))))
  
  )
