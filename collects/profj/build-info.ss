(module build-info scheme/base
  
  (require scheme/class scheme/path 
           "ast.ss" "types.ss" "error-messaging.ss" "parameters.ss" 
           "restrictions.ss" "parser.ss" "profj-pref.ss")

  (provide build-info build-interactions-info build-inner-info find-implicit-import load-lang)
  
  ;-------------------------------------------------------------------------------
  ;General helper functions for building information
  
  (define class-name (make-parameter #f))
  
  ;; name->list: name -> (list string)
  (define (name->list n)
    (cons (id-string (name-id n)) (map id-string (name-path n))))
  
  ;same-base-dir?: path path -> bool
  (define (same-base-dir? full sub)
    (with-handlers ((exn? (lambda (e) #f)))
      (letrec ((full-ex (explode-path full))
               (sub-ex (explode-path sub))
               (first-of?
                (lambda (full sub)
                  (or (null? sub)
                      (and (equal? (car full) (car sub))
                           (first-of? (cdr full) (cdr sub)))))))
        (and (< (length sub-ex) (length full-ex))
             (first-of? full-ex sub-ex)))))
  
  ;build-require-syntax: string (list string) dir bool bool-> (list syntax)
  (define (build-require-syntax name path dir local? scheme?)
    (let* ((syn (lambda (acc) (datum->syntax #f acc #f)))
           (profj-lib? (ormap (lambda (p) (same-base-dir? dir p))
                              (map (lambda (p) (build-path p "profj" "libs"))
                                   (current-library-collection-paths))))
           (htdch-lib? (ormap (lambda (p) (same-base-dir? dir p))
                              (map (lambda (p) (build-path p "profj" "htdch"))
                                   (current-library-collection-paths))))
           (scheme-lib? (ormap (lambda (p) (same-base-dir? dir p))
                               (current-library-collection-paths)))
           (access (lambda (name)
                     (cond
                       (profj-lib? `(lib ,name "profj" "libs" ,@path))
                       (htdch-lib? 
                        `(lib ,name "profj" "htdch" ,@(if scheme? (cdddr path) path)))
                       (scheme-lib? `(lib ,name ,@(cddr path)))
                       ((and local? (not (to-file))) `(quote ,name))
                       (else `(file ,(path->string (build-path dir name)))))))
           (make-name (lambda ()
                        (let ((n (if scheme? (java-name->scheme name) name)))
                          (if (or (not local?) profj-lib? htdch-lib? scheme-lib? (to-file))
                              (string-append n ".ss")
                              (string->symbol n))))))
      (if scheme?
          (list (syn `(prefix-in ,(string->symbol
                                (apply string-append
                                       (append (map (lambda (s) (string-append s ".")) path)
                                               (list name "-"))))
                              ,(syn (access (make-name)))))
                (syn `(prefix-in ,(string->symbol (string-append name "-")) ,(syn (access (make-name))))))
          (list (syn `(prefix-in ,(string->symbol (apply string-append
                                                      (map (lambda (s) (string-append s ".")) path)))
                              ,(syn (access (make-name)))))
                (syn (access (make-name)))))))
  
  ;-------------------------------------------------------------------------------
  ;Main functions

  ;; build-info: package symbol type-records (opt symbol)-> void
  (define (build-info prog level type-recs . args)
    (let* ((pname (if (package-name prog)
                      (append (map id-string (name-path (package-name prog)))
                              (list (id-string (name-id (package-name prog)))))
                      null))
           (lang-pack `("java" "lang"))
           (test-pack `("java" "tester"))
           (lang (filter (lambda (class)
                           (not (forbidden-lang-class? class level)))                  
                         (send type-recs get-package-contents lang-pack 
                               (lambda () (error 'type-recs "Internal error: Type record not set with lang")))))
           (tester (when (testcase-ext?)
                     (send type-recs get-package-contents test-pack
                           (lambda () null))))
           (defs (let loop ((cur-defs (package-defs prog)))
                   (cond
                     ((null? cur-defs) null)
                     ((def? (car cur-defs)) (cons (car cur-defs) (loop (cdr cur-defs))))
                     (else 
                      (when (execution?) 
                        (send type-recs add-interactions-box (car cur-defs)))
                      (loop (cdr cur-defs))))))
           (current-loc (cond
                          ((not (null? defs)) (def-file (car defs)))
                          ((not (null? (package-imports prog)))
                           (import-file (car (package-imports prog)))))))
      (set-package-defs! prog defs)

      ;Add lang to local environment
      (for-each (lambda (class) (send type-recs add-to-env class lang-pack current-loc)) lang)
      (for-each (lambda (class) (send type-recs add-class-req (cons class lang-pack) #f current-loc)) lang)
      (send type-recs add-class-req (list 'array) #f current-loc)
      
      {when (testcase-ext?)
        (for-each (lambda (class) (send type-recs add-to-env class test-pack current-loc)) tester)
        (for-each (lambda (class) (send type-recs add-class-req (cons class test-pack) #f current-loc)) tester)}
      
      ;Set location for type error messages
      (build-info-location current-loc)      
      
      (let loop ((cur-defs defs))
        (unless (null? cur-defs)
          (when (member (id-string (def-name (car cur-defs)))
                        (map (lambda (d) (id-string (def-name d))) (cdr cur-defs)))
            (repeated-def-name-error (def-name (car cur-defs))
                                     (class-def? (car cur-defs))
                                     level
                                     (id-src (def-name (car cur-defs)))))
          (loop (cdr cur-defs))))
                        
      ;Add all defs in this file to environment
      (for-each (lambda (def) (add-def-info def pname type-recs current-loc (null? args) level)) defs)

      ;Set the package of the interactions window to that of the definitions window
      (when (execution?) 
        (send type-recs set-interactions-package pname)
        (send type-recs set-execution-loc! current-loc))
      
      ;All further definitions do not come from the execution window
      (execution? #f)
      
      ;Add package information to environment
      (when (memq level '(advanced full))
        (add-my-package type-recs pname (package-defs prog) current-loc level))
      
      ;Add import information
      (let ([cur-def-names (map def-name defs)])
        (for-each (lambda (imp) (process-import type-recs imp cur-def-names level)) 
                  (package-imports prog)))
      
      ;Build jinfo information for each def in this file
      (for-each (lambda (def) (process-class/iface def pname type-recs (null? args) #t level)) defs)

      ;Add these to the list for type checking
      (add-to-queue defs)))

  ;build-interactions-info: ast location type-records -> void
  (define (build-interactions-info prog level loc type-recs)
    (build-info-location loc)
    (send type-recs give-interaction-execution-names)
    (if (list? prog)
        (for-each (lambda (f) (build-interactions-info f level loc type-recs)) prog)
        (when (field? prog)
          (send type-recs add-interactions-field 
                (process-field prog '("scheme-interactions") type-recs level)))))
  
  ;add-def-info: def (list string) type-records loc bool symbol . (list syntax)-> void
  (define (add-def-info def pname type-recs current-loc look-in-table level . inner-req)
    (let* ((name (id-string (def-name def)))
           (defname (cons name pname))
           (native-name (cons (string-append name "-native-methods") pname))
           (dir (dir-path-path (find-directory pname (lambda () 
                                                       (make-dir-path (build-path 'same) #f))))))
      (unless (memq 'private (map modifier-kind (header-modifiers (def-header def))))
        (send type-recs add-to-env name pname current-loc)
        (when (execution?) (send type-recs add-to-env name pname 'interactions)))
      (let ((req-syn (if (null? inner-req) (build-require-syntax name pname dir #t #f) (car inner-req))))
        (send type-recs add-class-req defname #f current-loc)
        (send type-recs add-require-syntax defname req-syn)
        (send type-recs add-class-req native-name #f current-loc)
        (send type-recs add-require-syntax native-name 
              (build-require-syntax (car native-name) pname dir #f #f))
        (send type-recs add-to-records defname
              (lambda () (process-class/iface def pname type-recs look-in-table #t level)))
        ;;get info for Inner member classes
        (let ([prefix (format "~a." name)])
          (for-each (lambda (member)
                      (when (class-def? member)
                        ;; Adjust id to attach the prefix:
                        (let ([id (def-name member)])
                          (set-id-string! id (string-append prefix (id-string id))))
                        (add-def-info member pname type-recs current-loc #f (def-level def) req-syn)))
                    (def-members def))))))
  
  ;build-inner-info: def (U void string) (list string) symbol type-records loc bool -> class-record
  (define (build-inner-info def unique-name pname level type-recs current-loc look-in-table?)
    ;(add-def-info def pname type-recs current-loc look-in-table? level)
    (class-name unique-name)
    (let ((record (process-class/iface def pname type-recs #f #f level)))
      (when (string? unique-name) (set-class-record-name! record (cons unique-name pname)))
      (send type-recs add-to-records 
            (cons (if (eq? (def-kind def) 'statement) unique-name (id-string (def-name def))) pname)
            record)
      ;(printf "~a~n" unique-name)
      (send type-recs add-to-env unique-name pname current-loc)
      (class-name #f)
      record))
  
  ;add-to-queue: (list definition) -> void
  (define (add-to-queue defs)
    (check-list (append defs (check-list))))  
  
  ;-----------------------------------------------------------------------------------
  ;Import processing/General loading

  ;;process-import: type-records import (listof id) symbol -> void
  (define (process-import type-recs imp def-names level)
    (let* ((star? (import-star imp))
           (file (import-file imp))
           (name (id-string (name-id (import-name imp))))
           (string-path (map id-string (name-path (import-name imp))))
           (path (if star? (append string-path (list name)) string-path))
           (err (lambda () (import-error (import-name imp) (import-src imp)))))
      (cond
        [star?
         (let ([classes (send type-recs get-package-contents path (lambda () #f))]
               [check-dup-import
                (lambda (importer)
                  (lambda (class)
                    (let ([dup? (memf (lambda (e) (equal? class (id-string e))) def-names)])
                      (if dup?
                          (unless (eq? level 'full)
                            (import-dup-error 'star (id-string (car dup?)) (reverse path) (id-src (car dup?))))
                          (importer class)))))])
           (cond
             [classes
              (for-each (check-dup-import 
                         (lambda (class) (send type-recs add-to-env class path file))) classes)]
             [else
              (let* ([dir (find-directory path err)]
                     [class-list (get-class-list dir)]
                     [package-contents null])
                (printf "~a~n~a~n" dir class-list)
                (for-each (check-dup-import
                           (lambda (class)
                             (import-class class path dir file type-recs level (import-src imp) #t)
                             (set! package-contents (cons class package-contents))))
                          class-list)
                (send type-recs add-package-contents path package-contents))]))]
        [else
         (when (member name (map id-string def-names))
           (import-dup-error 'no-star name (reverse path) (import-src imp)))
         (import-class name path (find-directory path err) file type-recs level (import-src imp) #t)])))
  
  ;import-class: string (list string) dir-path location type-records symbol src bool-> void
  (define (import-class class path in-dir loc type-recs level caller-src add-to-env)
    (let* ((dir (dir-path-path in-dir))
           (class-name (cons class path))
           (type-path (build-path dir "compiled" (string-append class ".jinfo")))
           (new-level (box level))
           (class-exists? (check-file-exists? class dir new-level))
           (suffix (case (unbox new-level)
                     ((beginner) ".bjava")
                     ((intermediate) ".ijava")
                     ((intermediate+access) ".iajava")
                     ((advanced) ".ajava")
                     ((full) ".java")
                     ((dynamic-full) ".djava")))
           (file-path (build-path dir (string-append class suffix))))
      (cond
        ((is-import-restricted? class path level) (used-restricted-import class path caller-src))
        ((send type-recs get-class-record class-name #f (lambda () #f)) 
         void )
        ((and (file-exists? type-path)
              (or (core? class-name) (older-than? file-path type-path)) (read-record type-path))
         =>
         (lambda (record)
           (send type-recs add-class-record record)
           (send type-recs add-require-syntax class-name (build-require-syntax class path dir #f #f))
           (map (lambda (ancestor)
                  (let* ([ancestor-name (car ancestor)]
                         [ancestor-path (if (null? (cdr ancestor))
                                            (send type-recs lookup-path ancestor-name 
                                                  (lambda () null))
                                            (cdr ancestor))])
                    (import-class ancestor-name ancestor-path
                                  (find-directory ancestor-path
                                                  (lambda () (error 'internal-error "Compiled parent's directory is not found")))
                                  loc type-recs level caller-src add-to-env)))
                (append (class-record-parents record) (class-record-ifaces record)))
           ))
        ((and (dynamic?) (dir-path-scheme? in-dir) (check-scheme-file-exists? class dir))
         (send type-recs add-to-records class-name (make-scheme-record class (cdr path) dir null))
         (send type-recs add-require-syntax class-name (build-require-syntax class path dir #f #t)))
        (class-exists?
         (send type-recs add-to-records 
               class-name
               (lambda () 
                 (let ((location (string-append class suffix))
                       #;(old-type-cloc (send type-recs get-compilation-location))
                       #;(old-type-loc (send type-recs get-location))
                       (old-dynamic? (dynamic?)))
                   (when (eq? 'dynamic-full (unbox new-level))
                     (dynamic? #t) (set-box! new-level 'full))
                   (let ((ast (call-with-input-file file-path 
                                (lambda (p) (parse p location (unbox new-level))))))
                     (send type-recs set-compilation-location location (build-path dir "compiled"))
                     (build-info ast (unbox new-level) type-recs 'not_look_up)
                     (begin0 (send type-recs get-class-record class-name #f 
                                   (lambda () 'internal-error "Failed to add record"))
                             (dynamic? old-dynamic?)
                             #;(send type-recs set-compilation-location old-type-cloc)
                             #;(send type-recs set-location! old-type-loc))
                     ))))
         (send type-recs add-require-syntax class-name (build-require-syntax class path dir #t #f)))
        (else 
         (file-error 'file (cons class path) caller-src level)))
      (when add-to-env (send type-recs add-to-env class path loc))
      (send type-recs add-class-req class-name (not add-to-env) loc)))

  ;determines if file a is older than file b
  ;older-than?: path path -> bool
  (define (older-than? file-a file-b)
    (and (file-exists? file-a)
         (file-exists? file-b)
         (<= (file-or-directory-modify-seconds file-a)
            (file-or-directory-modify-seconds file-b))))
  
  ;core: (list string) -> bool
  ;Determines if the given class is a core class not written in Java
  (define (core? class)
    (member class `(("Object" "java" "lang")
                   ("String" "java" "lang")
                   ("Throwable" "java" "lang")
                   ("Comparable" "java" "lang")
                   ("Serializable" "java" "io"))))
  
  ;check-file-exists?: string path box -> bool
  ;side-effect: modifies contents of box
  (define (check-file-exists? class path level)
    (let ((exists? 
           (lambda (suffix lang)
             (and (file-exists? (build-path path (string-append class suffix)))
                  (set-box! level lang)))))
      (or (exists? ".java" 'full)
          (exists? ".djava" 'dynamic-full)
          (exists? ".bjava" 'beginner)
          (exists? ".ijava" 'intermediate)
          (exists? ".iajava" 'intermediate+access)
          (exists? ".ajava" 'advanced))))
    
  ;check-scheme-file-exists? string path -> bool
  (define (check-scheme-file-exists? name path)
    (or (file-exists? (build-path path (string-append (java-name->scheme name) ".ss")))
        (file-exists? (build-path path (string-append (java-name->scheme name) ".scm")))))
  
  (define (create-scheme-type-rec mod-name req-path) 'scheme-types)
    
  ;add-my-package: type-records (list string) (list defs) loc symbol-> void
  (define (add-my-package type-recs package defs loc level)
    (let* ((dir (find-directory package 
                                (lambda () 
                                  (let-values (((base cur dir?) (split-path (current-directory))))
                                    (and (equal? (apply build-path package) cur)
                                         (make-dir-path (build-path 'same) #f))))))
           (classes (if dir (get-class-list dir) null)))
      ;(printf "~n~nadd-my-package package ~a loc ~a ~n" package loc)
      ;(printf "add-my-package: dir ~a class ~a~n" (dir-path-path dir) classes)
      (let ([external-classes (filter (lambda (c) (not (contained-in? defs c))) classes)])
        (for-each (lambda (c) (send type-recs add-to-env c package loc)) external-classes)
        (for-each (lambda (c) 
                    (import-class c package dir loc type-recs level #f #t)) external-classes)
        (send type-recs add-package-contents package classes))))
      
  ;contained-in? (list definition) definition -> bool
  (define (contained-in? defs class)
    (and (not (null? defs))
         (or (equal? class (id-string (def-name (car defs))))
             (contained-in? (cdr defs) class))))
  
  ;find-implicit-import: (list string) type-records symbol src-> ( -> record )
  (define (find-implicit-import name type-recs level call-src)
    (lambda ()
      (let ((original-loc (send type-recs get-location))
            (dir (find-directory (cdr name) (lambda () (file-error 'dir (cdr name) call-src level)))))
        (when (memq level '(beginner intermediate intermediate+access))
          (file-error 'file name call-src level))
        (import-class (car name) (cdr name) dir original-loc type-recs level call-src #f)
        (begin0 (get-record (send type-recs get-class-record name) type-recs)
                (send type-recs set-location! original-loc)))))
  
  ;(make-directory path bool)
  (define-struct dir-path (path scheme?) #:transparent)
  
  ;find-directory: (list string) ( -> void) -> dir-path
  (define (find-directory path fail)
    (cond
      ((null? path) (make-dir-path (build-path 'same) #f))
      ((and (dynamic?) (equal? (car path) "scheme"))
       (cond
         ((null? (cdr path)) (make-dir-path (build-path 'same) #t))
         ((not (equal? (cadr path) "lib")) 
          (let ((dir (find-directory (cdr path) fail)))
            (make-dir-path (dir-path-path dir) #t)))
         ((and (equal? (cadr path) "lib") (not (null? (cddr path))))
          (make-dir-path (apply collection-path (cddr path)) #t))
         (else (make-dir-path (build-path "mzlib") #t))))
      (else
       (when (null? (classpath)) (classpath (get-classpath)))
       (let-values (((search)
                     (lambda ()
                       (let loop ((paths (classpath)))
                         (cond
                           ((null? paths) (fail))
                            ((and (directory-exists? (build-path (car paths) 
                                                                 (apply build-path path))))
                             (make-dir-path (build-path (car paths) (apply build-path path)) #f))
                            (else (loop (cdr paths)))))))
                    ((cur-path-base cur-path dir?) 
                     (split-path (simplify-path (path->complete-path (build-path 'same))))))
         (cond
           ((equal? (car path) (path->string cur-path))
            (if (null? (cdr path))
                (make-dir-path (build-path 'same) #f)
                (if (directory-exists? (build-path (build-path 'same) (apply build-path (cdr path))))
                    (make-dir-path (build-path (build-path 'same) (apply build-path (cdr path))) #f)
                    (search))))
           (else (search)))))))

  ;get-class-list: dir-path -> (list string)
  (define (get-class-list dir)
    (if (and (dynamic?) (dir-path-scheme? dir))
        (filter (lambda (f) (or (equal? (filename-extension f) #".ss")
                                (equal? (filename-extension f) #".scm")))
                (directory-list (dir-path-path dir)))
        (filter (lambda (c-name) (not (equal? c-name "")))
                (map (lambda (fn)
                       (let ((str (path->string fn)))
                         (substring str 0 (- (string-length str) 
                                             (add1 (bytes-length (filename-extension fn)))))))
                     (filter (lambda (f)
                               (let ((first (substring (path->string f) 0 1))
                                     (ext (filename-extension f)))
                                 (and (not (equal? "#" first))
                                      (not (equal? "." first))
                                      (or (equal? ext #"java")
                                          (equal? ext #"djava")
                                          (equal? ext #"ajava")))))
                             (directory-list (dir-path-path dir)))))))
  
  ;load-lang: type-records -> void (adds lang to type-recs)
  (define (load-lang type-recs)
    (let* ((lang `("java" "lang"))
           (test '("java" "tester"))
           (lang-dir (find-directory lang (lambda () (error 'load-lang "Internal-error: Lang not accessible"))))
           (test-dir (when (testcase-ext?)
                       (find-directory test (lambda () (error 'load-lang "Internal-error: Test not accessible")))))
           (get-classes 
            (lambda (base-dir)
              (map (lambda (fn) (substring fn 0 (- (string-length fn) 6)))
                   (map path->string
                        (filter (lambda (f) (equal? (filename-extension f) #"jinfo"))
                                (directory-list (build-path (dir-path-path base-dir) "compiled")))))))
           (lang-classes (get-classes lang-dir)) 
           (test-classes (when (testcase-ext?) (get-classes test-dir)))
           (array (datum->syntax #f `(lib "array.ss" "profj/libs/java/lang") #f))
           
           (add
            (lambda (path classes dir array?)
              #;(printf "class-list ~a~n" classes)
              (send type-recs add-package-contents path classes)
              (for-each (lambda (c) (import-class c path dir #f type-recs 'full #f #f)) classes)
              (when array? (send type-recs add-require-syntax (list 'array) (list array array)))
              
              ;Add lang to interactions environment
              (for-each (lambda (class) (send type-recs add-to-env class path 'interactions)) classes)
              (send type-recs set-location! 'interactions)
              (for-each (lambda (class) (send type-recs add-class-req (cons class path) #f 'interactions))
                        classes)
              (when array? (send type-recs add-class-req (list 'array) #f 'interactions)))))
      (add lang lang-classes lang-dir #t)
      (when (testcase-ext?) (add test test-classes test-dir #f))))
        
  ;------------------------------------------------------------------------------------
  ;Functions for processing classes and interfaces
  
  ;; process-class/iface: (U class-def interface-def test-def) (list string) type-records bool bool symbol -> class-record
  (define (process-class/iface ci package-name type-recs look-in-table put-in-table level)
    (cond
      [(interface-def? ci)
       (process-interface ci package-name type-recs look-in-table put-in-table level)]
      [(class-def? ci) 
       (process-class ci package-name type-recs look-in-table put-in-table level)]
      [(test-def? ci)
       (process-test ci package-name type-recs look-in-table put-in-table level)]))
  
  ;;get-parent-record: (list string) name (list string) type-records (list string) -> record
  (define (get-parent-record name n child-name level type-recs)
    (when (equal? name child-name)
      (dependence-error 'immediate (name-id n) (name-src n)))
    (let ((record (send type-recs get-class-record name)))
      #;(printf "~a, ~a~n" name record)
      (cond
        ((class-record? record) record)
        ((procedure? record) 
         (let ((cur-src-loc (build-info-location)))
           (begin0 (get-record record type-recs)
                   (build-info-location cur-src-loc))))
        ((eq? record 'in-progress)
         (dependence-error 'cycle (name-id n) (name-src n)))
        (else 
         (let ((cur-src-loc (build-info-location)))
           (begin0
             (get-record (find-implicit-import name type-recs level (name-src n)) type-recs)
             (build-info-location cur-src-loc)))))))
  
  (define (class-specific-field? field)
    (not (memq 'private 
               (field-record-modifiers field))))
  
  (define (class-specific-method? method new-methods)
    (not (or (memq 'static (method-record-modifiers method))
             (memq 'private (method-record-modifiers method))
             (eq? 'ctor (method-record-rtype method))
             (over-riden? method new-methods))))
    
  (define (over-riden? m listm)
    (and (not (null? listm))
         (let ((m2 (car listm)))
           (or (and (equal? (method-record-name m)
                            (method-record-name m2))
                    (type=? (method-record-rtype m)
                            (method-record-rtype m2))
                    (and (= (length (method-record-atypes m))
                            (length (method-record-atypes m2)))
                         (andmap type=? (method-record-atypes m)
                                 (method-record-atypes m2)))
                    (and (= (length (method-record-modifiers m))
                            (length (method-record-modifiers m2)))
                         (andmap eq? (method-record-modifiers m)
                                     (method-record-modifiers m2))))
               (over-riden? m (cdr listm))))))
  
  ;; process-class: class-def (list string) type-records bool bool symbol -> class-record
  (define (process-class class package-name type-recs look-in-table? put-in-table? level)
    (let* ((info (def-header class))
           (cname (cons (id-string (header-id info)) package-name)))
      (send type-recs set-location! (def-file class))
      (let ((build-record
             (lambda ()
               (when put-in-table? 
                 (send type-recs add-to-records cname 'in-progress))
               (let* ((super (if (null? (header-extends info)) null (car (header-extends info))))
                      (super-name (if (null? super)
                                      '("Object" "java" "lang")
                                      (if (null? (name-path super))
                                          (cons (id-string (name-id super))
                                                (send type-recs lookup-path (id-string (name-id super)) (lambda () null)))
                                          (name->list super))))
                      (super-record (get-parent-record super-name super cname level type-recs))
                      (iface-records (map (lambda (i)
                                            (let ([name-list 
                                                   (if (null? (name-path i))
                                                       (cons (id-string (name-id i))
                                                             (send type-recs lookup-path (id-string (name-id i)) (lambda () null)))
                                                       (name->list i))])
                                              (get-parent-record name-list i #f level type-recs)))
                                          (header-implements info)))
                      (members (def-members class))
                      (modifiers (header-modifiers info))
                      (test-mods (map modifier-kind modifiers))
                      (reqs (map (lambda (name-list)
                                   (if (= (length name-list) 1)
                                       (make-req (car name-list) 
                                                 (send type-recs lookup-path (car name-list) (lambda () null)))
                                       (make-req (car name-list) (cdr name-list))))
                                 (cons super-name (map name->list (header-implements info)))))
                      (old-loc (send type-recs get-location)))
                 
                 (set! reqs
                       (remove-dup-reqs
                        (append (get-method-reqs (class-record-methods super-record))
                                reqs)))
                 (send type-recs set-location! (def-file class))
                 (set-def-uses! class reqs)
                 
                 (when (eq? level 'full)
                   (when (memq 'final (class-record-modifiers super-record))
                     (extension-error 'final (header-id info) super (id-src super))))
                   
                 (unless (class-record-class? super-record)
                   (extension-error 'class-iface (header-id info) super (name-src super)))
                 
                 #;(printf "~a~n" (header-implements info))
                 #;(printf "~a~n" iface-records)
                 
                 (when (ormap class-record-class? iface-records)
                   (letrec ((find-class
                             (lambda (recs names)
                               (if (class-record-class? (car recs))
                                   (car names)
                                   (find-class (cdr recs) (cdr names)))))
                            (name (find-class iface-records (header-implements info))))
                     (extension-error 'implement-class (header-id info) name (name-src name))))
                 
                 (valid-iface-implement? iface-records (header-implements info))
                                  
                 (let*-values (((old-methods) (class-record-methods super-record))
                               ((f m i)
                                (if (memq 'strictfp test-mods)
                                    (process-members members old-methods cname type-recs level #f #f
                                                     (find-strictfp modifiers))
                                    (process-members members old-methods cname type-recs level #f #f)))
                               ((ctor?) (has-ctor? m)))
                                      
                   (unless ctor?
                     (when (and (eq? level 'beginner) #;(not (memq 'abstract test-mods)))
                       (beginner-ctor-error 'none (header-id info) (id-src (header-id info))))
                     (add-ctor class (lambda (rec) (set! m (cons rec m))) old-methods (header-id info) level))
                   
                   #;(when (and ctor? (eq? level 'beginner) (memq 'abstract test-mods))
                     (beginner-ctor-error 'abstract (header-id info) (id-src (header-id info))))

                   (valid-field-names? (if (memq level '(beginner intermediate intermediate+access advanced)) 
                                           (append f (class-record-fields super-record)) f)
                                       members m level type-recs)
                   (valid-method-sigs? m members level type-recs)

                   (when (not (memq 'abstract test-mods))
                     (and (class-fully-implemented? super-record super 
                                                    iface-records (header-implements info)
                                                    m type-recs level)
                          (no-abstract-methods m members level type-recs)))
                                      
                   (valid-inherited-methods? (cons super-record iface-records)
                                             (cons (if (null? super)
                                                       (make-name (make-id "Object" #f) null #f)
                                                       super)
                                                   (header-implements info))
                                             level
                                             type-recs)
                   (check-current-methods (cons super-record iface-records)
                                          m
                                          members
                                          level
                                          type-recs)
                   
                   (when (and (memq 'abstract test-mods)
                              (or (not (null? iface-records))
                                  (not (null? (header-implements info)))))
                     (let ((unimp-stubs (make-unimplmented-stubs iface-records (header-implements info)
                                                                 m type-recs)))
                       (set-def-members! class
                                         (append unimp-stubs (def-members class)))
                       (set! m (append m (map method-rec unimp-stubs)))))
                   
                   (let ((record
                          (make-class-record 
                           cname
                           (check-class-modifiers level (def-kind class) modifiers)
                           #t
                           #t
                           (append f (filter class-specific-field? (class-record-fields super-record)))
                           (append m (filter (lambda (meth)
                                               (class-specific-method? meth m))
                                             (class-record-methods super-record)))
                           (append i (filter (lambda (i-r)
                                               (not (memq 'private (inner-record-modifiers i-r))))
                                             (class-record-inners super-record)))
                           (cons super-name (class-record-parents super-record))
                           (map (lambda (iface)
                                  (if (null? (cdr iface))
                                      (cons (car iface)
                                            (send type-recs lookup-path (car iface) (lambda () null)))
                                      iface))
                                (filter (lambda (iface) (not (null? iface)))
                                        (append (map name->list (header-implements info))
                                                (apply append (map class-record-parents iface-records))
                                                (class-record-ifaces super-record)))))))
                     (when put-in-table? (send type-recs add-class-record record))
                     
                     (for-each (lambda (member)
                                 (when (def? member)
                                   (process-class/iface member package-name type-recs #f put-in-table? level)))
                               members)
                     (send type-recs set-location! old-loc)
                     
                     record))))))
        (cond
          ((class-record? (send type-recs get-class-record cname)) => 
           (lambda (rec) rec))
          (look-in-table?
           (get-record (send type-recs get-class-record cname #f build-record) type-recs))
          (else (build-record))))))
  
  ;find-strictfp (list modifier) -> modifier
  (define (find-strictfp mods)
    (if (null? mods)
        null
        (if (eq? 'strictfp (modifier-kind (car mods)))
            (car mods)
            (find-strictfp (cdr mods)))))
  
  ;has-ctor?: (list method-record)-> bool
  (define (has-ctor? methods)
    (and (not (null? methods))
         (or (eq? (method-record-rtype (car methods)) 'ctor)
             (has-ctor? (cdr methods)))))
  
  ;add-ctor: class-def (method-record -> void) (list method-record) id symbol-> void
  (define (add-ctor class add-rec super-methods name level) 
    (let ((super-ctor (find-default-ctor super-methods)))
      (cond
        ((not super-ctor)
         (default-ctor-error 'non-accessible name (car (method-record-class (car super-methods))) (id-src name) level))
        ((memq 'private (method-record-modifiers super-ctor))
         (default-ctor-error 'private name (method-record-class super-ctor) (id-src name) level))
        ((and (memq level '(advanced full))
              (not (null? (method-record-throws super-ctor))))
         (default-ctor-error 'throws name (method-record-class super-ctor) (id-src name) level))
        (else
         (let* ((rec (make-method-record (id-string name) `(public) 'ctor null null #f 
                                         (if (class-name) (list (class-name)) (list (id-string name)))))
                (method (make-method (list (make-modifier 'public #f))
                                     (make-type-spec 'ctor 0 #f)
                                     null
                                     (make-id (id-string name) #f)
                                     null
                                     null
                                     (make-block 
                                     (list (make-call #f #f #f (make-special-name #f #f "super") null #f)) #f)
                                    #t
                                    rec
                                    #f)))
           (set-def-members! class (cons method (def-members class)))
           (add-rec rec))))))

  ;find-default-ctor: (list method-record) -> (U boolean method-record)
  (define (find-default-ctor methods)
    (and (not (null? methods))
         (or (and (eq? (method-record-rtype (car methods)) 'ctor)
                  (null? (method-record-atypes (car methods)))
                  (car methods))
             (find-default-ctor (cdr methods)))))
  
  ;remove-dup-reqs (list req) -> (list req)
  (define (remove-dup-reqs reqs)
    (cond
      ((null? reqs) null)
      ((member (car reqs) (cdr reqs))
       (remove-dup-reqs (cdr reqs)))
      (else
       (cons (car reqs) (remove-dup-reqs (cdr reqs))))))
  
  ;get-method-reqs: (list method-record) -> (list req)
  (define (get-method-reqs methods)
    (cond
      ((null? methods) methods)
      ((or (memq (method-record-rtype (car methods)) '(void ctor short byte int long float double
                                                            boolean dynamic char))
           (array-type? (method-record-rtype (car methods))))
       (append (get-reqs-parms (method-record-atypes (car methods)))
               (get-method-reqs (cdr methods))))
      ((null? (method-record-atypes (car methods)))
       (cons (type->req (method-record-rtype (car methods)))
             (get-method-reqs (cdr methods))))
      (else
       (cons (type->req (method-record-rtype (car methods)))
             (append (get-reqs-parms (method-record-atypes (car methods)))
                     (get-method-reqs (cdr methods)))))))
  
  (define (get-reqs-parms parms)
    (cond
      ((null? parms) null)
      ((memq (car parms) '(short byte int char long float double boolean dynamic))
       (get-reqs-parms (cdr parms)))
      ((array-type? (car parms)) (get-reqs-parms (cdr parms)))
      (else (cons (type->req (car parms)) (get-reqs-parms (cdr parms))))))
  (define (type->req t)
    (cond
      ((or (eq? 'string t) (equal? string-type t))
       (make-req "String" '("java" "lang")))
      ((ref-type? t)
       (make-req (ref-type-class/iface t) (ref-type-path t)))
      #;(else (make-req 'array '()))))
  
  ;; process-interface: interface-def (list string) type-records bool bool symbol -> class-record
  (define (process-interface iface package-name type-recs look-in-table? put-in-table? level)
    (let* ((info (def-header iface))
           (iname (cons (id-string (header-id info)) package-name)))
      (send type-recs set-location! (def-file iface))
      (let ((build-record 
             (lambda ()
               (send type-recs add-to-records iname 'in-progress)
               (let* ((super-names (map name->list (header-extends info)))
                      (super-records 
                       (map (lambda (n sc) (get-parent-record n sc iname level type-recs))
                            super-names
                            (header-extends info)))
                      (object-methods 
                       (filter (lambda (m) 
                                 (not (eq? 'ctor (method-record-rtype m))))
                               (class-record-methods (send type-recs get-class-record object-type))))
                      (members (def-members iface))
                      (reqs (map (lambda (name-list)
                                   (if (= (length name-list) 1)
                                       (make-req (car name-list)
                                                 (send type-recs lookup-path (car name-list) (lambda () null)))
                                       (make-req (car name-list) (cdr name-list))))
                                 super-names))
                      (old-loc (send type-recs get-location)))
                 (send type-recs set-location! (def-file iface))
                 (set-def-uses! iface reqs)                 
                 
                 (when (ormap class-record-class? super-records)
                   (letrec ((find-class
                             (lambda (recs names)
                               (if (class-record-class? (car recs))
                                   (car names)
                                   (find-class (cdr recs) (cdr names)))))
                            (name (find-class super-records (header-extends info))))
                     (extension-error 'iface-class (header-id info) name (name-src name))))
                 
                 (valid-iface-extend? super-records (header-extends info))
                 
                 (let-values (((f m i) (process-members members (apply append 
                                                                       (map class-record-methods super-records))
                                                        iname type-recs level #t #f)))
                   
                   (valid-field-names? f members m level type-recs)
                   (valid-method-sigs? m members level type-recs)
                   (valid-inherited-methods? super-records (header-extends info) level type-recs)
                   (check-current-methods super-records m members level type-recs)
                   
                   (for-each (lambda (fi)
                               (unless (memq 'static (field-record-modifiers fi))
                                 (set-field-record-modifiers! fi 
                                                              (cons 'static (field-record-modifiers fi)))))
                             f)
                   
                   (let ((record
                          (make-class-record 
                           iname
                           (check-interface-modifiers level (header-modifiers info))
                           #f
                           #t
                           (apply append (cons f (map class-record-fields super-records)))
                           (apply append (cons m (map class-record-methods super-records)))
                           (apply append (cons i (map class-record-inners super-records)))
                           (apply append (cons (map class-record-name super-records) 
                                               (map class-record-parents super-records)))
                           null)))
                     (send type-recs add-class-record record)
                     (send type-recs set-location! old-loc)
                     record))))))
        (if look-in-table?
            (get-record (send type-recs get-class-record iname #f build-record) type-recs)
            (build-record)))))
  
  ;process-test: def-test (list string) type-records boolean? boolean? symbol -> class-record
  (define (process-test test package-name type-recs look-in-table? put-in-table? level)
    (let* ((info (def-header test))
           (test-base '("TestBase" "java" "tester"))
           (tname (cons (id-string (header-id info)) package-name)))
      (send type-recs set-location! (def-file test))
      (let ((build-record
             (lambda ()
               (when put-in-table? (send type-recs add-to-records tname 'in-progress))
               (let* ((super (if (null? (header-extends info)) null (car (header-extends info))))
                      (super-name (if (null? super)
                                      test-base
                                      (if (null? (name-path super))
                                          (cons (id-string (name-id super))
                                                (send type-recs lookup-path (id-string (name-id super)) (lambda () null)))
                                          (name->list super))))
                      (super-record (get-parent-record super-name super tname level type-recs))
                      (members (def-members test))
                      (super-req ((lambda (name-list)
                                    (if (= (length name-list) 1)
                                        (make-req (car name-list) 
                                                  (send type-recs lookup-path (car name-list) (lambda () null)))
                                        (make-req (car name-list) (cdr name-list))))
                                  super-name))
                      (old-loc (send type-recs get-location)))
                 
                 (send type-recs set-location! (def-file test))
                 (set-def-uses! test
                                (remove-dup-reqs 
                                 (cons super-req (get-method-reqs (class-record-methods super-record)))))
                 
                 (unless (and (class-record-class? super-record)
                              (or (equal? super-name test-base)
                                  (member test-base (class-record-parents super-record))))
                   (test-extension-error (class-record-class? super-record)
                                         (header-id info)
                                         super
                                         (name-src super)))
                                                   
                 (let*-values (((old-methods) (class-record-methods super-record))
                               ((f m i)
                                (process-members members old-methods tname type-recs level #f #t))
                               ((ctor?) (has-ctor? m)))
                   
                   (if ctor?
                       (unless (= 0 (length (filter (lambda (m)
                                                      (and (eq? 'ctor (method-record-rtype m))
                                                           (null? (method-record-atypes m))
                                                           (not (memq 'private (method-record-modifiers m)))
                                                           (not (memq 'protected (method-record-modifiers m)))))
                                                    m)))
                         (test-not-visible-ctor-error (header-id info) (def-src test)))
                       (add-ctor test 
                                 (lambda (rec) (set! m (cons rec m))) old-methods (header-id info) level))
                   
                   (valid-field-names? (if (memq level '(beginner intermediate intermediate+access advanced)) 
                                           (append f (class-record-fields super-record)) f)
                                       members m level type-recs)
                   
                   (valid-method-sigs? m members level type-recs)

                   (and (class-fully-implemented? super-record super null null
                                                  m type-recs level)
                        (no-abstract-methods m members level type-recs))
                                      
                   (valid-inherited-methods? (cons super-record null)
                                             (cons (if (null? super)
                                                       (make-name (make-id "Test" #f) 
                                                                  (list (make-id "java" #f)
                                                                        (make-id "test" #f)) #f)
                                                       super) null)
                                             level
                                             type-recs)

                   (check-current-methods (cons super-record null)
                                          m
                                          members
                                          level
                                          type-recs)
                   
                   (let ((record
                          (make-class-record 
                           tname (header-modifiers info) #t #t
                           (append f (filter class-specific-field? (class-record-fields super-record)))
                           (append m (filter (lambda (meth)
                                               (class-specific-method? meth m))
                                             (class-record-methods super-record)))
                           (append i (filter (lambda (i-r)
                                               (not (memq 'private (inner-record-modifiers i-r))))
                                             (class-record-inners super-record)))
                           (cons super-name (class-record-parents super-record))
                           null)))
                     (when put-in-table? 
                       (send type-recs add-class-record record)
                       (send type-recs add-test-class (car tname))
                       )
                                          
                     (for-each (lambda (member)
                                 (when (def? member)
                                   (process-class/iface member package-name type-recs #f put-in-table? level)))
                               members)
                     (send type-recs set-location! old-loc)
                     record))))))
        (cond
          ((class-record? (send type-recs get-class-record tname)) => 
           (lambda (rec) rec))
          (look-in-table?
           (get-record (send type-recs get-class-record tname #f build-record) type-recs))
          (else (build-record))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Code to check for conflicts in method/field/class naming (including types)
  
  (define (valid-iface-extend? records extends)
    (or (null? records)
        (and (memq (car records) (cdr records))
             (extension-error 'ifaces #f (car extends) (name-src (car extends))))
        (valid-iface-extend? (cdr records) (cdr extends))))
  
  (define (valid-iface-implement? records implements)
    (or (null? records)
        (and (memq (car records) (cdr records))
             (extension-error 'implement #f (car implements) (name-src (car implements))))
        (valid-iface-implement? (cdr records) (cdr implements))))
  
  ;valid-field-names? (list field-record) (list member) (list method-record) symbol type-records -> bool
  (define (valid-field-names? fields members methods level type-recs)
    (or (null? fields)
        (and (field-member? (car fields) (cdr fields))
             (let ((f (find-member (car fields) members level type-recs)))
               (if (method? f)
                   (field-name-error 'inherited-conflict-method
                                     (method-name f)
                                     level
                                     (method-src f))
                   (field-name-error 'field (field-name f) level (field-src f)))))
        (and (memq level '(beginner intermediate))
             (or (and (shared-name? (car fields) methods)
                      (let ((f (find-member (car fields) members level type-recs)))
                        (if (method? f)
                            (field-name-error 'inherited-conflict-method
                                              (method-name f)
                                              level
                                              (method-src f))
                            (field-name-error 'method (field-name f) level (field-src f)))))
                 (and (shared-class-name? (car fields) (send type-recs get-class-env))
                      (let ((f (find-member (car fields) members level type-recs)))
                        (if (method? f)
                            (field-name-error 'inherited-conflict-method
                                              (method-name f)
                                              level
                                              (method-src f))
                            (field-name-error 'class (field-name f) level (field-src f)))))))
        (valid-field-names? (cdr fields) members methods level type-recs)))
  
  ;field-member: field-record (list field-record) -> bool
  (define (field-member? field fields)
    (and (not (null? fields))
         (or (equal? (field-record-name field)
                     (field-record-name (car fields)))
             (field-member? field (cdr fields)))))
  
  ;shared-name field-record (list method-record) -> bool
  (define (shared-name? field methods)
    (and (not (null? methods))
         (or (equal? (field-record-name field)
                     (method-record-name (car methods)))
             (shared-name? field (cdr methods)))))
  
  ;shared-class-name?: (U field-record method-record) (list string) -> bool
  (define (shared-class-name? member classes)
    (and (not (null? classes))
         (or (equal? ((if (field-record? member) field-record-name method-record-name) member)
                      (car classes))
             (shared-class-name? member (cdr classes)))))
  
  ;find-member: (U field-record method-record) (list member) symbol type-records -> member
  (define (find-member member-record members level type-recs)
    (when (null? members)
      (printf "~a~n" member-record)
      (error 'internal-error "Find-member given a member that is not contained in the member list"))
    (cond
      ((and (field-record? member-record)
            (field? (car members)))
       (if (equal? (id-string (field-name (car members)))
                   (field-record-name member-record))
           (car members)
           (find-member member-record (cdr members) level type-recs)))
      ((and (method-record? member-record)
            (method? (car members)))
       (if (and (equal? (id-string (method-name (car members)))
                        (method-record-name member-record))
                (= (length (method-record-atypes member-record))
                   (length (method-parms (car members))))
                (andmap type=?
                        (method-record-atypes member-record)
                        (map (lambda (t)
                               (type-spec-to-type t (method-record-class member-record)  level type-recs))
                             (map field-type-spec (method-parms (car members)))))
                (type=? (method-record-rtype member-record)
                        (type-spec-to-type (method-type (car members)) (method-record-class member-record) level type-recs)))
           (car members)
           (find-member member-record (cdr members) level type-recs)))
      ((memq level '(beginner intermediate intermediate+access advanced))
       (let ((given-name ((if (field-record? member-record) field-record-name method-record-name) member-record))
             (looking-at (id-string ((if (field? (car members)) field-name method-name) (car members)))))
         (if (equal? given-name looking-at)
             (car members)
             (find-member member-record (cdr members) level type-recs))))
      (else
       (find-member member-record (cdr members) level type-recs))))
 
  ;valid-method-sigs? (list method-record) (list member) symbol type-records -> bool
  (define (valid-method-sigs? methods members level type-recs)
    (or (null? methods)
        (and (equal? (method-record-name (car methods)) 
                     (method-record-class (car methods)))
             (not (eq? (method-record-rtype (car methods)) 'ctor))
             (let ((m (find-member (car methods) members level type-recs))
                   (class (method-record-class (car methods))))
               (if (field? m)
                   (method-error 'inherited-conflict-field
                                 (field-name m)
                                 null
                                 #f
                                 (car class)
                                 (field-src m)
                                 #f)
                   (method-error 'ctor-ret-value 
                                 (method-name m)
                                 (map field-type (method-parms m))
                                 (type-spec-to-type (method-type) #f level type-recs)
                                 (car class)
                                 (method-src m)
                                 #f))))
        (let ((res (same-method-name? (car methods) (cdr methods)))
              (m (and (not (eq? 'ctor (method-record-rtype (car methods))))
                      (find-member (car methods) members level type-recs)))
              (class (method-record-class (car methods))))
          (and res m (memq level '(beginner intermediate intermediate+access))
               (not (type=? (method-record-rtype (car methods))
                            (method-record-rtype res)))
               (method-error 'bad-ret
                             (method-name m)
                             (map field-type (method-parms m))
                             (method-record-rtype (car methods))
                             (car class)
                             (method-src m)
                             (method-record-rtype res))))
        (and (method-member? (car methods) (cdr methods) level)
             (let ((m (find-member (car methods) members level type-recs))
                   (class (method-record-class (car methods))))
               (if (field? m)
                   (method-error 'inherited-conflict-field
                                 (field-name m)
                                 null
                                 #f
                                 (car class)
                                 (field-src m)
                                 #f)
                   (method-error 'repeated 
                             (method-name m)
                             (map field-type (method-parms m))
                             'void
                             (car class)
                             (method-src m)
                             (eq? (method-record-rtype (car methods)) 'ctor)))))
        (and (memq level `(beginner intermediate))
             (not (eq? (method-record-rtype (car methods)) 'ctor))
             (shared-class-name? (car methods) (send type-recs get-class-env))
             (let ((m (find-member (car methods) members level type-recs))
                   (class (method-record-class (car methods))))
               (if (field? m)
                   (method-error 'inherited-conflict-field
                                 (field-name m)
                                 null
                                 #f
                                 (car class)
                                 (field-src m)
                                 #f)
                   (method-error 'class-name
                                 (method-name m)
                                 (map field-type (method-parms m))
                                 (type-spec-to-type (method-type m) #f level type-recs)
                                 (car class)
                                 (method-src m)
                                 (eq? (method-record-rtype (car methods)) 'ctor)))))
        (valid-method-sigs? (cdr methods) members level type-recs)))
  
  ;same-method-name? method-record (list method-record) -> (U #f method-record)
  (define (same-method-name? method methods)
    (and (not (null? methods))
         (or (and (equal? (method-record-name method)
                          (method-record-name (car methods)))
                  (car methods))
             (same-method-name? method (cdr methods)))))
  
  (define (method-member? method methods level)
    (and (not (null? methods))
         (or (and (equal? (method-record-name method)
                          (method-record-name (car methods)))
                  (type=? (method-record-rtype method) (method-record-rtype (car methods)))
                  (or (memq level '(beginner intermediate))
                      (and (= (length (method-record-atypes method))
                              (length (method-record-atypes (car methods))))
                           (andmap type=? (method-record-atypes method) 
                                   (method-record-atypes (car methods))))))
             (method-member? method (cdr methods) level))))
  
  ;identical-method-member? method-record (listof method-record) -> method-record
  (define (identical-method-member? method methods)
    (and (not (null? methods))
         (or (and (equal? (method-record-name method)
                          (method-record-name (car methods)))
                  (type=? (method-record-rtype method) (method-record-rtype (car methods)))
                  (= (length (method-record-atypes method))
                     (length (method-record-atypes (car methods))))
                  (andmap type=? (method-record-atypes method)
                          (method-record-atypes (car methods)))
                  (car methods))
             (identical-method-member? method (cdr methods)))))
 
  ;valid-inherited-methods?: (list class-record) (list name) symbol type-records -> bool
  (define (valid-inherited-methods? records extends level type-recs)
    (or (null? records)
        (and (check-inherited-method (class-record-methods (car records))
                                     (cdr records)
                                     (car extends)
                                     level
                                     type-recs)
             (valid-inherited-methods? (cdr records) (cdr extends) level type-recs))))
  
  ;check-inherited-method: (list method-record) (list class-record) name symbol type-records -> bool
  (define (check-inherited-method methods records from level type-recs)
    (or (null? methods)
        (and (method-conflicts? (car methods) 
                                (apply append (map class-record-methods records))
                                level)
             (method-error 'inherit-conflict 
                           (make-id (method-record-name (car methods)) #f)
                           (method-record-atypes (car methods))
                           (method-record-rtype (car methods))
                           (id-string (name-id from))
                           (name-src from)
                           #f))
        (check-inherited-method (cdr methods) records from level type-recs)))

  ;method-conflicts?: method-record (list method-record) symbol -> bool
  (define (method-conflicts? method methods level)
    (and (not (null? methods))
         (or (and (equal? (method-record-name method)
                          (method-record-name (car methods)))
                  (or (memq level '(beginner intermediate))
                      (and (= (length (method-record-atypes method)) (length (method-record-atypes (car methods))))
                           (andmap type=? (method-record-atypes method) (method-record-atypes (car methods)))))
                  (not (type=? (method-record-rtype method) (method-record-rtype (car methods)))))
             (method-conflicts? method (cdr methods) level))))                              

  ;check-current-methods: (list method-record) (list method) (list member) symbol type-records -> bool
  (define (check-current-methods records methods members level type-recs)
    (or (null? records)
        (and (check-for-conflicts methods (car records) members level type-recs)
             (check-current-methods (cdr records) methods members level type-recs))))
  
  (define (check-for-conflicts methods record members level type-recs)
    (or (null? methods)
        (and (method-conflicts? (car methods)
                                (class-record-methods record)
                                level)
             (let ((method (find-member (car methods) members level type-recs))
                   (class (class-record-name record)))
               (if (field? method)
                   (method-error 'inherited-conflict-field
                                 (field-name method)
                                 null
                                 #f
                                 (car class)
                                 (field-src method)
                                 #f)
                   (method-error 'conflict
                                 (method-name method)
                                 (map field-type (method-parms method))
                                 (type-spec-to-type (method-type method) #f level type-recs)
                                 (car class)
                                 (method-src method)
                                 #f))))
        (check-for-conflicts (cdr methods) record members level type-recs)))
  
  ;class-fully-implemented? class-record id (list class-record) (list id) (list method) type-records symbol -> bool
  (define (class-fully-implemented? super super-name ifaces ifaces-name methods type-recs level)
    (let ((unimplemented-iface-methods 
           (get-unimplemented-methods (class-record-methods super) (class-record-ifaces super)
                                      ifaces ifaces-name type-recs)))
      (andmap (lambda (unimp iface)
                (or (null? unimp)
                    (implements-all? unimp methods iface level)))
              (car unimplemented-iface-methods) (cadr unimplemented-iface-methods))
      (when (memq 'abstract (class-record-modifiers super))
        (implements-all? (get-methods-need-implementing (class-record-methods super))
                         methods super-name level))
      #;(andmap (lambda (iface iface-name)
                (or (super-implements? iface (class-record-ifaces super) (class-record-methods super))
                    (implements-all? (class-record-methods iface) methods iface-name level)))
              ifaces
              ifaces-name)))
  
  ;super-implements?: class-record (list (list string)) -> boolean
  (define (super-implements? iface ifaces)
    (member (class-record-name iface) ifaces))
  
  ;make-unimplmented-stubs: (list class-record) (list name) (list method-record) type-records -> (list method)
  (define (make-unimplmented-stubs ifaces ifaces-name methods type-recs)
    (letrec ((type->type-spec
              (lambda (t)
                (cond
                  ((symbol? t) (make-type-spec t 0 #f))
                  ((ref-type? t) (make-type-spec (make-name (make-id (ref-type-class/iface t) #f)
                                                            (map (lambda (t) (make-id t #f))
                                                                 (ref-type-path t)) #f)
                                                 0 #f))
                  ((array-type? t) (make-type-spec (type-spec-name (type->type-spec (array-type-type t)))
                                                   (array-type-dim t) #f)))))
             (copy-method-record
              (lambda (m)
                (make-method-record (method-record-name m)
                                    (cons 'abstract (method-record-modifiers m))
                                    (method-record-rtype m)
                                    (method-record-atypes m)
                                    (method-record-throws m)
                                    #f
                                    (method-record-class m))))
             (remove-dups
              (lambda (l)
                (cond
                  ((null? l) l)
                  ((member (car l) (cdr l)) (remove-dups (cdr l)))
                  (else (cons (car l) (remove-dups (cdr l)))))))
             (unimplemented-iface-methods 
              (car (get-unimplemented-methods methods 
                                              (remove-dups (append (map (lambda (iface)
                                                                          (cond 
                                                                            ((id? iface) (list (id-string iface)))
                                                                            ((name? iface) (cons (id-string (name-id iface))
                                                                                                 (map id-string (name-path iface))))))
                                                                        ifaces-name)
                                                                   (map class-record-name ifaces))) null null type-recs))))
      (apply append
             (map (lambda (m-lists)
                    (map (lambda (m)
                           (make-method (cons (make-modifier 'abstract #f) 
                                              (map (lambda (a) (make-modifier a #f)) (method-record-modifiers m)))
                                        (type->type-spec (method-record-rtype m))
                                        null
                                        (make-id (method-record-name m) #f)
                                        (map (lambda (a)
                                               (make-var-decl (make-id (gensym) #f) null
                                                              (type->type-spec a) #f #f))
                                             (method-record-atypes m))
                                        null #f #f 
                                        (copy-method-record m) #f))
                         m-lists))
                  unimplemented-iface-methods))))
  
  ;get-unimplemented-methods: (list method-record) (list (list string)) 
                            ; (list class-record) (list id) type-recs -> (list (list (list method-record)) (list string))
  (define (get-unimplemented-methods methods ifaces iface-records iface-names type-recs)
    (letrec ((method-req-equal 
              (lambda (mrec1 mrec2)
                (and (equal? (method-record-name mrec1) (method-record-name mrec2))
                     (type=? (method-record-rtype mrec1) (method-record-rtype mrec2))
                     (= (length (method-record-atypes mrec1)) (length (method-record-atypes mrec2)))
                     (andmap type=? (method-record-atypes mrec1) (method-record-atypes mrec2)))))
             (method-rec-mem
              (lambda (mrec mrecs)
                (and (not (null? mrecs))
                     (or (method-req-equal mrec (car mrecs))
                         (method-rec-mem mrec (cdr mrecs)))))))
      (list (map (lambda (iface)
                   (filter (lambda (m) (not (method-rec-mem m methods))) (class-record-methods iface)))       
                 (append (map (lambda (iface-spec) (send type-recs get-class-record iface-spec)) ifaces)
                         iface-records))
            (append (map (lambda (iface) (make-name (make-id (car iface) #f) (cdr iface) #f)) ifaces)
                    (map (lambda (iface) 
                           (if (name? iface) iface (make-name iface null #f))) iface-names)))))
  
  ;get-methods-need-implementing: (list method-record) -> (list method-record)
  (define (get-methods-need-implementing methods)
    (let ((abstract-methods (filter (lambda (m) (memq 'abstract (method-record-modifiers m))) methods))
          (non-abstract-methods (filter (lambda (m) (not (memq 'abstract (method-record-modifiers m)))) methods)))
      (filter (lambda (m)
                (not (member m (map method-record-override non-abstract-methods))))
              abstract-methods)))
  
  ;implements-all? (list method-record) (list method) name symbol -> bool
  (define (implements-all? inherit-methods methods name level)
    (or (null? inherit-methods)
        (let* ([current-method (car inherit-methods)]
               [id-name (make-id (method-record-name current-method) #f)] 
               [implementing-method (identical-method-member? current-method methods)])
          (or (and (not implementing-method)
                   (method-error 'not-implement 
                                 id-name
                                 (method-record-atypes current-method)
                                 (method-record-rtype current-method)
                                 (id-string (name-id name))
                                 (id-src (name-id name))
                                 #f))
              (and (memq level '(advanced full))
                   (not (equal-greater-access? (method-record-modifiers implementing-method)
                                               (method-record-modifiers current-method)))
                   (implements-error id-name
                                     (method-record-atypes current-method)
                                     (method-record-modifiers implementing-method)
                                     (method-record-modifiers current-method)
                                     (id-string (name-id name))
                                     (id-src (name-id name))))
              (implements-all? (cdr inherit-methods) methods name level)))))
  
  (define (no-abstract-methods methods members level type-recs)
    (or (null? methods)
        (and (memq 'abstract (method-record-modifiers (car methods)))
             (let ((method (find-member (car methods) members level type-recs))
                   (class (method-record-class (car methods))))
               (method-error 'illegal-abstract 
                             (method-name method) 
                             (map field-type (method-parms method))
                             (type-spec-to-type (method-type method) #f level type-recs)
                             (car class)
                             (method-src method)
                             #f)))
        (no-abstract-methods (cdr methods) members level type-recs)))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Methods to process fields and methods
  
  ;; process-members: (list members) (list method-record) (list string) type-records symbol boolean boolean-> 
  ;;                     (values (list field-record) (list method-record) (list inner-record))
  (define (process-members members inherited-methods cname type-recs level iface? test? . args)
    (let loop ((members members)
               (fields null)
               (methods null)
               (inners null))
      (cond
        ((null? members) (values fields methods inners))
        ((field? (car members))
         (loop (cdr members)
               (cons (process-field (car members) cname type-recs level) fields)
               methods
               inners))
        ((method? (car members))
         (loop (cdr members)
               fields
               (cons (if (null? args)
                         (process-method (car members) inherited-methods cname type-recs level iface? test?)
                         (process-method (car members) inherited-methods cname type-recs level iface? test? (car args)))
                         methods)
               inners))
        ((def? (car members))
         (loop (cdr members)
               fields
               methods
               (cons (process-inner (car members) cname type-recs level) inners)))
        (else
         (loop (cdr members)
               fields
               methods
               inners)))))
  
  ;; process-field: field (string list) type-records symbol -> field-record
  (define (process-field field cname type-recs level)
    (set-field-type! field (type-spec-to-type (field-type-spec field) cname level type-recs))
    (make-field-record (id-string (field-name field)) 
                       (check-field-modifiers level (field-modifiers field))
                       (var-init? field)
                       (if (class-name) (cons (class-name) (cdr cname)) cname)
                       (field-type field)))
                  
  ;; process-method: method (list method-record) (list string) type-records symbol boolean boolean -> method-record  
  (define (process-method method inherited-methods cname type-recs level iface? test? . args)
    (let* ((name (id-string (method-name method)))
           (parms (map (lambda (p)
                         (set-field-type! p (type-spec-to-type (field-type-spec p) cname level type-recs))
                         (field-type p))
                       (method-parms method)))
           (mods 
            (cond
              [(not (null? args)) (cons (car args) (method-modifiers method))]
              [(and iface? (not (memq 'public (map modifier-kind (method-modifiers method)))))
               (cons (make-modifier 'public #f) (method-modifiers method))]
              [else (method-modifiers method)]))
           (ret (type-spec-to-type (method-type method) cname level type-recs))
           (throws (filter (lambda (n)
                             (not (or (is-eq-subclass? n runtime-exn-type type-recs))))
                           ;(is-eq-subclass? n error-type type-recs))))
                           (map (lambda (t)
                                  (let ((n (make-ref-type 
                                            (id-string (name-id t))
                                            (if (null? (name-path t))
                                                (send type-recs lookup-path (id-string (name-id t)) 
                                                      (lambda () null))
                                                (map id-string (name-path t))))))
                                    (if (is-eq-subclass? n throw-type type-recs)
                                        n
                                        (throws-error (name-id t) (name-src t)))))
                                (method-throws method))))
           (over? (overrides? name parms inherited-methods)))
      
      (when (test-method? method)
        (unless test? (testcase-not-in-test name (car cname) (id-src (method-name method))))
        (unless (null? parms) (testcase-args-error name (car cname) (id-src (method-name method))))
        (unless (eq? 'boolean ret) (testcase-ret-error name (car cname) ret (id-src (method-name method)))))
            
      (when (and (memq level '(beginner intermediate))
                 (member name (map method-record-name inherited-methods))
                 (not over?))
        (inherited-overload-error (car cname) name parms 
                                  (method-record-atypes 
                                   (car (filter (lambda (m) (equal? (method-record-name m) name))
                                                inherited-methods)))
                                  (id-src (method-name method))))
            
      (when (eq? ret 'ctor)
        (if (regexp-match "\\." (car cname))
            (begin
              (unless (equal? name (bytes->string/locale (filename-extension (car cname))))
                (not-ctor-error name (car cname) (id-src (method-name method))))
              (set! name (car cname))
              (set-id-string! (method-name method) (car cname)))
            (unless (equal? name (car cname))
              (not-ctor-error name (car cname) (id-src (method-name method))))))
            
      (check-parm-names (method-parms method) name cname)
      
      (when over?
        (when (memq level `(advanced full))
          (check-gtequal-access mods name parms cname over? (method-src method)))
        
        (unless (type=? ret (method-record-rtype over?))
          (override-return-error name parms cname ret 
                        (method-record-rtype over?)
                        (type-spec-src (method-type method))))
        
        (when (memq 'final (method-record-modifiers over?))
          (override-access-error 'final level 
                                 name parms cname (method-record-class over?) 
                                (id-src (method-name method))))
        
        (when (and (memq level '(advanced full))
                   (memq 'static (method-record-modifiers over?)))
          (override-access-error 'static level
                                 name parms cname (method-record-class over?)
                                 (id-src (method-name method))))
        (when (eq? level 'full)
          (check-throws-match throws method cname over? type-recs)))
      
      (let ((record (make-method-record name
                                        (check-method-modifiers level mods (eq? 'ctor ret))
                                        ret
                                        parms
                                        throws
                                        over?
                                        (if (class-name) (cons (class-name) (cdr cname)) cname))))
        (set-method-rec! method record)
        record)))
  
  ;process-inner def (list name) type-records symbol -> inner-record
  (define (process-inner def cname type-recs level)
    (make-inner-record (bytes->string/locale (filename-extension (id-string (def-name def))))
                       (id-string (def-name def))
                       (map modifier-kind (header-modifiers (def-header def)))
                       (class-def? def)))

  ;overrides?: string (list type) (list method-record) -> (U bool method-record)
  (define (overrides? mname parms methods)
    (and (not (null? methods))
         (if (and (equal? mname
                          (method-record-name (car methods)))
                  (= (length parms)
                     (length (method-record-atypes (car methods))))
                  (andmap type=? parms (method-record-atypes (car methods))))
             (car methods)
             (overrides? mname parms (cdr methods)))))
    
  ;check-parm-names: (list field) string (list string) -> void
  (define (check-parm-names parms meth class)
    (or (null? parms)
        (and (parm-member? (car parms) (cdr parms))
             (repeated-parm-error (car parms) meth class))
        (check-parm-names (cdr parms) meth class)))

  ;parm-member? field (list field) -> bool
  (define (parm-member? p parms)
    (and (not (null? parms))
         (or (equal? (id-string (field-name p))
                     (id-string (field-name (car parms))))
             (parm-member? p (cdr parms)))))
  
  ;check-gtequal-access: (list modifier) string (list type) (list string) method-record src -> void
  (define (check-gtequal-access mods name parms class over src)
    (let ((old-mods (method-record-modifiers over))
          (old-class (method-record-class over)))
      (cond
        ((memq 'public old-mods)
         (unless (memq 'public (map modifier-kind mods))
           (override-access-error 'public 'full name parms class (method-record-class over) src)))
        ((memq 'protected old-mods) 
         (unless (or (memq 'public (map modifier-kind mods))
                     (memq 'protected (map modifier-kind mods)))
           (override-access-error 'protected 'full name parms class (method-record-class over) src)))
        (else 
         (unless (or (memq 'public (map modifier-kind mods))
                     (not (memq 'private (map modifier-kind mods)))
                     (not (memq 'protected (map modifier-kind mods))))
           (override-access-error 'package 'full name parms class (method-record-class over) src))))))

  ;check-throws-same: (list type) method (list string) method-record type-records -> void
  (define (check-throws-match throws method cname over type-recs)
    (if (= 0 (length (method-record-throws over)))
        (for-each (lambda (t) 
                    (unless (is-subclass-of1? t (method-record-throws over) type-recs)
                      (inherited-throw-error 'subclass 
                                        (method-name method)
                                        (method-parms method)
                                        cname
                                        (method-record-class over)
                                        t
                                        (id-src (find-type t (method-throws method))))))
                  throws)
        (inherited-throw-error 'num (method-name method) (method-parms method) cname
                          (method-record-class over) #t (method-src method))))
    
  ;is-subclass-of1?: type (list type) type-records-> bool
  (define (is-subclass-of1? throw thrown type-recs)
    (and (not (null? thrown))
         (or (is-eq-subclass? throw (car thrown) type-recs)
             (is-subclass-of1? throw (cdr thrown) type-recs))))
  
  ;find-type type (list name) -> src
  (define (find-type throw throws)
    (or (and (equal? (ref-type-class/iface throw)
                     (id-string (name-id (car throws))))
             (name-id (car throws)))
        (find-type throw (cdr throws))))  
  
  ;-----------------------------------------------------------------------------------
  ;Code to check modifiers
  
  ;check-class-modifiers: symbol symbol (list modifier) -> (list symbol)
  (define (check-class-modifiers level kind mods)
    (when (and ((valid-class-mods? kind) level mods)
               (not (final-and-abstract? mods))
               (not (duplicate-mods? mods)))
      (map modifier-kind mods)))
  
  ;check-interface-modifiers: (list modifier) symbol -> (list symbol)
  (define (check-interface-modifiers level mods)
    (when (and (valid-interface-mods? level mods)
               (not (duplicate-mods? mods)))
      (map modifier-kind mods)))

  ;check-method-modifiers: symbol (list modifier) -> (list symbol)
  (define (check-method-modifiers level mods ctor?)
    (when (and (not (duplicate-mods? mods))
               (one-of-access? mods)
               (if ctor?
                   (valid-method-mods? 'ctor mods)
                   (valid-method-mods? level mods))
               (not (native-and-fp? mods))
               (or (not (memq 'abstract (map modifier-kind mods)))
                   (valid-method-mods? 'abstract mods)))
      (map modifier-kind mods)))
  
  ;check-field-modifiers: symbol (list modifier) -> (list symbol)
  (define (check-field-modifiers level mods)
    (when (and (not (duplicate-mods? mods))
               (one-of-access? mods)
               (valid-field-mods? level mods)
               (not (volatile-and-final? mods)))
      (map modifier-kind mods)))
  
  ;make-valid-mods: (symbol -> (list symbol)) (symbol -> symbol) -> (symbol (list modifier) -> bool)
  (define (make-valid-mods valids-choice error-type)
    (letrec ((tester
              (lambda (level mods)
                (or (null? mods)
                    (and (not (memq (modifier-kind (car mods)) (valids-choice level)))
                         (modifier-error (error-type level) (car mods)))
                    (tester level (cdr mods))))))
      tester))

  ;valid-*-mods?: symbol -> (symbol (list modifier) -> bool)
  (define (valid-class-mods? kind)
    (make-valid-mods 
     (lambda (level)
       (case kind
         ((top) '(public abstract final strictfp))
         ((member) '(public private protected abstract final static))
         ((anon) '())
         ((statement) '(public private protected abstract final))))
     (lambda (x) 'invalid-class)))
  (define valid-interface-mods?
    (make-valid-mods (lambda (x) '(public abstract strictfp))
                     (lambda (x) 'invalid-iface)))
  (define valid-field-mods?
    (make-valid-mods
     (lambda (level)
       (case level
         ((beginner) '(public final))
         ((intermediate) '(public))
         ((intermediate+access) '(public protected private))
         ((advanced) '(public protected private static))
         ((full) `(public protected private static final transient volatile))))
     (lambda (x) 'invalid-field)))  
  (define valid-method-mods?
    (make-valid-mods
     (lambda (level)
       (case level
         ((beginner) '(public))
         ((intermediate) '(public abstract))
         ((intermediate+access) '(public protected private abstract))
         ((advanced) `(public protected private abstract static final))
         ((full) '(public protected private abstract static final synchronized native strictfp))
         ((abstract) '(public protected abstract))
         ((ctor) '(public protected private))))
     (lambda (level)
       (case level
         ((abstract) 'invalid-abstract)
         ((ctor) 'invalid-ctor)
         (else 'invalid-method)))))
  
  ;one-access: symbol symbol symbol (list modifiers) -> bool
  (define (one-access is check1 check2 mods)
    (and (eq? is (modifier-kind (car mods)))
         (or (memq check1 (map modifier-kind (cdr mods)))
             (memq check2 (map modifier-kind (cdr mods))))
         (modifier-error 'access (car mods))))

  ;one-of-access?: (list modifier) -> bool
  (define (one-of-access? mods)
    (or (null? mods)
        (one-access 'public 'private 'protected mods)
        (one-access 'private 'public 'protected mods)
        (one-access 'protected 'private 'public mods)
        (one-of-access? (cdr mods))))

  ;make-not-two: symbol symbol symbol -> ((list modifier) -> bool)
  (define (make-not-two first second error)
    (letrec ((tester
              (lambda (mods)
                (and (not (null? mods))
                     (or (and (eq? first (modifier-kind (car mods)))
                              (memq second (map modifier-kind (cdr mods)))
                              (modifier-error error (car mods)))
                         (and (eq? second (modifier-kind (car mods)))
                              (memq first (map modifier-kind (cdr mods)))
                              (modifier-error error (car mods)))
                         (tester (cdr mods)))))))
      tester))
  
  (define final-and-abstract? (make-not-two 'final 'abstract 'final-abstract))
  (define volatile-and-final? (make-not-two 'volatile 'final 'final-volatile))
  (define native-and-fp? (make-not-two 'native 'strictfp 'native-strictfp))
    
  ;duplicate-mods?: (list modifier) -> bool
  (define (duplicate-mods? mods)
    (and (not (null? mods))
         (or (and (memq (modifier-kind (car mods))
                        (map modifier-kind (cdr mods)))
                  (modifier-error 'dups (car mods)))
             (duplicate-mods? (cdr mods)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Error raising code: code that takes information about the error message and throws the error
  
  ;repeated-def-name-error: id bool symbol src -> void
  (define (repeated-def-name-error name class? level src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (format "~a ~a and another class or interface have the same name. ~a names must be unique."
                           (if class? "Class" "Interface") n (if class? "Class" "Interface"))
                   n src)))
  
  ;modifier-error: symbol modifier -> void
  (define (modifier-error kind mod)
    (let ((m (modifier-kind mod))
          (src (modifier-src mod)))
      (raise-error m
                   (case kind
                     ((dups) 
                      (format "Modifier ~a may only appear once in a declaration, it occurs multiple times here." m))
                     ((access)
                      "Declaration may only be one of public, private, or protected, more than one occurs here.")
                     ((invalid-iface)
                      (format "Modifier ~a is not valid for interfaces." m))
                     ((invalid-class)
                      (format "Modifier ~a is not valid for classes." m))
                     ((invalid-field)
                      (format "Modifier ~a is not valid for fields." m))
                     ((invalid-method)
                      (format "Modifier ~a is not valid for methods." m))
                     ((invalid-ctor)
                      (format "Modifier ~a is not valid for constructors." m))
                     ((invalid-abstract)
                      (format "Modifier ~a is not valid for an abstract method." m))
                     ((final-abstract) "Class declared final and abstract which is not allowed.")
                     ((final-volatile) "Field declared final and volatile which is not allowed.")
                     ((native-strictfp) "Method declared native and strictfp which is not allowed."))
                   m src)))

  ;dependence-error: symbol id src -> void
  (define (dependence-error kind name src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (case kind
                     ((immediate) (format "~a may not extend itself, which it does here." n))
                     ((cycle) 
                      (format "~a is illegally dependent on itself, potentially through other definitions." n)))
                   n src)))

  ;extension-error: symbol id name src -> void
  (define (extension-error kind name super src) 
    (let ((n (if name (id->ext-name name) name))
          (s (id->ext-name (name-id super))))
      (raise-error 
       s
       (case kind
         ((final) 
          (format "Final classes may never be extended, therefore final class ~a may not be extended by ~a." s n))
         ((implement) 
          (format 
           "A class may only declare an implemented interface once, this class declares it is implementing ~a more than once."
           s))
         ((ifaces) 
          (format "An interface may only declare each extended interface once, ~a declares this interface more than once." s))
         ((iface-class) 
          (format "Interfaces may never extend classes, interface ~a has attemped to extend ~a, which is a class." n s))
         ((class-iface) 
          (format "Classes may never extend interfaces, class ~a has attempted to extend ~a, which is an interface." n s))
         ((implement-class) 
          (format "Only interfaces may be implemented, class ~a has attempted to implement class ~a." n s)))
       s src)))
  
  ;test-extension-error: boolean id name src -> void
  (define (test-extension-error class? name super src)
    (let ([n (id->ext-name name)]
          [s (id->ext-name (name-id super))])
      (raise-error
       'extends
       (if class?
           (format "Tests may only extend other tests.~nFound ~a, which is not a test and cannot be the parent of test ~a."
                   s n)
           (format "Tests may not extend interfaces. Found interface ~a for test ~a." s n))
       'extends src)))
  
  ;test-not-visible-ctor-error: id src -> void
  (define (test-not-visible-ctor-error name src)
    (raise-error
     'test
     (format 
      "Tests must have a non-private constructor expecting no arguments. ~a does not have a matching constructor."
      (id->ext-name name))
     'test src))
  
  ;method-error: symbol id (list type) type string src bool -> void
  (define (method-error kind name parms ret class src ctor?)
    (if (eq? kind 'inherited-conflict-field)
        (let ((n (id->ext-name name)))
          (raise-error n (format "Field ~a conflicts with a method of the same name from ~a." n class) n src))
        (let ((m-name (method-name->ext-name (id-string name) null))
              (m-full-name (method-name->ext-name (id-string name) parms))
              (r-name (type->ext-name ret)))
          (raise-error 
           m-name
           (case kind
             ((illegal-abstract)
              (format 
               "Abstract method ~a is not allowed in non-abstract class ~a, abstract methods must be in abstract classes." 
               m-full-name class))
             ((repeated)
              (format "~a ~a has already been written in this class, ~a, and cannot be written again." 
                      (if ctor? "Constructor" "Method") m-full-name class))
             ((inherit-conflict)
              (format "Inherited method ~a from ~a conflicts with another method of the same name." m-full-name class))
             ((conflict)
              (format "Method ~a conflicts with a method inherited from ~a" m-full-name class))
             ((not-implement) (format "Method ~a returning ~a from ~a should be implemented and was not." m-full-name r-name class))
             ((ctor-ret-value)
              (format "Constructor ~a for class ~a has a return type, which is not allowed." m-full-name class))
             ((class-name)
              (format "Method ~a from ~a has the same name as a class, which is not allowed." m-full-name class))
             ((bad-ret)
              (format "Methods with the same name must have the same return type. Found definitions of method ~a in ~a with return types ~a and ~a."
                      m-full-name class r-name (type->ext-name ctor?))))
           m-name src))))

  ;implements-error: id (list type) (list symbol) (list symbol) string src -> void
  (define (implements-error name parms current-mods parent-mods class src)
    (let ([m-name (method-name->ext-name (id-string name) parms)]
          [current-access (extract-access current-mods)]
          [parent-access (extract-access parent-mods)])      
      (raise-error
       'implements
       (format "Method ~a must be at least ~a to implement the ~a method from ~a; ~a access is not sufficient."
               m-name (if (eq? parent-access 'package) 'default parent-access) m-name
               class (if (eq? current-access 'package) 'default current-access))
       m-name src)))
  
  ;testcase-not-in-test: string string src -> void
  (define (testcase-not-in-test name class src)
    (raise-error
     'testcase
     (format "Testcase ~a may not appear in class ~a. Testcases may only occur in tests." name class)
     'testcase src))
  
  ;testcase-args-error: string string src -> void
  (define (testcase-args-error name class src)
    (raise-error 'testcase
                 (format "A testcase cannot require parameters. testcase ~a in ~a specifies arguments."
                         name class)
                 'testcase src))
  
  ;testcase-ret-error: string string type src -> void
  (define (testcase-ret-error name class ret src)
    (raise-error 'testcase
                 (format "A testcase must return a boolean. testcase ~a from ~a returns a ~a instead."
                         name class (type->ext-name ret))
                 'testcase src))
  
  ;inherited-overload-error: string string (list type) (list type) src -> void
  (define (inherited-overload-error curr-class name new-type inherit-type src)
    (let* ((n (string->symbol name))
           (nt (map type->ext-name new-type))
           (nt-l (length nt))
           (gt (map type->ext-name inherit-type))
           (gt-l (length gt)))
      (raise-error 
       (string->symbol curr-class)
       (string-append
        (format "Attempted to override method ~a, but it should have " n)
        (cond
          ((= gt-l 0) "no arguments.~n")
          ((= gt-l 1) (format "1 argument with type ~a.~n" (car gt)))
          (else (format "~a arguments with types ~a." gt-l gt)))
        (cond
          ((= nt-l 0) "Given a method with no arguments.")
          ((= nt-l 1) (format "Given a method with one argument with type ~a." (car nt)))
          (else (format "Given a method with ~a arguments with types ~a." nt-l nt))))
       (string->symbol curr-class) src)))
  
  ;not-ctor-error: string string src -> void
  (define (not-ctor-error meth class src)
    (let ((n (string->symbol meth)))
      (raise-error 
       n
       (format "~a~n~a"
               (format "Method ~a has no return type and does not have the same name as the class, ~a."
                       n class)
               "Only constructors may have no return type, but must have the name of the class.")
       n src)))

  ;beginner-ctor-error: symbol id src -> void
  (define (beginner-ctor-error kind class src) 
    (let ((n (id->ext-name class)))
      (raise-error n (case kind
                       ((none) (format "Class ~a must have a constructor." n))
                       ((abstract) (format "Abstract class ~a may not have a constructor." n))) n src)))

  ;default-ctor-error symbol id string src symbol -> void
  (define (default-ctor-error kind name parent src level)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (case kind
                     ((private)
                      (if (memq level '(beginner intermediate))
                          (format "Class ~a cannot extend ~a." n parent)
                          (format "Class ~a cannot access the default constructor of ~a, which is private." n parent)))
                     ((non-accessible)
                      (if (memq level '(beginner intermediate))
                          (format "Class ~a must have a constructor due to its extension of class ~a." n parent)
                          (format "Class ~a cannot access a default constructor for ~a." n parent)))
                     ((throws)
                      (format "Class ~a cannot use the default constructor for ~a, as ~a's default contains a throws clause."
                              n parent parent)))
                   n src)))
  
  ;inherited-throw-error:symbol string (list type) (list string) string type src -> void
  (define (inherited-throw-error kind m-name parms class parent throw src)
    (raise-error 
     'throws
     (case kind
       ((num) 
        (format 
         "Method ~a in ~a overrides a method from ~a: Method in ~a should throw no types if original doesn't."
         (method-name->ext-name m-name parms) (car class) parent (car class)))
       ((subclass)
        (let ((line1 (format "Method ~a in ~a overrides from a method from ~a."
                             (method-name->ext-name m-name parms) (car class) parent))
              (line2 
               (format 
                "All types thrown by overriding method in ~a must be subtypes of original throws: ~a is not."
                (car class) (type->ext-name throw))))
          (format "~a~n~a" line1 line2))))
     'throws src))
                      
  ;return-error string (list type) (list string) type type src -> void
  (define (override-return-error name parms class ret old-ret src)
    (let ((name (string->symbol name))
          (m-name-short (method-name->ext-name name null))
          (m-name (method-name->ext-name name parms)))
      (raise-error 
       name
       (format 
        "~a~n~a"
        (format "Method ~a of class ~a overrides an inherited method, in overriding the return type must remain the same."
                m-name (car class))
        (format "~a's return has changed from ~a to ~a." m-name-short (type->ext-name old-ret) (type->ext-name ret)))
       name src)))
  
  ;override-access-error symbol symbol string (list type) (list string) string src -> void
  (define (override-access-error kind level name parms class parent src)
    (let ((name (string->symbol name))
          (m-name (method-name->ext-name name parms)))
      (raise-error name
                   (case kind
                     ((final) 
                      (if (eq? level 'full)
                          (format 
                           "Method ~a in ~a attempts to override final method from ~a, final methods may not be overridden"
                           m-name (car class) (if (list? parent) (car parent) parent))
                          (format "Method ~a from ~a cannot be overridden in ~a" name parent (car class))))
                     ((static)
                      (format "Method ~a in ~a attempts to override static method from ~a, which is not allowed."
                              m-name (car class) parent))
                     ((public) 
                      (format "Method ~a in ~a must be public to override public method from ~a, ~a is not public." 
                              m-name (car class) parent name))
                     ((protected) 
                      (format 
                       "Method ~a in ~a must be public or protected to override protected method from ~a, it is neither"
                       m-name (car class) parent))
                     ((package) 
                      (format "Method ~a in ~a must be public, or have no access modifier, to override method from ~a"
                              m-name (car class) parent)))
                   name src)))
  
  ;repeated-parm-error: field string (list string) -> void
  (define (repeated-parm-error parm meth class)
    (let ((name (id->ext-name (field-name parm))))
      (raise-error name
                   (format 
                    "Method parameters may not share names, ~a in ~a cannot have multiple parameters with the name ~a."
                    meth (car class) name)
                   name (id-src (field-name parm)))))

  ;field-name-error: symbol id symbol src -> void
  (define (field-name-error kind name level src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (case kind
                     ((field) 
                      (format 
                       "Each field in a class must have a unique name. Multiple fields have been declared with the name ~a." 
                       n))
                     ((method) 
                      (format "~a has been declared as a field and a method, which is not allowed." n))
                     ((class) 
                      (format "~a has been declared as a field and a ~a, which is not allowed." n
                              (if (eq? level 'intermediate) "class or interface" "class")))
                     ((inherited-conflict-method)
                      (format "Method ~a conflicts with an inherited field of the same name." n)))
                   n src)))
  
  ;import-error: name src -> void
  (define (import-error imp src)
    (raise-error 'import
                 (format "Import ~a not found." (path->ext (name->path imp)))
                 'import src))
  
  ;import-dup-error: symbol string [list id] src -> void
  (define (import-dup-error kind name path src)
    (raise-error 'import
                 (case kind
                   [(star) (format "~a cannot be used to name a class or interface, as it conflicts with an import from ~a."
                                   name (path->ext path))]
                   [(no-star)
                    (format "Class or interface ~a~a cannot be imported, as it conflicts with a class or interface in this file."
                            name (if (null? path) "" (format " from ~a" (path->ext path))))])
                 (string->symbol name)
                 src))

  ;file-error: symbol (list string) src symbol -> void
  (define (file-error kind path src level)
    (if (eq? level 'full)
        (let ((k (if (eq? kind 'file) 'file-not-found 'directory-not-found)))
          (raise-error k
                       (case kind
                         ((file) (format "Required file ~a, for class or interface ~a, not found" 
                                         (string-append (path->ext path) ".java") (path->ext path)))
                         ((dir) (format "Required directory, for package ~a not found" (path->ext path))))
                       k src))
        (raise-error (string->symbol (car path))
                     (case kind
                       ((file) (format "Class or interface ~a is not known." (path->ext path)))
                       ((dir) (format "Package ~a is not known." (path->ext path))))
                     (string->symbol (car path))
                     src)))

  ;used-restricted-import: string (list string) src -> void
  (define (used-restricted-import class path src)
    (raise-error 'import
                 (format "Imported class, ~a, cannot be imported or used." (path->ext (cons class path)))
                 'import src))

  
  ;throws-error id src -> void
  (define (throws-error t src)
    (raise-error 'throws
                 (format "Thrown class must be a subtype of Throwable: Given ~a." (id->ext-name t))
                 'throws src))
        
  (define build-info-location (make-parameter #f))
  (define raise-error (make-error-pass build-info-location))
  
  )
