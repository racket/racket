(module compile scheme/base
  (require "parameters.ss" "ast.ss" "types.ss" "parser.ss" "build-info.ss" "check.ss" "to-scheme.ss" "profj-pref.ss")
  (require #;mzlib/list
           #;mzlib/file
           scheme/path
           scheme/class)

  (provide compile-java compile-interactions compile-files compile-ast compile-interactions-ast
           compilation-unit-code compilation-unit-contains set-compilation-unit-code!
           read-record write-record
           set-syntax-location create-type-record
           compile-to-ast)
  
  (define (set-syntax-location so) (syntax-location so))
  
  (define (create-type-record)
    (let ((t (make-object type-records)))
      (set-importer! t find-implicit-import)
      (classpath (get-classpath))
      (load-lang t)
      t))
  
  ;kind = 'file | 'port
  ;level = 'beginner | 'intermediate | 'intermediate+access | 'advanced | 'full
  
  ;compile: kind kind level (U #f string) (U #f port) (U #f location) -> (U (list compilation-unit) void)
  (define (compile-java src dest level name port loc . type-recs)
    (when (and (eq? src 'file)
               (not (file-exists? name)))
      (error 'compile-java "compile-java given file that does not exist: ~a" name))
    (when (and (eq? src 'file)
               (path? name)
               (regexp-match #".djava$" (path->bytes name)))
      (dynamic? #t))
    (when (null? (classpath)) (get-classpath))
    (let ((type-recs (if (null? type-recs)
                         (make-object type-records)
                         (car type-recs))))
      (cond
        ((and (eq? src 'file) (eq? dest 'file))
         (let-values  (((path-base file dir?) (split-path (path->complete-path (build-path name)))))
           (let ((compiled-path (build-path path-base "compiled" (path-add-suffix
                                                                  (path-replace-suffix file ".ss")
                                                                  ".zo")))
                 (type-path (build-path path-base "compiled" (path-replace-suffix file ".jinfo"))))
             (unless 
                 (and (file-exists? compiled-path)
                      (file-exists? type-path)
                      (equal? (version) (call-with-input-file compiled-path get-version))
                      (read-record type-path)
                      (> (file-or-directory-modify-seconds compiled-path)
                         (file-or-directory-modify-seconds (build-path name)))
                      (> (file-or-directory-modify-seconds compiled-path)
                         (file-or-directory-modify-seconds (build-path (collection-path "mzlib") "contract.ss"))))
               (call-with-input-file name (lambda (port) (compile-to-file port name level)))))))
        ((eq? dest 'file)
         (compile-to-file port loc level))
        ((eq? src 'file)
         (let-values  (((path-base file dir?) (split-path (path->complete-path (build-path name)))))
           (let ((compiled-path (build-path path-base "compiled" (path-add-suffix
                                                                  (path-replace-suffix file ".ss")
                                                                  ".zo")))
                 (type-path (build-path path-base "compiled" (path-replace-suffix file ".jinfo"))))
             (unless (or (and (file-exists? compiled-path)
                              (> (file-or-directory-modify-seconds compiled-path)
                                 (file-or-directory-modify-seconds (build-path name)))
                              (> (file-or-directory-modify-seconds compiled-path)
                                 (file-or-directory-modify-seconds (build-path (collection-path "mzlib") "contract.ss"))))
                         (and (file-exists? type-path)
                              (read-record type-path)))
               (call-with-input-file 
                   name
                 (lambda (port) (compile-java-internal port name type-recs #f level)))))))
        (else
         (compile-java-internal port loc type-recs #f level)))))

  (define (compile-module expr)
    (parameterize ([current-namespace (make-base-namespace)])
      (compile expr)))

  ;compile-to-file: port location level -> void
  ;Should have side-effect of writing to file all files needed for compilation
  (define (compile-to-file port location level)
    (let ((type-recs (make-object type-records)))
      (send type-recs set-compilation-location
            location 
            (if (equal? (file-name-from-path location) location)
                (build-path "compiled")
                (build-path (substring (path->string location) 0 
                                       (- (string-length (path->string location))
                                          (string-length (path->string (file-name-from-path location))))) "compiled")))
      (for-each (lambda (dependents)
                  (let ((names (compilation-unit-contains dependents))
                        (syntaxes (compilation-unit-code dependents))
                        (locations (compilation-unit-locations dependents)))
                    ;(print-struct #t)
                    ;(printf "names ~a~n" names)
                    ;(printf "depends ~a~n~n" (compilation-unit-depends dependents))
                    (unless (= (length names) (length syntaxes))
                      ;(printf "Writing a composite file out~n")
                      ;(printf "~a~n~n" (syntax-object->datum (car syntaxes)))
                      (call-with-output-zo-file* location
                                                 (build-path (send type-recs get-compilation-location)
                                                             (file-name-from-path
                                                              (send type-recs get-composite-location (car names))))
                                                 (lambda (port) (write (compile-module (car syntaxes)) port)) 'truncate/replace)
                      (set! syntaxes (cdr syntaxes)))
                    (unless (= (length names) (length syntaxes) (length locations))
                      (error 'compile-to-file "Internal error: compilation unit not represented as expected"))
                    (for-each (lambda (name code location)
                                ;(printf "~a~n~n" (syntax-object->datum code))
                                (send type-recs set-location! location)
                                (let ((directory (send type-recs get-compilation-location)))
                                  (unless (directory-exists? directory) (make-directory directory))
                                  (call-with-output-zo-file* location
                                                             (build-path directory (string-append name "_ss.zo"))
                                                             (lambda (port) (write (compile-module code) port))
                                                             'truncate/replace)
                                  (call-with-output-file* (build-path directory (string-append name ".jinfo"))
                                                          (lambda (port) (write-record (send type-recs get-class-record 
                                                                                             (list name)
                                                                                             #f
                                                                                             class-record-error)
                                                                                       port))
                                                          #:exists 'truncate/replace)))
                              names syntaxes locations)))
                (compile-java-internal port location type-recs #t level))))

  ;; call-with-output-zo-file* path-string path-string proc [symbol ...] -> 
  ;;  Like call-with-output-file*, but takes an extra initial path to use
  ;;  as an original location, so that marshaled paths in a generated .zo file
  ;;  can be written as relative paths
  (define (call-with-output-zo-file* loc name proc flag)
    (let ([dir (and (path-string? loc)
                    (path-only (path->complete-path loc)))])
      (parameterize ([current-write-relative-directory dir])
        (call-with-output-file* name proc #:exists flag))))
  
  (define (class-record-error) (error 'compile-to-file "Internal error: class record not found"))
  
  ;package: (list string)
  
  ;compile-files: (list (list (list path) package)) boolean symbol ->
  ;               (list (list package (list (list compiliation-unit)) (list class-record)))
  (define (compile-files files to-file? level)
    (when (null? (classpath)) (classpath (get-classpath)))
    (coverage? #f)
    (let ((type-recs (make-object type-records))
          (get-class-names 
           (lambda (files)
             (map (lambda (f) (path->string (path-replace-suffix (file-name-from-path f) "")))
                  files))))
      (map (lambda (package-files)
             (let* ((files (car package-files))
                    (package-name (cadr package-files))
                    (class-names (get-class-names files)))
               (list package-name
                     (filter (lambda (t) t)
                             (map (lambda (file class)
                                    (let ((existing-record (send type-recs get-class-record (cons class package-name) #f
                                                                 (lambda () #f))))
                                      (and (or (not existing-record) 
                                               (procedure? existing-record))
                                           (call-with-input-file file 
                                             (lambda (port) 
                                               (compile-java-internal port file type-recs to-file? level))))))
                                  files class-names))
                     (map (lambda (class)
                            (send type-recs get-class-record (cons class package-name) #f (lambda () (error 'internal-error))))
                          class-names))))
           files)))
  
  (define (compile-ast ast level type-recs)
    (packages null)
    (check-list null)
    (load-lang type-recs)
    (build-info ast level type-recs #f)
    (unless (null? (check-list))
      (check-defs (car (check-list)) level type-recs))
    (remove-from-packages ast type-recs)
    (reverse (translate-program ast type-recs))
    #;(order-cus (translate-program ast type-recs)
               type-recs))
  
  (define (compile-to-ast port location type-recs file? level)
    (packages null)
    (check-list null)
    (to-file file?)
    (let ((ast (parse port location level)))
      (remember-main ast)
      (load-lang type-recs)
      (set-importer! type-recs find-implicit-import)
      (build-info ast level type-recs #f)
      (unless (null? (check-list))
        (check-defs (car (check-list)) level type-recs))
      (remove-from-packages ast type-recs)))             
  
  ;compile-java-internal: port location type-records bool level-> (list compilation-unit)
  (define (compile-java-internal port location type-recs file? level)
    (packages null)
    (check-list null)
    (to-file file?)
    (let ((ast (parse port location level)))
      (remember-main ast)
      (load-lang type-recs)
      (set-importer! type-recs find-implicit-import)
      (build-info ast level type-recs #f)
      (unless (null? (check-list))
        (check-defs (car (check-list)) level type-recs))
      (remove-from-packages ast type-recs)
      (reverse (translate-program ast type-recs))
      #;(order-cus (translate-program ast type-recs) type-recs)))
  
  ;compile-interactions: port location type-records level -> syntax
  (define (compile-interactions port location type-recs level)
    (to-file #f)
    (let ((ast (parse-interactions port location level)))
      (if (null? ast)
          (datum->syntax #f '(void) #f)
          (begin 
            (build-interactions-info ast level location type-recs)
            (check-interactions-types ast level location type-recs)
            (translate-interactions ast location type-recs #t)))))
      
  (define (compile-interactions-ast ast location level type-recs gen-require?)
    (to-file #f)
    (if (null? ast)
        (datum->syntax #f '(void) #f)
        (begin
          (build-interactions-info ast level location type-recs)
          (check-interactions-types ast level location type-recs)
          (translate-interactions ast location type-recs gen-require?))))
  
  (define-struct elt (prev val next) #:mutable)
  
  (define fifo
    (class* object% ()
      (define head null)
      (define tail null)
      
      (define/public (empty?)
        (and (null? head) (null? tail)))
      
      (define/public (pop)
        (let ((old-head head))
          (set! head (elt-next head))
          (when (null? head) 
            (set! tail null))
          (unless (null? head)
            (set-elt-prev! head null))
          (elt-val old-head)))
      (define/public (push e)
        (let ((new-elt (make-elt tail e null)))
          (if (empty?)
              (begin (set! head new-elt)
                     (set! tail head))
              (begin
                (set-elt-next! tail new-elt)
                (set! tail new-elt)))))
      (super-instantiate ())))
  
  (define (make-queue) (make-object fifo))
  (define (empty-queue? q) (send q empty?))
  (define (add-to-work-queue q elts) (for-each (lambda (e) (send q push e)) elts))
  (define (queue-head q) (send q pop))

  ;split-cu (list compilation-unit) (list compilation-unit) (list compilation-unit) (lis compilation-unit) type-records 
  ;          -> (values (list compilation-unit) (list compilation-unit)
  (define (split-cu cus cus-full with-depends without-depends type-recs)
    (if (null? cus)
        (values with-depends without-depends)
        (if (null? (get-local-depends (compilation-unit-depends (car cus)) cus-full type-recs))
            (split-cu (cdr cus) cus-full with-depends (cons (car cus) without-depends) type-recs)
            (split-cu (cdr cus) cus-full (cons (car cus) with-depends) without-depends type-recs))))
  
  ;ok-to-add? compilation-unit (list compilation-unit) type-records -> bool
  (define (ok-to-add? cu cus cus-full type-recs)
    (andmap (lambda (depends-on)
              (or (is-in? depends-on cu type-recs)
                  (dependency-satisfied? depends-on cus type-recs)))
            (get-local-depends (compilation-unit-depends cu) cus-full type-recs)))
  
  ;dependency-satisfied? req (list compilation-unit) type-records -> bool
  (define (dependency-satisfied? depends-on cus type-recs)
    (and (not (null? cus))
         (or (is-in? depends-on (car cus) type-recs)
             (dependency-satisfied? depends-on (cdr cus) type-recs))))
  
  ;get-local-depends: (list req) (list compilation-unit) type-records -> (list req)
  (define (get-local-depends reqs cus type-recs)
    (if (null? reqs)
        null
        (if (ormap (lambda (cu) (is-in? (car reqs) cu type-recs)) cus)
            (cons (car reqs) (get-local-depends (cdr reqs) cus type-recs))
            (get-local-depends (cdr reqs) cus type-recs))))
  
  ;is-in? req compilation-unit type-records -> bool
  (define (is-in? class cu type-recs)
    (and (member (req-class class) (compilation-unit-contains cu))
         (equal? (req-path class)
                 (begin
                   (send type-recs set-location! (list-ref (compilation-unit-locations cu)
                                                           (get-position (req-class class) (compilation-unit-contains cu) 0)))
                   (send type-recs lookup-path (req-class class) (lambda () (error 'internal-error)))))))
  
  ;get-position: 'a (list 'a) int -> int
  (define (get-position name names pos)
    (if (or (null? (cdr names))
            (equal? name (car names)))
        pos
        (get-position name (cdr names) (add1 pos))))
  
  ;order-cus (list compilation-unit) type-records -> (list compilation-unit)
  (define (order-cus cus type-recs)
    ;(printf "~a~n" cus)
    (let-values (((work-list ordered) (split-cu cus cus null null type-recs)))
      ;(printf "order-cus: ~a ~a ~a ~n" (length cus) (length work-list) (length ordered)) 
      (unless (null? work-list)
        (let ((queue (make-queue)))
          (for-each (lambda (cu) (send queue push cu)) work-list)
          (let loop ()
            ;(printf "looping in order-cus: ~a ~a ~n" (empty-queue? queue) (length ordered))
            (unless (empty-queue? queue)
              (let ((cu (send queue pop)))
                ;(printf "cu ~a~n" cu)
                (if (ok-to-add? cu ordered cus type-recs)
                    (set! ordered (cons cu ordered))
                    (send queue push cu)))
              (loop)))))
      (reverse ordered)))
  
  (define (remove-from-packages ast type-recs) 
    (packages (filter (lambda (def) (not (contained-in? def (package-defs ast))))
                      (packages))))

  (define (contained-in? def defs)
    (and (not (null? defs))
         (or (eq? def (car defs))
             (contained-in? def (cdr defs)))))
  
  (define (contains-main? members)
    (and (not (null? members))
         (or (and (method? (car members))
                  (equal? "main" (id-string (method-name (car members)))))
             (contains-main? (cdr members)))))
  
  (define (remember-main ast)
    (let ((main-class (filter (lambda (def)
                                (memq 'public 
                                      (map modifier-kind (header-modifiers (def-header def)))))
                              (filter class-def?
                                      (package-defs ast)))))
      (if (null? main-class)
          (main (list #f null))
          (main (list (contains-main? (def-members (car main-class)))
                      (id-string (header-id (def-header (car main-class)))))))))
    
  ;Extracts the version from a .zo file. Will probably blow up on anything else.
  ;get-version port -> string
  (define (get-version port)
    (if (eof-object? (peek-char port))
        ""
        (begin
          (let get-to-count ((n 0))
            (unless (= n 2)
              (read-bytes 1 port)
              (get-to-count (add1 n))))
          (let ((count (bytes-ref (read-bytes 1 port) 0)))
            (list->string (let loop ((c count))
                            (if (= c 0)
                                null
                                (cons (read-char port)
                                      (loop (sub1 c))))))))))
  )

