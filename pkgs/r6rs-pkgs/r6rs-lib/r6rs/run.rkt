#lang scheme/base
(require scheme/cmdline
         compiler/cm
         (prefix-in r6rs: "lang/reader.rkt")
         syntax/modcode
         setup/dirs
         scheme/port
         scheme/file
         "private/readtable.rkt"
         "private/encode-name.rkt")

(define install-mode (make-parameter #f))
(define compile-mode (make-parameter #f))
(define install-all-users (make-parameter #f))
(define install-dir (make-parameter #f))
(define install-force (make-parameter #f))
(define extra-collection-dirs (make-parameter null))

(define-values (main args)
  (command-line
   #:once-any
   [("--install") "install libraries from <file>, or stdin if no <file> provided"
    (install-mode #t)]
   [("--compile") "compile <file> and all dependencies"
    (compile-mode #t)]
   #:once-any
   [("--all-users") "install into main installation"
    (install-all-users #t)]
   [("--collections") dir "install into <dir>"
    (install-dir (path->complete-path dir))]
   [("--force") "overwrite existing libraries"
    (install-force #t)]
   #:multi
   [("++path") dir "use <dir> as a container of library dirs (i.e., collections)"
    (extra-collection-dirs (append (extra-collection-dirs)
                                   (list (path->complete-path dir))))]
   #:handlers
   (case-lambda
    [(x) (values #f null)]
    [(x file . args) (values file args)])
   '("file" "arg")))

(current-command-line-arguments (apply vector-immutable args))

(current-library-collection-paths (append (extra-collection-dirs)
                                          (if (install-dir)
                                              (list (install-dir))
                                              '())
                                          (current-library-collection-paths)))

(define r6rs-read-syntax
  (case-lambda
   [() (r6rs-read-syntax (object-name (current-input-port)))]
   [(name) (r6rs-read-syntax name (current-input-port))]
   [(name port)
    (datum->syntax #f (r6rs:read-syntax name port #'r6rs 1 0 1))]))

(define (extract-libraries orig)
  (let loop ([last-pos 0])
    (let ([peeker (let-values ([(line col pos) (port-next-location orig)])
                    (let ([p (peeking-input-port orig)])
                      (port-count-lines! p)
                      (relocate-input-port p line col pos)))])
      (port-count-lines! peeker)
      (let ([lib-stx (with-r6rs-reader-parameters
                      (lambda ()
                        (read-syntax (object-name orig) peeker)))])
        (if (eof-object? lib-stx)
            null
            (let ([lib (syntax->datum lib-stx)])
              (unless (and (list? lib)
                           ((length lib) . >= . 2)
                           (eq? 'library (car lib)))
                (raise-syntax-error
                 'library
                 "not an R6RS library form"
                 lib-stx))
              (let ([name (cadr lib)])
                (unless (valid-name? name)
                  (error (format
                          "~a: invalid library name: ~e"
                          (find-system-path 'run-file)
                          name)))
                (let ([path (name->path name)])
                  (unless (install-force)
                    (when (file-exists? path)
                      (error (format "~a: file already exists: ~a for library: ~e"
                                     (find-system-path 'run-file)
                                     path
                                     name))))
                  (let ([code (open-output-bytes)])
                    (let ([pos (file-position peeker)])
                      (copy-port (make-limited-input-port orig (- pos last-pos)) code)
                      (cons (cons path (get-output-bytes code #t))
                            (loop pos))))))))))))

(define (install-libraries orig)
  (port-count-lines! orig)
  (let ([libs (extract-libraries orig)])
    (for-each (lambda (lib)
                (let ([path (car lib)]
                      [code (cdr lib)])
                  (printf " [installing ~a]\n" path)
                  (let-values ([(base name dir?) (split-path path)])
                    (make-directory* base))
                  (call-with-output-file* 
                   path
                   #:exists (if (install-force) 'truncate/replace 'error)
                   (lambda (out)
                     (display "#!r6rs\n" out)
                     (display code out)
                     (display "\n" out)))))
              libs)
    (for-each (lambda (lib)
                (compile-file (car lib)))
              libs)))
            
(define (valid-name? name)
  (and (list? name)
       (pair? name)
       (symbol? (car name))
       (let loop ([name name])
         (cond
          [(null? (cdr name))
           (or (symbol? (car name))
               (and (list? (car name))
                    (andmap exact-nonnegative-integer? (car name))))]
          [else (and (symbol? (car name))
                     (loop (cdr name)))]))))

(define (name->path name)
  (let* ([name (let ([len (length name)])
                 (cond
                  [(or (= len 1)
                       (and (= len 2)
                            (not (symbol? (cadr name)))))
                   ;; Add implicit "main":
                   (list* (car name) 'main (cdr name))]
                  [(and (or (= len 2)
                            (and (= len 3)
                                 (not (symbol? (caddr name)))))
                        (regexp-match #rx"^main_*$" (symbol->string (cadr name))))
                   ;; Rename (X main_*) => (X main__*)
                   (list* (car name)
                          (string->symbol
                           (string-append (symbol->string (cadr name)) "_"))
                          (cddr name))]
                  [else name]))])
    (apply build-path
           (cond [(install-dir)
                  => values]
                 [(install-all-users)
                  (find-collects-dir)]
                 [else
                  (find-user-collects-dir)])
           (let loop ([name name])
             (cond
              [(and (pair? (cdr name))
                    (null? (cddr name))
                    (not (symbol? (cadr name))))
               ;; versioned:
               (list
                (format "~a~a.ss"
                        (encode-name (car name))
                        (apply
                         string-append
                         (map (lambda (v)
                                (format "-~a" v))
                              (cadr name)))))]
              [(null? (cdr name))
               ;; unversioned:
               (list (format "~a.ss" (encode-name (car name))))]
              [else
               (cons (encode-name (car name))
                     (loop (cdr name)))])))))

;; ----------------------------------------

(define (compile-file src)
  (parameterize ([manager-compile-notify-handler
                  (lambda (p)
                    (printf " [Compiling ~a]\n" p))])
    (managed-compile-zo src r6rs-read-syntax)))

;; ----------------------------------------

(cond
 [(install-mode)
  (if main
      (call-with-input-file* main install-libraries)
      (install-libraries (current-input-port)))]
 [(compile-mode)
  (unless main
    (error (format "~a: need a file to compile" (find-system-path 'run-file))))
  (compile-file main)]
 [else
  (unless main
    (error (format "~a: need a file to run" (find-system-path 'run-file))))
  (let* ([main (path->complete-path main)]
         [zo (let-values ([(base name dir?) (split-path main)])
               (build-path base 
                           "compiled"
                           (path-add-suffix name #".zo")))])
    (if ((file-or-directory-modify-seconds zo #f (lambda () -inf.0))
         . > . 
         (file-or-directory-modify-seconds main #f (lambda () -inf.0)))
        ;; .zo will be used; no need to set reader:
        (dynamic-require main #f)
        ;; need to read with R6RS reader
        (let ([code (get-module-code main #:source-reader r6rs-read-syntax)]
              [rpath (module-path-index-resolve
                      (module-path-index-join main #f))])
          (parameterize ([current-module-declare-name rpath]
                         [current-module-declare-source main])
            (eval code))
          (dynamic-require rpath #f))))])
