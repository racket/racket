#lang mzscheme

(require make
         dynext/link
         dynext/compile
         dynext/file
         mzlib/file
         mzlib/list
         mzlib/etc
         launcher
         compiler/xform
         setup/dirs)

(provide pre-install)

;; Syntax used to add a command-line flag:
(define-syntax with-new-flags
  (syntax-rules ()
    [(_ ([param flags] ...) body0 body ...)
     (parameterize* ([param (append (param) flags)] ...)
       body0 body ...)]))

(define (extract-base-filename file.c)
  (let-values ([(base name dir?)
                (split-path (extract-base-filename/c file.c 'pre-install))])
    name))

(define (string-path->string s)
  (if (string? s) s (path->string s)))

(define pre-install 
  (opt-lambda (main-collects-parent-dir 
               collection-dir 
               file.c
               default-lib-dir
               include-subdirs
               find-unix-libs
               find-windows-libs
               unix-libs
               windows-libs
               extra-depends
               last-chance-k
               [3m-too? #f])
    ;; Compile and link one file:
    (define (go file.c xform-src.c)
      (pre-install/check-precompiled main-collects-parent-dir
                                     collection-dir 
                                     file.c
                                     default-lib-dir
                                     include-subdirs
                                     find-unix-libs
                                     find-windows-libs
                                     unix-libs
                                     windows-libs
                                     extra-depends
                                     last-chance-k
                                     xform-src.c))

    (define avail (available-mzscheme-variants))

    ;; Maybe do CGC mode:
    (when (or (memq 'cgc avail)
              (and (memq 'normal avail)
                   (eq? 'cgc (system-type 'gc))))
      (parameterize ([link-variant 'cgc])
        (go file.c #f)))
    ;; Maybe do 3m mode:
    (when (and 3m-too?
               (or (memq '3m avail)
                   (and (memq 'normal avail)
                        (eq? '3m (system-type 'gc)))))
      (parameterize ([link-variant '3m])
        (let ([3m-dir (build-path collection-dir 
                                  "compiled" "native" 
                                  (system-library-subpath '3m))])
	  (make-directory* 3m-dir)
          (go (build-path 3m-dir (let-values ([(base name dir?) (split-path file.c)])
                                   name))
              file.c))))))

(define (pre-install/check-precompiled
         main-collects-parent-dir collection-dir file.c . rest)
  (let* ([pre-dir (build-path collection-dir "precompiled" "native")]
         [variant-dir (system-library-subpath (link-variant))]
         [base-file (string-append (path-element->string (extract-base-filename file.c))
                                   "_ss")]
         [file.so (build-path pre-dir variant-dir (append-extension-suffix base-file))])
    (if (file-exists? file.so)
      ;; Just copy pre-compiled file:
      (let* ([dest-dir (build-path collection-dir "compiled" "native"
                                   variant-dir)]
             [dest-file.so (build-path dest-dir (append-extension-suffix base-file))])
        (make-directory* dest-dir)
        (printf "  Copying ~a\n       to ~a\n" file.so dest-file.so)
        (when (file-exists? dest-file.so) 
          (delete-file dest-file.so))
        (copy-file file.so dest-file.so))
      ;; Normal build...
      (apply pre-install/normal main-collects-parent-dir collection-dir file.c rest))))

(define (pre-install/normal main-collects-parent-dir
                            collection-dir
                            file.c
                            default-lib-dir
                            include-subdirs
                            find-unix-libs
                            find-windows-libs
                            unix-libs
                            windows-libs
                            extra-depends
                            last-chance-k
                            xform-src.c)
  (parameterize ([current-directory collection-dir])
    (define mach-id (string->symbol (path->string (system-library-subpath #f))))
    (define is-win? (eq? mach-id 'win32\\i386))

    ;; We look for libraries and includes in the 
    ;;  following places:
    (define search-path
      (append
       (let ([v (getenv "PLT_EXTENSION_LIB_PATHS")])
         (if v
           (path-list-string->path-list v (list default-lib-dir))
           (list default-lib-dir)))
       (list "/usr"
             "/usr/local"
             "/usr/local/gnu"
             ;; OS X fink location:
             "/sw"
             ;; OS X DarwinPorts location:
             "/opt/local"
             ;; Hack for NU PLT's convenience:
             "/arch/gnu/packages/readline-4.2")))

    (define sys-path
      (ormap (lambda (x)
               (and (andmap
                     (lambda (sub)
                       (directory-exists? (build-path x "include" sub)))
                     include-subdirs)
                    (andmap (lambda (lib)
                              (ormap (lambda (suffix)
                                       (file-exists? 
                                        (build-path x 
                                                    "lib"
                                                    (format "~a~a.~a" 
                                                            (if is-win? 
                                                              ""
                                                              "lib")
                                                            lib 
                                                            suffix))))
                                     '("a" "so" "dylib" "lib")))
                            (if is-win?
                              find-windows-libs
                              find-unix-libs))
                    (if (string? x)
                      (string->path x)
                      x)))
             search-path))
    
    (unless sys-path
      (error 'extension-installer
             "can't find needed include files and/or library; try setting the environment variable PLT_EXTENSION_LIB_PATHS"))
    
    (parameterize ([make-print-checking #f])
      
      ;; Used as make dependencies:
      (define mz-inc-dir (find-include-dir))
      (define headers (map (lambda (name)
                             (build-path mz-inc-dir name))
                           '("scheme.h" "schvers.h" "schemef.h" "sconfig.h" "stypes.h")))
      
      (define dir (build-path "compiled" "native" (system-library-subpath (link-variant))))
      (define base-file (string-append (path-element->string (extract-base-filename file.c))
                                       "_ss"))
      (define file.so (build-path dir (append-extension-suffix base-file)))
      (define file.o (build-path dir (append-object-suffix base-file)))
      
      (with-new-flags ([current-extension-compiler-flags
                        ((current-make-compile-include-strings)
                         (build-path sys-path "include"))]
                       [current-extension-preprocess-flags
                        ((current-make-compile-include-strings)
                         (build-path sys-path "include"))]
                       ;; Add -L and -l for Unix:
                       [current-extension-linker-flags
                        (if is-win?
                          null
                          (list (format "-L~a/lib" (path->string sys-path))))]
                       ;; Add libs for Windows:
                       [current-standard-link-libraries
                        (if is-win?
                          (append (map
                                   (lambda (l)
                                     (build-path sys-path "lib"
                                                 (format "~a.lib" l)))
                                   find-windows-libs)
                                  windows-libs)
                          null)]
                       ;; Extra stuff:
                       [current-extension-linker-flags 
                        (case mach-id [(rs6k-aix) (list "-lc")] [else null])]
                       [current-standard-link-libraries
                        (case mach-id [(i386-cygwin) (list "-lc")] [else null])])
        (define (delete/continue x)
          (with-handlers ([(lambda (x) #t) void])
            (delete-file x)))

        (make-directory* dir)

        (last-chance-k
         (lambda ()
           (make/proc
            (append
             (list (list file.so 
                         (list file.o)
                         (lambda ()
                           (link-extension
                            #f (append
                                (list file.o) 
                                (if is-win?
                                  null
                                  (map (lambda (l)
                                         (string-append
                                          "-l" (string-path->string l)))
                                       (append find-unix-libs unix-libs))))
                            file.so)))

                   (list file.o 
                         (append (list file.c)
                                 (filter (lambda (x)
                                           (regexp-match
                                            #rx#"mzdyn[a-z0-9]*[.]o" 
                                            (if (string? x)
                                              x
                                              (path->string x))))
                                         (expand-for-link-variant
                                          (current-standard-link-libraries)))
                                 headers
                                 extra-depends)
                         (lambda ()
                           (compile-extension #f file.c file.o null))))
             (if xform-src.c
               (list (list file.c
                           (append (list xform-src.c)
                                   headers
                                   extra-depends)
                           (lambda ()
                             (xform #f 
                                    (if (path? xform-src.c)
                                      (path->string xform-src.c)
                                      xform-src.c)
                                    file.c
                                    (list (let-values ([(base name dir?)
                                                        (split-path xform-src.c)])
                                            (if (path? base)
                                              base
                                              (current-directory)))
                                          mz-inc-dir)))))
               null))
            #())))))))
