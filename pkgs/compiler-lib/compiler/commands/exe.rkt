#lang racket/base
(require racket/cmdline
         raco/command-name
         compiler/private/embed
         launcher/launcher
         dynext/file
         setup/dirs
         "../private/language.rkt")

(define verbose (make-parameter #f))
(define very-verbose (make-parameter #f))

(define gui (make-parameter #f))
(define variant (make-parameter (system-type 'gc)))
(define launcher (make-parameter #f))

(define exe-output (make-parameter #f))
(define exe-embedded-flags (make-parameter '("-U" "--")))
(define exe-embedded-modules (make-parameter null))
(define exe-embedded-languages (make-parameter null))
(define exe-aux (make-parameter null))
(define exe-embedded-config-path (make-parameter "etc"))
(define exe-embedded-collects-path (make-parameter null))
(define exe-embedded-collects-dest (make-parameter #f))

(define source-file
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("-o") file "Write executable as <file>"
    (exe-output file)]
   [("--gui") "Generate GUI executable"
    (gui #t)]
   [("-l" "--launcher") "Generate a launcher"
    (when (or (find-addon-tethered-gui-bin-dir)
              (find-addon-tethered-console-bin-dir))
      ;; When an addon-executable directory is configured, treat the
      ;; addon directory more like an installation directory, instead
      ;; of a user-specific directory: record it, and remove the -U
      ;; flag (if any)
      (exe-embedded-flags
       (append
        (list "-A" (path->string (find-system-path 'addon-dir)))
        (remove "-U" (exe-embedded-flags)))))
    (launcher #t)]
   [("--embed-dlls") "On Windows, embed DLLs in the executable"
    (exe-aux (cons (cons 'embed-dlls? #t) (exe-aux)))]
   [("--config-path") path "Set <path> as configuration directory for executable"
    (exe-embedded-config-path path)]
   [("--collects-path") path "Set <path> as main collects for executable"
    (exe-embedded-collects-path path)]
   [("--collects-dest") dir "Write collection code to <dir>"
    (exe-embedded-collects-dest dir)]
   [("--ico") .ico-file "Set Windows icon for executable"
    (exe-aux (cons (cons 'ico .ico-file) (exe-aux)))]
   [("--icns") .icns-file "Set Mac OS icon for executable"
    (exe-aux (cons (cons 'icns .icns-file) (exe-aux)))]
   [("--orig-exe") "Use original executable instead of stub"
    (exe-aux (cons (cons 'original-exe? #t) (exe-aux)))]
   [("--3m") "Generate using 3m variant"
    (variant '3m)]
   [("--cgc") "Generate using CGC variant"
    (variant 'cgc)]
   [("--cs") "Generate using CS variant"
    (variant 'cs)]
   #:multi
   [("++lib") lib "Embed <lib> in executable"
    (exe-embedded-modules (append (exe-embedded-modules)
                                  (list `(#t (lib ,lib)))))]
   [("++lang") lang "Embed support for `#lang <lang>` in executable"
    (exe-embedded-languages (append (exe-embedded-languages) (list lang)))]
   [("++named-lib") prefix lib "Embed <lib> with name using <prefix>"
    (exe-embedded-modules (append (exe-embedded-modules)
                                  (list `(,(string->symbol prefix) (lib ,lib)))))]
   [("++named-file") prefix file "Embed <file> with name using <prefix>"
    (exe-embedded-modules (append (exe-embedded-modules)
                                  (list `(,(string->symbol prefix) (file ,file)))))]
   [("++aux") aux-file "Extra executable info (based on <aux-file> suffix)"
    (let ([auxes (extract-aux-from-path (path->complete-path aux-file))])
      (when (null? auxes)
        (printf " warning: no recognized information from ~s\n" aux-file))
      (exe-aux (append auxes (exe-aux))))]
   [("++exf") flag "Add flag to embed in executable"
    (exe-embedded-flags (append (exe-embedded-flags) (list flag)))]
   [("--exf") flag "Remove flag to embed in executable"
    (exe-embedded-flags (remove flag (exe-embedded-flags)))]
   [("--exf-clear") "Clear flags to embed in executable"
    (exe-embedded-flags null)]
   [("--exf-show") "Show flags to embed in executable"
    (printf "Flags to embed: ~s\n" (exe-embedded-flags))]
   #:once-each
   [("-v") "Verbose mode"
    (verbose #t)]
   [("--vv") "Very verbose mode"
    (verbose #t)
    (very-verbose #t)]
   #:args (source-file)
   source-file))

(let ([dest (mzc:embedding-executable-add-suffix
             (or (exe-output)
                 (extract-base-filename/ss source-file
                                           (string->symbol (short-program+command-name))))
             (gui))])
  (unless (file-exists? source-file)
    (raise-user-error (string->symbol (short-program+command-name))
                      "source file does not exist\n  path: ~a" source-file))
  (with-handlers ([exn:fail:filesystem? (lambda (exn) (void))])
    (call-with-input-file* dest
      (lambda (dest-in)
        (call-with-input-file* source-file
          (lambda (source-in)
            (when (equal? (port-file-identity dest-in)
                          (port-file-identity source-in))
              (raise-user-error (string->symbol (short-program+command-name))
                                (string-append
                                 "source file is the same as the destination file"
                                 "\n  source path: ~a"
                                 "\n  destination path: ~a")
                                source-file
                                dest)))))))
  (cond
   [(launcher)
    (parameterize ([current-launcher-variant (variant)])
      ((if (gui) 
           make-gracket-launcher 
           make-racket-launcher)
       (append (list "-t" (path->string (path->complete-path source-file)))
               (exe-embedded-flags))
       dest
       (exe-aux)))]
   [else
    (define main-prefix
      ;; Check for a specified prefix, but fall back to '#%mzc:
      (let loop ([mods (exe-embedded-modules)])
        (cond
          [(null? mods) '#%mzc:]
          [(equal? `(file ,source-file) (cadar mods)) (caar mods)]
          [else (loop (cdr mods))])))
    (define mod-sym (string->symbol
                     (format "~a~a"
                             main-prefix
                             ;; Get base file name:
                             (let-values ([(base name dir?)
                                           (split-path source-file)])
                               (path->bytes (path-replace-suffix name #""))))))
    (mzc:create-embedding-executable
     dest
     #:mred? (gui)
     #:variant (variant)
     #:verbose? (very-verbose)
     #:expand-namespace (make-base-namespace)
     #:modules (cons `(,main-prefix (file ,source-file) (main configure-runtime))
                     (append
                      (exe-embedded-modules)
                      (map (lambda (mod) `(#t ,mod))
                           (languages->libraries
                            (exe-embedded-languages)
                            #:who (string->symbol (short-program+command-name))))))
     #:configure-via-first-module? #t
     #:early-literal-expressions
     (parameterize ([current-namespace (make-base-namespace)])
       (define cr-sym (string->symbol (format "~a(configure-runtime)" mod-sym)))
       (list
        (compile
         `(when (module-declared? '',cr-sym)
            (dynamic-require '',cr-sym #f)))))
     #:literal-expression
     (parameterize ([current-namespace (make-base-namespace)])
       (define main-sym (string->symbol (format "~a(main)" mod-sym)))
       (compile
        `(begin
           (namespace-require '',mod-sym)
           (when (module-declared? '',main-sym)
             (dynamic-require '',main-sym #f)))))
     #:cmdline (exe-embedded-flags)
     #:collects-path (exe-embedded-collects-path)
     #:collects-dest (exe-embedded-collects-dest)
     #:aux (cons `(config-dir . ,(exe-embedded-config-path))
                 (exe-aux)))])
  (when (verbose)
    (printf " [output to \"~a\"]\n" dest)))

(module test racket/base)
