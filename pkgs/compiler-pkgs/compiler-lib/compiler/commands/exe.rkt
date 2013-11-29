#lang racket/base
(require racket/cmdline
         raco/command-name
         compiler/private/embed
         launcher/launcher
         dynext/file)

(define verbose (make-parameter #f))
(define very-verbose (make-parameter #f))

(define gui (make-parameter #f))
(define 3m (make-parameter #t))
(define launcher (make-parameter #f))

(define exe-output (make-parameter #f))
(define exe-embedded-flags (make-parameter '("-U" "--")))
(define exe-embedded-libraries (make-parameter null))
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
    (launcher #t)]
   [("--config-path") path "Set <path> as configuration directory for executable"
    (exe-embedded-config-path path)]
   [("--collects-path") path "Set <path> as main collects for executable"
    (exe-embedded-collects-path path)]
   [("--collects-dest") dir "Write collection code to <dir>"
    (exe-embedded-collects-dest dir)]
   [("--ico") .ico-file "Set Windows icon for executable"
    (exe-aux (cons (cons 'ico .ico-file) (exe-aux)))]
   [("--icns") .icns-file "Set Mac OS X icon for executable"
    (exe-aux (cons (cons 'icns .icns-file) (exe-aux)))]
   [("--orig-exe") "Use original executable instead of stub"
    (exe-aux (cons (cons 'original-exe? #t) (exe-aux)))]
   [("--3m") "Generate using 3m variant"
    (3m #t)]
   [("--cgc") "Generate using CGC variant"
    (3m #f)]
   #:multi
   [("++aux") aux-file "Extra executable info (based on <aux-file> suffix)"
    (let ([auxes (extract-aux-from-path (path->complete-path aux-file))])
      (when (null? auxes)
        (printf " warning: no recognized information from ~s\n" aux-file))
      (exe-aux (append auxes (exe-aux))))]
   [("++lib") lib "Embed <lib> in executable"
    (exe-embedded-libraries (append (exe-embedded-libraries) (list lib)))]
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
    (parameterize ([current-launcher-variant (if (3m) '3m 'cgc)])
      ((if (gui) 
           make-gracket-launcher 
           make-racket-launcher)
       (append (list "-t" (path->string (path->complete-path source-file)))
               (exe-embedded-flags))
       dest
       (exe-aux)))]
   [else
    (define mod-sym (string->symbol
                     (format "#%mzc:~a"
                             (let-values ([(base name dir?)
                                           (split-path source-file)])
                               (path->bytes (path-replace-suffix name #""))))))
    (mzc:create-embedding-executable
     dest
     #:mred? (gui)
     #:variant (if (3m) '3m 'cgc)
     #:verbose? (very-verbose)
     #:modules (cons `(#%mzc: (file ,source-file) (main configure-runtime))
                     (map (lambda (l) `(#t (lib ,l)))
                          (exe-embedded-libraries)))
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
