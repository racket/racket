#lang scheme/base
(require scheme/cmdline
         raco/command-name
         compiler/private/embed
         launcher/launcher
         dynext/file)

(define verbose (make-parameter #f))
(define very-verbose (make-parameter #f))

(define gui (make-parameter #f))
(define 3m (make-parameter #t))

(define exe-output (make-parameter #f))
(define exe-embedded-flags (make-parameter '("-U" "--")))
(define exe-embedded-libraries (make-parameter null))
(define exe-aux (make-parameter null))
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
  (mzc:create-embedding-executable
   dest
   #:mred? (gui)
   #:variant (if (3m) '3m 'cgc)
   #:verbose? (very-verbose)
   #:modules (cons `(#%mzc: (file ,source-file))
                   (map (lambda (l) `(#t (lib ,l)))
                        (exe-embedded-libraries)))
   #:configure-via-first-module? #t
   #:literal-expression
   (parameterize ([current-namespace (make-base-namespace)])
     (compile
      `(namespace-require
        '',(string->symbol
            (format "#%mzc:~a"
                    (let-values ([(base name dir?)
                                  (split-path source-file)])
                      (path->bytes (path-replace-suffix name #""))))))))
   #:cmdline (exe-embedded-flags)
   #:collects-path (exe-embedded-collects-path)
   #:collects-dest (exe-embedded-collects-dest)
   #:aux (exe-aux))
  (when (verbose)
    (printf " [output to \"~a\"]\n" dest)))
