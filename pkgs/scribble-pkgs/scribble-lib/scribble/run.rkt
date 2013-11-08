#lang racket/base

(require "xref.rkt"
         "render.rkt"
         scheme/cmdline
         raco/command-name
         (prefix-in text:     "text-render.rkt")
         (prefix-in markdown: "markdown-render.rkt")
         (prefix-in html:     "html-render.rkt")
         (prefix-in latex:    "latex-render.rkt")
         (prefix-in pdf:      "pdf-render.rkt"))

(define multi-html:render-mixin
  (lambda (%) (html:render-multi-mixin (html:render-mixin %))))

(define current-render-mixin       (make-parameter html:render-mixin))
(define current-html               (make-parameter #t))
(define current-dest-directory     (make-parameter #f))
(define current-dest-name          (make-parameter #f))
(define current-info-output-file   (make-parameter #f))
(define current-info-input-files   (make-parameter null))
(define current-xref-input-modules (make-parameter null))
(define current-prefix-file        (make-parameter #f))
(define current-style-file         (make-parameter #f))
(define current-style-extra-files  (make-parameter null))
(define current-extra-files        (make-parameter null))
(define current-redirect           (make-parameter #f))
(define current-redirect-main      (make-parameter #f))
(define current-directory-depth    (make-parameter 0))
(define current-quiet              (make-parameter #f))
(define helper-file-prefix         (make-parameter #f))

(define (read-one str)
  (let ([i (open-input-string str)])
    (with-handlers ([exn:fail:read? (lambda (x) #f)])
      (let ([v (read i)])
        (and (eof-object? (read i)) v)))))

(define (run)
  (command-line
   #:program (short-program+command-name)
   #:once-any
   [("--html") "generate HTML-format output file (the default)"
    (current-html #t)
    (current-render-mixin html:render-mixin)]
   [("--htmls") "generate HTML-format output directory"
    (current-html #t)
    (current-render-mixin multi-html:render-mixin)]
   [("--html-tree") n "generate HTML-format output directories <n> deep"
    (let ([nv (string->number n)])
      (unless (exact-nonnegative-integer? nv)
        (raise-user-error 'scribble
                          "invalid depth: ~a"
                          n))
      (current-directory-depth nv)
      (current-html #t)
      (current-render-mixin (if (zero? nv)
                                html:render-mixin
                                multi-html:render-mixin)))]
   [("--latex") "generate LaTeX-format output"
    (current-html #f)
    (current-render-mixin latex:render-mixin)]
   [("--pdf") "generate PDF-format output (with PDFLaTeX)"
    (current-html #f)
    (current-render-mixin pdf:render-mixin)]
   [("--latex-section") n "generate LaTeX-format output for section depth <n>"
    (current-html #f)
    (let ([v (string->number n)])
      (unless (exact-nonnegative-integer? v)
        (raise-user-error 'scribble (format "bad section depth: ~a" n)))
      (current-render-mixin (latex:make-render-part-mixin v)))]
   [("--text") "generate text-format output"
    (current-html #f)
    (current-render-mixin text:render-mixin)]
   [("--markdown") "generate markdown-format output"
    (current-html #f)
    (current-render-mixin markdown:render-mixin)]
   #:once-each
   [("--dest") dir "write output in <dir>"
    (current-dest-directory dir)]
   [("--dest-name") name "write output as <name>"
    (current-dest-name name)]
   [("--dest-base") prefix "start support-file names with <prefix>"
    (helper-file-prefix prefix)]
   #:multi
   [("++style") file "add given .css/.tex file after others"
    (current-style-extra-files (cons file (current-style-extra-files)))]
   #:once-each
   [("--style") file "use given base .css/.tex file"
    (current-style-file file)]
   [("--prefix") file "use given .html/.tex prefix (for doctype/documentclass)"
    (current-prefix-file file)]
   #:multi
   [("++extra") file "add given file"
    (current-extra-files (cons file (current-extra-files)))]
   [("--redirect-main") url "redirect main doc links to <url>"
    (current-redirect-main url)]
   [("--redirect") url "redirect external links to tag search via <url>"
    (current-redirect url)]
   [("+m" "++main-xref-in") ("load format-speficic cross-ref info for"
                             "all installed library collections")
    (current-xref-input-modules
     (cons (cons 'setup/xref 'load-collections-xref) (current-xref-input-modules)))]
   [("++xref-in") module-path proc-id ("load format-specific cross-ref info by"
                                       "calling <proc-id> as exported by <module-path>")
    (let ([mod (read-one module-path)]
          [id (read-one proc-id)])
      (unless (module-path? mod)
        (raise-user-error
         'scribble "bad module path for ++ref-in: ~s" module-path))
      (unless (symbol? id)
        (raise-user-error
         'scribble "bad procedure identifier for ++ref-in: ~s" proc-id))
      (current-xref-input-modules
       (cons (cons mod id) (current-xref-input-modules))))]
   [("--info-out") file "write format-specific cross-ref info to <file>"
    (current-info-output-file file)]
   [("++info-in") file "load format-specific cross-ref info from <file>"
    (current-info-input-files
     (cons file (current-info-input-files)))]
   #:once-each
   [("--quiet") "suppress output-file and undefined-tag reporting"
    (current-quiet #t)]
   #:args (file . another-file)
   (let ([files (cons file another-file)])
     (build-docs (map (lambda (file) 
                        ;; Try `doc' submodule, first:
                        (if (module-declared? `(submod (file ,file) doc) #t)
                            (dynamic-require `(submod (file ,file) doc) 'doc)
                            (dynamic-require `(file ,file) 'doc)))
                      files)
                 files))))

(define (build-docs docs files)
  (when (and (current-dest-name)
             ((length files) . > . 1))
    (raise-user-error 'scribble "cannot supply a destination name with multiple inputs"))
  (render docs
          (map (lambda (fn)
                 (let-values ([(base name dir?) (split-path fn)])
                   (or (current-dest-name) name)))
               files)
          #:dest-dir (current-dest-directory)
          #:render-mixin (current-render-mixin)
          #:prefix-file (current-prefix-file)
          #:style-file (current-style-file)
          #:style-extra-files (reverse (current-style-extra-files))
          #:extra-files (reverse (current-extra-files))
          #:helper-file-prefix (helper-file-prefix)
          #:redirect (and (current-html) (current-redirect))
          #:redirect-main (and (current-html) (current-redirect-main))
          #:directory-depth (current-directory-depth)
          #:quiet? (current-quiet)
          #:info-in-files (reverse (current-info-input-files))
          #:xrefs (for/list ([mod+id (in-list (reverse (current-xref-input-modules)))])
                    (let* ([get-xref (dynamic-require (car mod+id) (cdr mod+id))]
                           [xr (get-xref)])
                      (unless (xref? xr)
                        (raise-user-error
                         'scribble "result from `~s' of `~s' is not an xref: ~e"
                         (cdr mod+id) (car mod+id) xr))
                      xr))
          #:info-out-file (current-info-output-file)))

(run)
