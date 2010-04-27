#lang scheme/base
(require scheme/runtime-path scheme/path scheme/file
         dynext/file dynext/link dynext/compile)

(define-runtime-path top-dir ".")
(define (from-top . ps) (simplify-path (apply build-path top-dir ps) #f))

(define dir->libname '(["all" "libplplot"] ["fit" "libfit"]))
(define src-dir (from-top "src"))
(define tmp-dir (from-top "src/tmp"))
(define sys-subpath (system-library-subpath #f))
(define native-dir (from-top "compiled" "native" sys-subpath))

(define (build-library lib)
  (define libname (cond [(assoc (path->string lib) dir->libname) => cadr]
                        [else (error 'plot-preinstaller
                                     "Found an unknown source directory: ~s\n"
                                     lib)]))
  (define so-name (build-path native-dir (append-extension-suffix libname)))
  (define c-files (filter (lambda (f)
                            (regexp-match? "\\.[cC]$" (path->string f)))
                          (directory-list)))
  (parameterize ([current-extension-compiler-flags
                  (append (current-extension-compiler-flags)
                          (case (system-type)
                            [(windows) '("/DHAVE_LIBPNG" "/DPLD_png")]
                            [else '("-DHAVE_LIBPNG" "-DPLD_png")]))]
                 ;; we compile a simple .so, not an extension
                 [current-standard-link-libraries '()])
    (when (or (not (file-exists? so-name))
              (let ([so-time (file-or-directory-modify-seconds so-name)])
                (for/or ([f c-files])
                  ((file-or-directory-modify-seconds f) . > . so-time))))
      (printf "plot: compiling \"~a\" -> \"~a\"...\n"
              (find-relative-path (from-top) (current-directory))
              (find-relative-path (from-top) so-name))
      (make-directory* tmp-dir)
      (for ([c-file c-files])
        (let ([o-file (append-object-suffix (path-replace-suffix c-file #""))])
          ;; first #t means quiet (here and in link-extension)
          (compile-extension #t c-file (build-path tmp-dir o-file) null)))
      (parameterize ([current-directory tmp-dir])
        (let* ([o-files (directory-list)]
               [flags (if (string=? "i386-cygwin" (path->string sys-subpath))
                        ;; DLL needs every dependence explicit:
                        '("-lc" "-lm" "-lcygwin" "-lkernel32")
                        null)])
          (link-extension #t (append o-files flags) so-name)))
      (delete-directory/files tmp-dir))))

(provide pre-installer)
(define (pre-installer main-collects-parent-dir)
  (unless (directory-exists? src-dir)
    (error 'plot-preinstall "Could not find the source directory at \"~a\""
           src-dir))
  (unless (directory-exists? native-dir) (make-directory* native-dir))
  (parameterize ([current-directory src-dir])
    (for ([path (directory-list)])
      (when (and (directory-exists? path)
                 (not (member (path->string path) '("CVS" ".svn"))))
        (parameterize ([current-directory path])
          (build-library path))))))
