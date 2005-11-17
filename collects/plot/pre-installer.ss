(module pre-installer mzscheme
  (require (lib "etc.ss") (lib "file.ss") (lib "list.ss")
           (lib "file.ss" "dynext") (lib "link.ss" "dynext")
           (lib "compile.ss" "dynext")
           (lib "compiler.ss" "compiler"))

  (define top-dir (this-expression-source-directory))
  (define src-dir (build-path top-dir "src"))
  (define tmp-dir (build-path src-dir "tmp"))
  (define dir->libname '(["all" "libplplot"] ["fit" "libfit"]))
  (define native-dir
    (build-path top-dir "compiled" "native" (system-library-subpath)))

  (define (build-library lib)
    (when (and (directory-exists? lib)
               (not (member (path->string lib) '("CVS" ".svn"))))
      (let* ([libname (cond
                       [(assoc (path->string lib) dir->libname) => cadr]
                       [else (error 'plot-preinstaller
                                    "Found an unknown source directory: ~s\n"
                                    lib)])]
             [so-name (build-path top-dir "compiled" "native"
                                  (system-library-subpath)
                                  (append-extension-suffix libname))])
        (parameterize ([current-directory lib]
                       [current-extension-compiler-flags
                        (append (current-extension-compiler-flags)
                                (case (system-type)
                                  [(windows) '("/DHAVE_LIBPNG" "/DPLD_png")]
                                  [else '("-DHAVE_LIBPNG" "-DPLD_png")]))]
                       ;; we compile a simple .so, not an extension
                       [current-standard-link-libraries '()]
                       ;[current-use-mzdyn #f]
                       )
          (define c-files (filter (lambda (f)
                                    (regexp-match "\\.[cC]$" (path->string f)))
                                  (directory-list)))
          (when (or (not (file-exists? so-name))
                    (let ([so-time (file-or-directory-modify-seconds so-name)])
                      (ormap (lambda (f)
                               (> (file-or-directory-modify-seconds f)
                                  so-time))
                             c-files)))
            (printf "plot: compiling \"~a\" -> \"~a\"...\n" lib so-name)
            (make-directory* tmp-dir)
            (compile-c-extension-parts c-files tmp-dir)
            (parameterize ([current-directory tmp-dir])
              (link-extension #f (append 
				  (directory-list tmp-dir) 
				  (if (string=? "i386-cygwin"
						(path->string (system-library-subpath #f)))
				      ;; DLL needs every dependence explicit:
				      '("-lc" "-lm" "-lcygwin" "-lkernel32")
				      null))
			      so-name))
            (delete-directory/files tmp-dir))))))

  (provide pre-installer)
  (define (pre-installer plthome)
    (unless (directory-exists? src-dir)
      (error 'plot-preinstall "Could not find the source directory at ~a"
             src-dir))
    (when (directory-exists? src-dir)
      (unless (directory-exists? native-dir) (make-directory* native-dir))
      (parameterize ([current-directory src-dir])
        (for-each build-library (directory-list))))))
