#lang scheme/base

;; On error, exit with 1 status code
(error-escape-handler (lambda () (exit 1)))

(error-print-width 512)

(require (prefix-in compiler:option: "../option.ss")
         "../compiler.ss"
         rico/command-name
         mzlib/cmdline
         dynext/file
         dynext/compile
         dynext/link
         scheme/pretty
         setup/pack
         setup/getinfo
         setup/dirs)

(define dest-dir (make-parameter #f))
(define auto-dest-dir (make-parameter #f))

(define ld-output (make-parameter #f))

(define exe-output (make-parameter #f))
(define exe-embedded-flags (make-parameter '("-U" "--")))
(define exe-embedded-libraries (make-parameter null))
(define exe-aux (make-parameter null))
(define exe-embedded-collects-path (make-parameter #f))
(define exe-embedded-collects-dest (make-parameter #f))
(define exe-dir-add-collects-dirs (make-parameter null))

(define exe-dir-output (make-parameter #f))

(define mods-output (make-parameter #f))

(define module-mode (make-parameter #f))

(define default-plt-name "archive")

(define disable-inlining (make-parameter #f))

(define plt-output (make-parameter #f))
(define plt-name (make-parameter default-plt-name))
(define plt-files-replace (make-parameter #f))
(define plt-files-plt-relative? (make-parameter #f))
(define plt-files-plt-home-relative? (make-parameter #f))
(define plt-force-install-dir? (make-parameter #f))
(define plt-setup-collections (make-parameter null))
(define plt-include-compiled (make-parameter #f))

(define stop-at-source (make-parameter #f))

(define (extract-suffix appender)
  (bytes->string/latin-1
   (subbytes (path->bytes (appender (bytes->path #"x"))) 1)))

(define ((add-to-param param) f v) (param (append (param) (list v))))

(define mzc-symbol (string->symbol (short-program+command-name)))

;; Returns (values mode files prefixes)
;;  where mode is 'compile, 'make-zo, etc.
(define-values (mode source-files prefix)
  (parse-command-line
   (short-program+command-name)
   (current-command-line-arguments)
   `([help-labels
      "-------------------------------- mode flags ---------------------------------"]
     [once-any
      [("--cc")
       ,(lambda (f) 'cc)
       (,(format "Compile arbitrary file(s) for an extension: ~a -> ~a"
                 (extract-suffix append-c-suffix)
                 (extract-suffix append-object-suffix)))]
      [("--ld")
       ,(lambda (f name) (ld-output name) 'ld)
       (,(format "Link arbitrary file(s) to create <extension>: ~a -> ~a"
                 (extract-suffix append-object-suffix)
                 (extract-suffix append-extension-suffix))
        "extension")]
      [("-x" "--xform")
       ,(lambda (f) 'xform)
       ((,(format "Convert for 3m compilation: ~a -> ~a"
                  (extract-suffix append-c-suffix)
                  (extract-suffix append-c-suffix))
         ""))]
      [("--c-mods")
       ,(lambda (f name) (mods-output name) 'c-mods)
       ((,(format "Write C-embeddable module bytecode to <file>") "")
        "file")]
      [("-e" "--extension")
       ,(lambda (f) 'compile)
       (,(format "Output ~a file(s) from Scheme source(s)" (extract-suffix append-extension-suffix)))]
      [("-c" "--c-source")
       ,(lambda (f) 'compile-c)
       (,(format "Output ~a file(s) from Scheme source(s)" (extract-suffix append-c-suffix)))]]
     [help-labels ""]
     [once-any
      [("--3m")
       ,(lambda (f) (compiler:option:3m #t))
       (,(format "Compile/link for 3m~a"
                 (if (eq? '3m (system-type 'gc)) " [current default]" "")))]
      [("--cgc")
       ,(lambda (f) (compiler:option:3m #f))
       (,(format "Compile/link for CGC~a"
                 (if (eq? 'cgc (system-type 'gc)) " [current default]" "")))]]
     [once-each
      [("-m" "--module")
       ,(lambda (f) (module-mode #t))
       ("Skip eval of top-level syntax, etc. for -e/-c")]
      [("--embedded")
       ,(lambda (f) (compiler:option:compile-for-embedded #t))
       ("Compile for embedded run-time engine, with -c")]
      [("-p" "--prefix")
       ,(lambda (f v) v)
       ("Add elaboration-time prefix file for -e/-c/-z" "file")]
      [("-n" "--name")
       ,(lambda (f name) (compiler:option:setup-prefix name))
       ("Use <name> as extra part of public low-level names" "name")]]
     [once-any
      [("-d" "--destination")
       ,(lambda (f d)
          (unless (directory-exists? d)
            (error mzc-symbol "the destination directory does not exist: ~s" d))
          (dest-dir d))
       ("Output -e/-c/-x file(s) to <dir>" "dir")]
      [("--auto-dir")
       ,(lambda (f) (auto-dest-dir #t))
       (,(format "Output -e to ~s"
                 (path->string (build-path "compiled" "native"
                                           (system-library-subpath #f)))))]]
     [help-labels
      "------------------- compiler/linker configuration flags ---------------------"]
     [once-each
      [("--tool")
       ,(lambda (f v)
          (let ([v (string->symbol v)])
            (use-standard-compiler v)
            (use-standard-linker v)))
       (,(format "Use pre-defined <tool> as C compiler/linker:~a"
                 (apply string-append
                        (apply append (map (lambda (t)
                                             (list " " (symbol->string t)))
                                           (get-standard-compilers)))))
        "tool")]
      [("--compiler")
       ,(lambda (f v) (current-extension-compiler v))
       ("Use <compiler-path> as C compiler" "compiler-path")]]
     [multi
      [("++ccf")
       ,(add-to-param current-extension-compiler-flags)
       ("Add C compiler flag" "flag")]
      [("--ccf")
       ,(lambda (f v)
          (current-extension-compiler-flags
           (remove v (current-extension-compiler-flags))))
       ("Remove C compiler flag" "flag")]
      [("--ccf-clear")
       ,(lambda (f) (current-extension-compiler-flags null))
       ("Clear C compiler flags")]
      [("--ccf-show")
       ,(lambda (f)
          (printf "C compiler flags: ~s\n"
                  (expand-for-link-variant (current-extension-compiler-flags))))
       ("Show C compiler flags")]]
     [once-each
      [("--linker")
       ,(lambda (f v) (current-extension-linker v))
       ("Use <linker-path> as C linker" "linker-path")]]
     [multi
      [("++ldf")
       ,(add-to-param current-extension-linker-flags)
       ("Add C linker flag" "flag")]
      [("--ldf")
       ,(lambda (f v)
          (current-extension-linker-flags
           (remove v (current-extension-linker-flags))))
       ("Remove C linker flag" "flag")]
      [("--ldf-clear")
       ,(lambda (f) (current-extension-linker-flags null))
       ("Clear C linker flags")]
      [("--ldf-show")
       ,(lambda (f)
          (printf "C linker flags: ~s\n"
                  (expand-for-link-variant (current-extension-linker-flags))))
       ("Show C linker flags")]
      [("++ldl")
       ,(add-to-param current-standard-link-libraries)
       ("Add C linker library" "lib")]
      [("--ldl-show")
       ,(lambda (f)
          (printf "C linker libraries: ~s\n"
                  (expand-for-link-variant (current-standard-link-libraries))))
       ("Show C linker libraries")]]
     [multi
      [("++cppf")
       ,(add-to-param current-extension-preprocess-flags)
       ("Add C preprocess (xform) flag" "flag")]
      [("--cppf")
       ,(lambda (f v)
          (current-extension-preprocess-flags
           (remove v (current-extension-preprocess-flags))))
       ("Remove C preprocess (xform) flag" "flag")]
      [("--cppf-clear")
       ,(lambda (f) (current-extension-preprocess-flags null))
       ("Clear C preprocess (xform) flags")]
      [("--cppf-show")
       ,(lambda (f)
          (printf "C compiler flags: ~s\n"
                  (expand-for-link-variant (current-extension-preprocess-flags))))
       ("Show C preprocess (xform) flags")]]
     [help-labels
      "-------------------- -c/-e compiler optimization flags ----------------------"]
     [once-each
      [("--no-prop")
       ,(lambda (f) (compiler:option:propagate-constants #f))
       ("Don't propagate constants")]
      [("--inline")
       ,(lambda (f d)
          (compiler:option:max-inline-size
           (with-handlers ([void (lambda (x)
                                   (error mzc-symbol "bad size for --inline: ~a" d))])
             (let ([v (string->number d)])
               (unless (and (not (negative? v)) (exact? v) (real? v))
                 (error 'bad))
               v))))
       ("Set the maximum inlining size" "size")]
      [("--no-prim")
       ,(lambda (f) (compiler:option:assume-primitives #f))
       ("Do not assume `scheme' bindings at top level")]
      [("--stupid")
       ,(lambda (f) (compiler:option:stupid #t))
       ("Compile despite obvious non-syntactic errors")]
      [("--unsafe-disable-interrupts")
       ,(lambda (f) (compiler:option:disable-interrupts #t))
       ("Ignore threads, breaks, and stack overflow")]
      [("--unsafe-skip-tests")
       ,(lambda (f) (compiler:option:unsafe #t))
       ("Skip run-time tests for some primitive operations")]
      [("--unsafe-fixnum-arithmetic")
       ,(lambda (f) (compiler:option:fixnum-arithmetic #t))
       ("Assume fixnum arithmetic yields a fixnum")]]
     [help-labels
      "-------------------------- miscellaneous flags ------------------------------"]
     [once-each
      [("-v")
       ,(lambda (f) (compiler:option:somewhat-verbose #t))
       ("Slightly verbose mode, including version banner and output files")]
      [("--vv")
       ,(lambda (f) (compiler:option:somewhat-verbose #t) (compiler:option:verbose #t))
       ("Very verbose mode")]
      [("--save-temps")
       ,(lambda (f) (compiler:option:clean-intermediate-files #f))
       ("Keep intermediate files")]
      [("--debug")
       ,(lambda (f) (compiler:option:debug #t))
       ("Write debugging output to dump.txt")]])
   (lambda (accum . files)
     (let ([mode (let ([l (filter symbol? accum)])
                   (if (null? l) 
                       (error mzc-symbol "no mode flag specified")
                       (car l)))])
       (values
        mode
        files
        (let ([prefixes (filter string? accum)])
          (unless (or (memq mode '(compile compile-c)) (null? prefixes))
            (error mzc-symbol "prefix files are not useful in ~a mode" mode))
          (if (module-mode)
            (begin
              (unless (compiler:option:assume-primitives)
                (error mzc-symbol "--no-prim is not useful with -m or --module"))
              (unless (null? prefixes)
                (error mzc-symbol "prefix files not allowed with -m or --module"))
              #f)
            `(begin
               (require scheme)
               ,(if (compiler:option:assume-primitives)
                    '(void)
                    '(namespace-require/copy 'scheme))
               (require compiler/cffi)
               ,@(map (lambda (s) `(load ,s)) prefixes)
               (void)))))))
   (list "file")))

(when (compiler:option:somewhat-verbose)
  (printf "~a v~a [~a], Copyright (c) 2004-2010 PLT Scheme Inc.\n"
          (short-program+command-name)
          (version)
          (system-type 'gc)))

(when (and (auto-dest-dir) (not (memq mode '(zo compile))))
  (error mzc-symbol "--auto-dir works only with -z, --zo, -e, or --extension (or default mode)"))

(define (never-embedded action)
  (when (compiler:option:compile-for-embedded)
    (error mzc-symbol "cannot ~a an extension for an embedded MzScheme" action)))

(if (compiler:option:3m)
  (begin (link-variant '3m)  (compile-variant '3m))
  (begin (link-variant 'cgc) (compile-variant 'cgc)))

(define (compiler-warning)
  (fprintf (current-error-port)
           "Warning: ~a\n         ~a\n"
           "compilation to C is usually less effective for performance"
           "than relying on the bytecode just-in-time compiler."))

(case mode
  [(compile)
   (compiler-warning)
   (never-embedded "compile")
   ((compile-extensions prefix)
    source-files
    (if (auto-dest-dir) 'auto (dest-dir)))]
  [(compile-c)
   ((compile-extensions-to-c prefix) source-files (dest-dir))]
  [(cc)
   (for ([file source-files])
     (let* ([base (extract-base-filename/c file mzc-symbol)]
            [dest (append-object-suffix
                   (let-values ([(base name dir?) (split-path base)])
                     (build-path (or (dest-dir) 'same) name)))])
       (when (compiler:option:somewhat-verbose)
         (printf "\"~a\":\n" file))
       (compile-extension (not (compiler:option:verbose)) file dest null)
       (when (compiler:option:somewhat-verbose)
         (printf " [output to \"~a\"]\n" dest))))]
  [(ld)
   (extract-base-filename/ext (ld-output) mzc-symbol)
   ;; (for ([file source-files]) (extract-base-filename/o file mzc-symbol))
   (let ([dest (if (dest-dir)
                 (build-path (dest-dir) (ld-output))
                 (ld-output))])
     (when (compiler:option:somewhat-verbose)
       (printf "~a:\n" (let ([s (apply string-append
                                       (map (lambda (n) (format " \"~a\"" n))
                                            source-files))])
                         (substring s 1 (string-length s)))))
     (link-extension (not (compiler:option:verbose))
                     source-files
                     dest)
     (when (compiler:option:somewhat-verbose)
       (printf " [output to \"~a\"]\n" dest)))]
  [(xform)
   (for ([file source-files])
     (let* ([out-file (path-replace-suffix file ".3m.c")]
            [out-file  (if (dest-dir)
                         (build-path (dest-dir) out-file)
                         out-file)])
       ((dynamic-require 'compiler/xform 'xform)
        (not (compiler:option:verbose))
        file
        out-file
        (list (find-include-dir)))
       (when (compiler:option:somewhat-verbose)
         (printf " [output to \"~a\"]\n" out-file))))]
  [(c-mods)
   (let ([dest (mods-output)])
     (let-values ([(in out) (make-pipe)])
       (parameterize ([current-output-port out])
         ((dynamic-require 'compiler/embed 'write-module-bundle)
          #:modules
          (append (map (lambda (l) `(#f (file ,l))) source-files)
             (map (lambda (l) `(#t (lib ,l))) (exe-embedded-libraries)))))
       (close-output-port out)
       (let ([out (open-output-file dest #:exists 'truncate/replace)])
         (fprintf out "#ifdef MZ_XFORM\n")
         (fprintf out "XFORM_START_SKIP;\n")
         (fprintf out "#endif\n")
         (fprintf out "static void declare_modules(Scheme_Env *env) {\n")
         (fprintf out "  static unsigned char data[] = {")
         (let loop ([pos 0])
           (let ([b (read-byte in)])
             (when (zero? (modulo pos 20)) (fprintf out "\n    "))
             (unless (eof-object? b) (fprintf out "~a," b) (loop (add1 pos)))))
         (fprintf out "0\n  };\n")
         (fprintf out "  Scheme_Object *eload = NULL, *a[3] = {NULL, NULL, NULL};\n")
         (fprintf out "  MZ_GC_DECL_REG(4);\n")
         (fprintf out "  MZ_GC_VAR_IN_REG(0, eload);\n")
         (fprintf out "  MZ_GC_ARRAY_VAR_IN_REG(1, a, 3);\n")
         (fprintf out "  MZ_GC_REG();\n")
         (fprintf out "  eload = scheme_builtin_value(\"embedded-load\");\n")
         (fprintf out "  a[0] = scheme_false;\n")
         (fprintf out "  a[1] = scheme_false;\n")
         (fprintf out "  a[2] = scheme_make_sized_byte_string((char *)data, ~a, 0);\n"
                  (file-position in))
         (fprintf out "  scheme_apply(eload, 3, a);\n")
         (fprintf out "  MZ_GC_UNREG();\n")
         (fprintf out "}\n")
         (fprintf out "#ifdef MZ_XFORM\n")
         (fprintf out "XFORM_END_SKIP;\n")
         (fprintf out "#endif\n")
         (close-output-port out)))
     (when (compiler:option:somewhat-verbose)
       (printf " [output to \"~a\"]\n" dest)))]
  [else (printf "bad mode: ~a\n" mode)])
