;; Starts up the compiler according to command-line flags.
;; (c) 1997-2001 PLT

;; Scheme->C compilation is the only mode really handled
;;  by the code in this collection. Other modes are handled
;;  by other collections, such as MzLib and dynext.
;; If you are interested Scheme->C part of mzc, look in
;;  "private/driver.rkt", which is the `main' file for the compiler.

;; Different compilation modes are driven by dynamically
;;  linking in appropriate libraries. This is handled
;;  by "compiler.rkt".

;; See manual for information about the Scheme-level interface
;;  provided by this collection.

#lang scheme/base

;; On error, exit with 1 status code
(error-escape-handler (lambda () (exit 1)))

(error-print-width 512)

(require (prefix-in compiler:option: "option.rkt")
         "compiler.rkt")

;; Read argv array for arguments and input file name
(require mzlib/cmdline
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
(define assume-primitives (make-parameter #t))

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

;; Returns (values mode files prefixes)
;;  where mode is 'compile, 'make-zo, etc.
(define (parse-options argv)
  (define ((add-to-param param) f v) (param (append (param) (list v))))
  (parse-command-line
   "mzc"
   argv
   `([help-labels
      "-------------------------------- mode flags ---------------------------------"]
     [once-any
      [("-k" "--make")
       ,(lambda (f) 'make-zo)
       ("Recursively compile Scheme source(s); uses/generates .dep files")]
      [("--make-collection")
       ,(lambda (f) 'collection-zos)
       ((,(format "Makes all Scheme sources in specified collection(s)") ""))]
      [("--exe")
       ,(lambda (f name) (exe-output name) 'exe)
       (,(format "Embed module in Racket to create <exe>")
        "exe")]
      [("--gui-exe")
       ,(lambda (f name) (exe-output name) 'gui-exe)
       (,(format "Embed module in GRacket to create <exe>")
        "exe")]
      [("--exe-dir")
       ,(lambda (f name) (exe-dir-output name) 'exe-dir)
       ((,(format "Combine executables with support files in <dir>") "")
        "dir")]
      [("--collection-plt")
       ,(lambda (f name) (plt-output name) 'plt-collect)
       (,(format "Create .plt <archive> containing collections")
        "archive")]
      [("--plt")
       ,(lambda (f name) (plt-output name) 'plt)
       ((,(format "Create .plt <archive> containing relative files/dirs") "")
        "archive")]
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
      [("--expand")
       ,(lambda (f) 'expand)
       ((,(format "Write macro-expanded Scheme source(s) to stdout") ""))]
      [("-r" "--decompile")
       ,(lambda (f) 'decompile)
       ((,(format "Write quasi-Scheme for ~a file(s) to stdout" (extract-suffix append-zo-suffix)) ""))]
      [("-z" "--zo")
       ,(lambda (f) 'zo)
       ((,(format "Output ~a file(s) from Scheme source(s)" (extract-suffix append-zo-suffix)) ""))]]
     [help-labels ""]
     [once-any
      [("--3m")
       ,(lambda (f) (compiler:option:3m #t))
       (,(format "Compile/link for 3m, with --exe/etc.~a"
                 (if (eq? '3m (system-type 'gc)) " [current default]" "")))]
      [("--cgc")
       ,(lambda (f) (compiler:option:3m #f))
       (,(format "Compile/link for CGC, with --exe/etc.~a"
                 (if (eq? 'cgc (system-type 'gc)) " [current default]" "")))]]
     [once-each
      [("-m" "--module")
       ,(lambda (f) (module-mode #t))
       ("Skip eval of top-level syntax, etc. for -z")]
      [("-p" "--prefix")
       ,(lambda (f v) v)
       ("Add elaboration-time prefix file for -z" "file")]
      [("-n" "--name")
       ,(lambda (f name) (compiler:option:setup-prefix name))
       ("Use <name> as extra part of public low-level names" "name")]]
     [once-any
      [("-d" "--destination")
       ,(lambda (f d)
          (unless (directory-exists? d)
            (error 'mzc "the destination directory does not exist: ~s" d))
          (dest-dir d))
       ("Output -z/-x file(s) to <dir>" "dir")]
      [("--auto-dir")
       ,(lambda (f) (auto-dest-dir #t))
       (,(format "Output -z to \"compiled\", -e to ~s"
                 (path->string (build-path "compiled" "native"
                                           (system-library-subpath #f)))))]]
     [help-labels
      "----------------------- bytecode compilation flags --------------------------"]
     [once-each
      [("--no-prim")
       ,(lambda (f) (assume-primitives #f))
       ("Do not assume `scheme' bindings at top level")]
      [("--disable-inline")
       ,(lambda (f) (disable-inlining #t))
       ("Disable procedure inlining during compilation")]]
     [help-labels
      "--------------------- executable configuration flags ------------------------"]
     [once-each
      [("--collects-path")
       ,(lambda (f i)
          (exe-embedded-collects-path i))
       ("Set <path> main collects in --[gui-]exe/--exe-dir" "path")]
      [("--collects-dest")
       ,(lambda (f i) (exe-embedded-collects-dest i))
       ("Add --[gui-]exe collection code to <dir>" "dir")]
      [("--ico")
       ,(lambda (f i) (exe-aux (cons (cons 'ico i) (exe-aux))))
       ("Windows icon for --[gui-]exe executable" ".ico-file")]
      [("--icns")
       ,(lambda (f i) (exe-aux (cons (cons 'icns i) (exe-aux))))
       ("Mac OS X icon for --[gui-]exe executable" ".icns-file")]
      [("--orig-exe")
       ,(lambda (f) (exe-aux (cons (cons 'original-exe? #t) (exe-aux))))
       ("Use original executable for --[gui-]exe instead of stub")]]
     [multi
      [("++lib")
       ,(lambda (f l)
          (exe-embedded-libraries (append (exe-embedded-libraries) (list l))))
       ("Embed <lib> in --[gui-]exe executable or --c-mods output" "lib")]
      [("++collects-copy")
       ,(lambda (f d)
          (exe-dir-add-collects-dirs (append (exe-dir-add-collects-dirs) (list d))))
       ("Add collects in <dir> to --exe-dir" "dir")]
      [("++exf")
       ,(add-to-param exe-embedded-flags)
       ("Add flag to embed in --[gui-]exe executable" "flag")]
      [("--exf")
       ,(lambda (f v) (exe-embedded-flags (remove v (exe-embedded-flags))))
       ("Remove flag to embed in --[gui-]exe executable" "flag")]
      [("--exf-clear")
       ,(lambda (f) (exe-embedded-flags null))
       ("Clear flags to embed in --[gui-]exe executable")]
      [("--exf-show")
       ,(lambda (f) (printf "Flags to embed: ~s\n" (exe-embedded-flags)))
       ("Show flag to embed in --[gui-]exe executable")]]
     [help-labels
      "----------------------------- .plt archive flags ----------------------------"]
     [once-each
      [("--plt-name")
       ,(lambda (f n) (plt-name n))
       ("Set the printed <name> describing the archive" "name")]
      [("--replace")
       ,(lambda (f) (plt-files-replace #t))
       ("Files in archive replace existing files when unpacked")]
      [("--at-plt")
       ,(lambda (f) (plt-files-plt-relative? #t))
       ("Files/dirs in archive are relative to user's add-ons directory")]]
     [once-any
      [("--all-users")
       ,(lambda (f) (plt-files-plt-home-relative? #t))
       ("Files/dirs in archive go to PLT installation if writable")]
      [("--force-all-users")
       ,(lambda (f) (plt-files-plt-home-relative? #t) (plt-force-install-dir? #t))
       ("Files/dirs forced to PLT installation")]]
     [once-each
      [("--include-compiled")
       ,(lambda (f) (plt-include-compiled #t))
       ("Include \"compiled\" subdirectories in the archive")]]
     [multi
      [("++setup")
       ,(lambda (f c)
          (plt-setup-collections (append (plt-setup-collections) (list c))))
       ("Setup <collect> after the archive is unpacked" "collect")]]
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
      "-------------------------- miscellaneous flags ------------------------------"]
     [once-each
      [("-v")
       ,(lambda (f) (compiler:option:somewhat-verbose #t))
       ("Slightly verbose mode, including version banner and output files")]
      [("--vv")
       ,(lambda (f) (compiler:option:somewhat-verbose #t) (compiler:option:verbose #t))
       ("Very verbose mode")]])
   (lambda (accum . files)
     (let ([mode (let ([l (filter symbol? accum)])
                   (if (null? l) 'make-zo (car l)))])
       (values
        mode
        files
        (let ([prefixes (filter string? accum)])
          (unless (or (memq mode '(zo)) (null? prefixes))
            (error 'mzc "prefix files are not useful in ~a mode" mode))
          (if (module-mode)
            (begin
              (unless (null? prefixes)
                (error 'mzc "prefix files not allowed with -m or --module"))
              #f)
            `(begin
               (require scheme)
               ,(if (assume-primitives)
                    '(void)
                    '(namespace-require/copy 'scheme))
               ,@(map (lambda (s) `(load ,s)) prefixes)
               (void)))))))
   (list "file/directory/collection")))

(define-values (mode source-files prefix)
  (parse-options (current-command-line-arguments)))

(when (compiler:option:somewhat-verbose)
  (printf "mzc v~a [~a], Copyright (c) 2004-2013 PLT Scheme Inc.\n"
          (version)
          (system-type 'gc)))

(when (and (auto-dest-dir) (not (memq mode '(zo compile))))
  (error 'mzc "--auto-dir works only with -z, --zo, -e, or --extension (or default mode)"))

(if (compiler:option:3m)
  (begin (link-variant '3m)  (compile-variant '3m))
  (begin (link-variant 'cgc) (compile-variant 'cgc)))

(define (compiler-warning)
  (eprintf "Warning: ~a\n         ~a\n"
           "compilation to C is usually less effective for performance"
           "than relying on the bytecode just-in-time compiler."))

(case mode
  [(zo)
   ((compile-zos prefix #:verbose? (compiler:option:somewhat-verbose))
    source-files
    (if (auto-dest-dir) 'auto (dest-dir)))]
  [(expand)
   (for ([src-file source-files])
     (let ([src-file (path->complete-path src-file)])
       (let-values ([(base name dir?) (split-path src-file)])
         (parameterize ([current-load-relative-directory base]
                        [current-namespace (make-base-namespace)]
                        [read-accept-reader #t])
           (call-with-input-file*
            src-file
            (lambda (in)
              (port-count-lines! in)
              (let loop ()
                (let ([e (read-syntax src-file in)])
                  (unless (eof-object? e)
                    (pretty-print (syntax->datum (expand e)))
                    (loop))))))))))]
  [(decompile)
   (let ([zo-parse (dynamic-require 'compiler/zo-parse 'zo-parse)]
         [decompile (dynamic-require 'compiler/decompile 'decompile)])
     (for ([zo-file source-files])
       (let ([zo-file (path->complete-path zo-file)])
         (let-values ([(base name dir?) (split-path zo-file)])
           (let ([alt-file (build-path base "compiled" (path-add-suffix name #".zo"))])
             (parameterize ([current-load-relative-directory base]
                            [print-graph #t])
               (pretty-print
                (decompile
                 (call-with-input-file*
                  (if (file-exists? alt-file) alt-file zo-file)
                  (lambda (in)
                    (zo-parse in)))))))))))]
  [(make-zo)
   (let ([n (make-base-empty-namespace)]
         [mc (dynamic-require 'compiler/cm 'managed-compile-zo)]
         [cnh (dynamic-require 'compiler/cm 'manager-compile-notify-handler)]
         [cth (dynamic-require 'compiler/cm 'manager-trace-handler)]
         [did-one? #f])
     (parameterize ([current-namespace n]
                    [cth (lambda (p)
                           (when (compiler:option:verbose)
                             (printf "  ~a\n" p)))]
                    [cnh (lambda (p)
                           (set! did-one? #t)
                           (when (compiler:option:somewhat-verbose)
                             (printf "  making ~s\n" (path->string p))))])
       (for ([file source-files])
         (unless (file-exists? file)
           (error 'mzc "file does not exist: ~a" file))
         (set! did-one? #f)
         (let ([name (extract-base-filename/ss file 'mzc)])
           (when (compiler:option:somewhat-verbose)
             (printf "\"~a\":\n" file))
           (parameterize ([compile-context-preservation-enabled
                           (disable-inlining)])
             (mc file))
           (let ([dest (append-zo-suffix
                        (let-values ([(base name dir?) (split-path file)])
                          (build-path (if (symbol? base) 'same base)
                                      "compiled" name)))])
             (when (compiler:option:somewhat-verbose)
               (printf " [~a \"~a\"]\n"
                       (if did-one? "output to" "already up-to-date at")
                       dest)))))))]
  [(collection-zos)
   (parameterize ([compile-notify-handler 
                   (lambda (path)
                     (when (compiler:option:somewhat-verbose)
                       (printf "  making ~s\n" path)))])
     (apply compile-collection-zos source-files))]
  [(cc)
   (for ([file source-files])
     (let* ([base (extract-base-filename/c file 'mzc)]
            [dest (append-object-suffix
                   (let-values ([(base name dir?) (split-path base)])
                     (build-path (or (dest-dir) 'same) name)))])
       (when (compiler:option:somewhat-verbose)
         (printf "\"~a\":\n" file))
       (compile-extension (not (compiler:option:verbose)) file dest null)
       (when (compiler:option:somewhat-verbose)
         (printf " [output to \"~a\"]\n" dest))))]
  [(ld)
   (extract-base-filename/ext (ld-output) 'mzc)
   ;; (for ([file source-files]) (extract-base-filename/o file 'mzc))
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
  [(exe gui-exe)
   (unless (= 1 (length source-files))
     (error 'mzc "expected a single module source file to embed; given: ~e"
            source-files))
   (let ([dest ((dynamic-require 'compiler/private/embed
                                 'mzc:embedding-executable-add-suffix)
                (exe-output)
                (eq? mode 'gui-exe))])
     ((dynamic-require 'compiler/private/embed
                       'mzc:create-embedding-executable)
      dest
      #:mred? (eq? mode 'gui-exe)
      #:variant (if (compiler:option:3m) '3m 'cgc)
      #:verbose? (compiler:option:verbose)
      #:modules (cons `(#%mzc: (file ,(car source-files)))
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
                                     (split-path (car source-files))])
                         (path->bytes (path-replace-suffix name #""))))))))
      #:cmdline (exe-embedded-flags)
      #:collects-path (exe-embedded-collects-path)
      #:collects-dest (exe-embedded-collects-dest)
      #:aux (exe-aux))
     (when (compiler:option:somewhat-verbose)
       (printf " [output to \"~a\"]\n" dest)))]
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
  [(exe-dir)
   ((dynamic-require 'compiler/distribute 'assemble-distribution)
    (exe-dir-output)
    source-files
    #:collects-path (exe-embedded-collects-path)
    #:copy-collects (exe-dir-add-collects-dirs))
   (when (compiler:option:somewhat-verbose)
     (printf " [output to \"~a\"]\n" (exe-dir-output)))]
  [(plt)
   (for ([fd source-files])
     (unless (relative-path? fd)
       (error 'mzc
              "file/directory is not relative to the current directory: \"~a\""
              fd)))
   (pack-plt (plt-output) (plt-name)
             source-files
             #:collections (map list (plt-setup-collections))
             #:file-mode (if (plt-files-replace) 'file-replace 'file)
             #:plt-relative? (or (plt-files-plt-relative?)
                                 (plt-files-plt-home-relative?))
             #:at-plt-home? (plt-files-plt-home-relative?)
             #:test-plt-dirs (if (or (plt-force-install-dir?)
                                     (not (plt-files-plt-home-relative?)))
                               #f
                               '("collects" "doc" "include" "lib"))
             #:requires
             ;; Get current version of mzscheme for require:
             (let* ([i (get-info '("mzscheme"))]
                    [v (and i (i 'version (lambda () #f)))])
               (list (list '("mzscheme") v))))
   (when (compiler:option:somewhat-verbose)
     (printf " [output to \"~a\"]\n" (plt-output)))]
  [(plt-collect)
   (pack-collections-plt
    (plt-output)
    (if (eq? default-plt-name (plt-name)) #f (plt-name))
    (map (lambda (sf)
           (let loop ([sf sf])
             (let ([m (regexp-match "^([^/]*)/(.*)$" sf)])
               (if m (cons (cadr m) (loop (caddr m))) (list sf)))))
         source-files)
    #:replace? (plt-files-replace)
    #:extra-setup-collections (map list (plt-setup-collections))
    #:file-filter (if (plt-include-compiled)
                    (lambda (path)
                      (or (regexp-match #rx#"compiled$" (path->bytes path))
                          (std-filter path)))
                    std-filter)
    #:at-plt-home? (plt-files-plt-home-relative?)
    #:test-plt-collects? (not (plt-force-install-dir?)))
   (when (compiler:option:somewhat-verbose)
     (printf " [output to \"~a\"]\n" (plt-output)))]
  [else (printf "bad mode: ~a\n" mode)])
