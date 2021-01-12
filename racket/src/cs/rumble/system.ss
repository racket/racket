
(define unix-style-macos?
  (meta-cond
   [(getenv "PLT_CS_MAKE_UNIX_STYLE_MACOS") #t]
   [else #f]))

(define unix-link-shared?
  (meta-cond
   [(getenv "PLT_CS_MAKE_LINK_SHARED") #t]
   [else #f]))

(define cross-mode 'infer)
(define (set-cross-mode! m) (set! cross-mode m))

(define fs-change-properties '#(#f #f #f #f))
(define (set-fs-change-properties! vec) (set! fs-change-properties vec))

;; Definitons like `os-symbol` are also parsed by "../c/gen-system.rkt"

(define os-symbol
  (case (machine-type)
    [(a6osx ta6osx i3osx ti3osx arm64osx tarm64osx ppc32osx tppc32osx)
     (if unix-style-macos? 'unix 'macosx)]
    [(a6nt ta6nt i3nt ti3nt) 'windows]
    [else 'unix]))

(define os*-symbol
  (case (machine-type)
    [(a6osx ta6osx
            i3osx ti3osx
            arm64osx tarm64osx
            ppc32osx tppc32osx)
     (if unix-style-macos?
         'darwin
         'macosx)]
    [(a6nt ta6nt i3nt ti3nt) 'windows]
    [(a6le ta6le i3le ti3le
           arm32le tarm32le arm64le tarm64le
           ppc32le tppc32le)
     'linux]
    [(a6ob ta6ob i3ob ti3ob) 'openbsd]
    [(a6fb ta6fb i3fb ti3fb) 'freebsd]
    [(a6nb ta6nb i3nb ti3nb) 'netbsd]
    [(a6s2 ta6s2 i3s2 ti3s2) 'solaris]
    [(i3qnx) 'qnx]
    [else (error 'system-type "internal error: unknown operating system")]))

(define arch-symbol
  (case (machine-type)
    [(a6osx ta6osx
            a6nt ta6nt
            a6le ta6le
            a6ob ta6ob
            a6fb ta6fb
            a6s2 ta6s2)
     'x86_64]
    [(i3osx ti3osx
            i3nt ti3nt
            i3le ti3le
            i3ob ti3ob
            i3fb ti3fb
            i3s2 ti3s2
            i3qnx)
     'i386]
    [(arm32le tarm32le) 'arm]
    [(arm64le tarm64le arm64osx tarm64osx) 'aarch64]
    [(ppc32le tppc32le
              ppc32osx tppc32osx)
     'ppc]
    [else (error 'system-type "internal error: unknown architecture")]))

(define link-symbol
  (case (machine-type)
    [(a6osx ta6osx i3osx ti3osx arm64osx tarm64osx)
     (if unix-style-macos?
         'static
         'framework)]
    [(a6nt ta6nt i3nt ti3nt) 'windows]
    [else (if unix-link-shared?
              'shared
              'static)]))

(define so-suffix-bytes
  (case (machine-type)
    [(a6osx ta6osx i3osx ti3osx arm64osx tarm64osx ppc32osx tppc32osx) (string->utf8 ".dylib")]
    [(a6nt ta6nt i3nt ti3nt) (string->utf8 ".dll")]
    [else (string->utf8 ".so")]))

(define so-mode
  (case (machine-type)
    [(arm64osx tarm64osx) 'global]
    [else 'local]))

;; Force inline of some common cases, so optimization can use
;; the resulting constant:
(define-syntax system-type
  (lambda (stx)
    (syntax-case stx (quote)
      [(_ 'key) (case (#%syntax->datum #'key)
                  [(vm) #''chez-scheme]
                  [(os) #'os-symbol]
                  [(os*) #'os*-symbol]
                  [(arch) #'arch-symbol]
                  [(word) #'(if (> (fixnum-width) 32) 64 32)]
                  [(gc) #''cs]
                  [else #'(system-type* 'key)])]
      [(_ arg ...) #'(system-type* arg ...)]
      [_ #'system-type*])))

(define system-type*
  (|#%name|
   system-type
   (case-lambda
    [() (system-type* 'os)]
    [(mode)
     (case mode
       [(vm) 'chez-scheme]
       [(os) os-symbol]
       [(os*) os*-symbol]
       [(arch) arch-symbol]
       [(word) (if (> (fixnum-width) 32) 64 32)]
       [(gc) 'cs]
       [(link) link-symbol]
       [(machine) (get-machine-info)]
       [(so-suffix) so-suffix-bytes]
       [(so-mode) so-mode]
       [(fs-change) fs-change-properties]
       [(target-machine) (machine-type)]
       [(cross) cross-mode]
       [else (raise-argument-error 'system-type
                                   (string-append
                                    "(or/c 'os 'os* 'arch 'word 'vm 'gc 'link 'machine 'target-machine\n"
                                    "      'so-suffix 'so-mode 'fs-change 'cross)")
                                   mode)])])))

(define (system-path-convention-type)
  (case (machine-type)
    [(a6nt ta6nt i3nt ti3nt) 'windows]
    [else 'unix]))

(define system-library-subpath-string
  (string-append
   (case (machine-type)
     [(a6nt ta6nt) "win32\\x86_64"]
     [(i3nt ti3nt) "win32\\i386"]
     [else (string-append (symbol->string arch-symbol)
                          "-"
                          (symbol->string os*-symbol))])
   (let-syntax ([suffix
                 (lambda (stx)
                   (or (getenv "PLT_CS_SLSP_SUFFIX")
                       ""))])
     (suffix))))

(define get-machine-info (lambda () "localhost info..."))
(define (set-get-machine-info! proc)
  (set! get-machine-info proc))
