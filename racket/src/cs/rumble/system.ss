
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

(define-syntax (reflect-machine-type stx)
  (case (#%$target-machine)
    [(pb tpb
         pb64l tpb64l pb64b tpb64b
         pb32l tpb32l pb32b tpb32b)
     (let ([s (getenv "PLT_CS_MACHINE_TYPE")])
       (unless s (error 'machine-type "need PLT_CS_MACHINE_TYPE"))
       #`(quote #,(#%datum->syntax #'here (string->symbol s))))]
    [else #'(machine-type)]))

(define os-symbol
  (case (reflect-machine-type)
    [(a6osx ta6osx i3osx ti3osx arm64osx tarm64osx ppc32osx tppc32osx)
     (if unix-style-macos? 'unix 'macosx)]
    [(a6nt ta6nt i3nt ti3nt arm64nt tarm64nt) 'windows]
    [else 'unix]))

(define os*-symbol
  (case (reflect-machine-type)
    [(a6osx ta6osx
            i3osx ti3osx
            arm64osx tarm64osx
            ppc32osx tppc32osx)
     (if unix-style-macos?
         'darwin
         'macosx)]
    [(a6nt ta6nt i3nt ti3nt arm64nt tarm64nt) 'windows]
    [(a6le ta6le i3le ti3le
           arm32le tarm32le arm64le tarm64le
           ppc32le tppc32le
           rv64le trv64le)
     'linux]
    [(i3gnu ti3gnu)
     'gnu-hurd]
    [(a6fb ta6fb i3fb ti3fb
           arm32fb tarm32fb arm64fb tarm64fb
           ppc32fb tppc32fb)
     'freebsd]
    [(a6ob ta6ob i3ob ti3ob
           arm32ob tarm32ob arm64ob tarm64ob
           ppc32ob tppc32ob)
     'openbsd]
    [(a6nb ta6nb i3nb ti3nb
           arm32nb tarm32nb arm64nb tarm64nb
           ppc32nb tppc32nb)
     'netbsd]
    [(a6s2 ta6s2 i3s2 ti3s2) 'solaris]
    [(i3qnx) 'qnx]
    [(pb tpb
         pb64l tpb64l pb64b tpb64b
         pb32l tpb32l pb32b tpb32b)
     'unknown]
    [else (error 'system-type "internal error: unknown operating system")]))

(define arch-symbol
  (case (reflect-machine-type)
    [(a6osx ta6osx
            a6nt ta6nt
            a6le ta6le
            a6ob ta6ob
            a6nb ta6nb
            a6fb ta6fb
            a6s2 ta6s2)
     'x86_64]
    [(i3osx ti3osx
            i3nt ti3nt
            i3le ti3le
            i3ob ti3ob
            i3nb ti3nb
            i3fb ti3fb
            i3s2 ti3s2
            i3gnu ti3gnu
            i3qnx)
     'i386]
    [(arm32le tarm32le
              arm32fb tarm32fb
              arm32ob tarm32ob
              arm32nb tarm32nb)
     'arm]
    [(arm64le tarm64le
              arm64osx tarm64osx
              arm64fb tarm64fb
              arm64ob tarm64ob
              arm64nb tarm64nb
	      arm64nt tarm64nt)
     'aarch64]
    [(ppc32le tppc32le
              ppc32osx tppc32osx
              ppc32fb tppc32fb
              ppc32ob tppc32ob
              ppc32nb tppc32nb)
     'ppc]
    [(rv64le trv64le)
     'riscv64]
    [(pb tpb
         pb64l tpb64l pb64b tpb64b
         pb32l tpb32l pb32b tpb32b)
     'unknown]
    [else (error 'system-type "internal error: unknown architecture")]))

(define link-symbol
  (case (reflect-machine-type)
    [(a6osx ta6osx i3osx ti3osx arm64osx tarm64osx)
     (if unix-style-macos?
         'static
         'framework)]
    [(a6nt ta6nt i3nt ti3nt arm64nt tarm64nt) 'dll]
    [else (if unix-link-shared?
              'shared
              'static)]))

(define so-suffix-bytes
  (case (reflect-machine-type)
    [(a6osx ta6osx i3osx ti3osx arm64osx tarm64osx ppc32osx tppc32osx) (string->utf8 ".dylib")]
    [(a6nt ta6nt i3nt ti3nt arm64nt tarm64nt) (string->utf8 ".dll")]
    [else (string->utf8 ".so")]))

(define so-mode
  (case (reflect-machine-type)
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
  (case (reflect-machine-type)
    [(a6nt ta6nt i3nt ti3nt arm64nt tarm64nt) 'windows]
    [else 'unix]))

(define system-library-subpath-string
  (string-append
   (case (reflect-machine-type)
     [(a6nt ta6nt) "win32\\x86_64"]
     [(i3nt ti3nt) "win32\\i386"]
     [(arm64nt tarm64nt) "win32\\arm64"]
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
