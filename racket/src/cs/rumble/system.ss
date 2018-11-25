
(define system-type
  (case-lambda
   [() (system-type* 'os)]
   [(mode) (if (eq? mode 'vm)
               'chez-scheme
               (system-type* mode))]))

(define unix-style-macos?
  (meta-cond
   [(getenv "PLT_CS_MAKE_UNIX_STYLE_MACOS") #t]
   [else #f]))

(define cross-mode 'infer)
(define (set-cross-mode! m) (set! cross-mode m))

(define (system-type* mode)
  (case mode
    [(vm) 'chez-scheme]
    [(os) (case (machine-type)
            [(a6osx ta6osx i3osx ti3osx)
             (if unix-style-macos? 'unix 'macosx)]
            [(a6nt ta6nt i3nt ti3nt) 'windows]
            [else 'unix])]
    [(word) (if (> (fixnum-width) 32) 64 32)]
    [(gc) 'cs]
    [(link) 'framework]
    [(machine) (get-machine-info)]
    [(so-suffix) (case (machine-type)
                   [(a6osx ta6osx i3osx ti3osx) (string->utf8 ".dylib")]
                   [(a6nt ta6nt i3nt ti3nt) (string->utf8 ".dll")]
                   [else (string->utf8 ".so")])]
    [(so-mode) 'local]
    [(fs-change) '#(#f #f #f #f)] ; when this changes, change "gen-system.rkt", too
    [(target-machine) (machine-type)]
    [(cross) cross-mode]
    [else (raise-argument-error 'system-type
                                (string-append
                                 "(or/c 'os 'word 'vm 'gc 'link 'machine 'target-machine\n"
                                 "      'so-suffix 'so-mode 'fs-change 'cross)")
                                mode)]))

(define (system-path-convention-type)
  (case (machine-type)
    [(a6nt ta6nt i3nt ti3nt) 'windows]
    [else 'unix]))

(define system-library-subpath-string
  (case (machine-type)
    [(a6nt ta6nt) "win32\\x86_64"]
    [(i3nt ti3nt) "win32\\i386"]
    [(a6osx ta6osx) (if unix-style-macos? "x86_64-darwin" "x86_64-macosx")]
    [(i3osx ti3osx) (if unix-style-macos? "i386-darwin" "i386-macosx")]
    [(a6le ta6le) "x86_64-linux"]
    [(i3le ti3le) "i386-linux"]
    [(arm32le tarm32le) "arm-linux"]
    [(ppc32le tppc32le) "ppc-linux"]
    [(i3ob ti3ob) "i386-openbsd"]
    [(a6ob ta6ob) "x86_64-openbsd"]
    [(i3ob ti3ob) "i386-openbsd"]
    [(a6fb ta6fb) "x86_64-freebsd"]
    [(i3fb ti3fb) "i386-freebsd"]
    [(a6nb ta6nb) "x86_64-netbsd"]
    [(i3nb ti3nb) "i386-netbsd"]
    [(a6s2 ta6s2) "x86_64-solaris"]
    [(i3s2 ti3s2) "i386-solaris"]
    [else "unix"]))

(define get-machine-info (lambda () "localhost info..."))
(define (set-get-machine-info! proc)
  (set! get-machine-info proc))
