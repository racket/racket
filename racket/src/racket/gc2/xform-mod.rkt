(module xform-mod racket/base
  (require compiler/private/xform racket/cmdline)

  (define precompiling-header? (getenv "XFORM_PRECOMP"))
  (define precompiled-header (getenv "XFORM_USE_PRECOMP"))

  (define show-info? #f)
  (define output-depends-info? #f)
  (define gc-variable-stack-through-funcs? #f)

  (define def-via-include? #f)
  (define include-counter 1)

  (define palm? #f)
  (define pgc? #t)
  (define pgc-really? #t)

  (define keep-lines? #f)

  (define cpp #f)
  (define file-in #f)
  (define file-out #f)

  (command-line
   #:program "xform"
   #:argv (current-command-line-arguments)
   #:once-each
   [("--setup") dir "ignored; for compatbility with makefile"
    (void)]
   [("--precompile") "generate precompiled header; or set XFORM_PRECOMP"
    (set! precompiling-header? #t)]
   [("--precompiled") file "select precompiled header; or set XFORM_USE_PRECOMP"
    (set! precompiled-header file)]
   [("--notes") "enable notes in generated code"
    (set! show-info? #t)]
   [("--depends") "generate dependency information"
    (set! output-depends-info? #t)]
   #;
   [("--palm") "PalmOS splitting"
    (set! palm? #t)
    (set! pgc? #f)
    (set! pgc-really? #f)]
   [("--keep-lines") "keep source line information"
    (set! keep-lines? #t)]
   [("--cgc") "conservative collection mode"
    (set! pgc-really? #f)]
   [("--cpp") cmdline "set CPP command line"
    (set! cpp cmdline)]
   [("-o") dest-file "name destination file"
    (set! file-out dest-file)]
   [("--indirect") "access GC_variable_stack through functions"
    (set! gc-variable-stack-through-funcs? #t)]
   [("--D-via-include") "implement +D via -include"
    (set! def-via-include? #t)]
   #:multi
   [("+D") def "add CPP -D flag"
    (cond
     [def-via-include?
      ;; This mode is useful when command-line argument parsing
      ;; breaks down in corner cases, such as when using a Cygwin
      ;; build of gcc.
      (define fn (format "def~a.inc" include-counter))
      (set! include-counter (add1 include-counter))
      (define m (regexp-match #rx"^([^=]+)=(.*)$" def))
      (call-with-output-file fn
        (lambda (out)
          (fprintf out "#define ~a ~a\n" (cadr m) (caddr m)))
        #:exists 'truncate/replace)
      (set! cpp (string-append cpp (format " -include ~a" fn)))]
     [else
      (set! cpp (string-append cpp " -D"
                               (if (eq? (system-type) 'windows)
                                   def
                                   (regexp-replace* "[ \"]" def "'\\0'"))))])]
   #:args (file)
          (set! file-in file))

  (xform #t cpp
	 file-in
	 file-out
         keep-lines?
	 palm? pgc? pgc-really?
	 precompiling-header? precompiled-header
	 show-info? output-depends-info?
	 gc-variable-stack-through-funcs?))



