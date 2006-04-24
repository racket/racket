(module xform-mod mzscheme
  (require (lib "xform.ss" "compiler" "private")
	   (lib "cmdline.ss"))

  (define precompiling-header? (getenv "XFORM_PRECOMP"))
  (define precompiled-header (getenv "XFORM_USE_PRECOMP"))

  (define show-info? #f)
  (define output-depends-info? #f)
  (define gc-variable-stack-through-funcs? #f)

  (define palm? #f)
  (define pgc? #t)
  (define pgc-really? #t)

  (define cpp #f)
  (define file-in #f)
  (define file-out #f)

  (command-line
   "xform"
   (current-command-line-arguments)
   [once-each
    [("--setup") "ignored; for compatbility with makefile"
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
    [("--cgc") "conservative collection mode"
     (set! pgc-really? #f)]
    [("--cpp") cmdline "set CPP command line"
     (set! cpp cmdline)]
    [("-o") dest-file "name destination file"
     (set! file-out dest-file)]
    [("--indirect") "access GC_variable_stack through functions"
     (set! gc-variable-stack-through-funcs? #t)]
    [("+D") def "add CPP -D flag"
     (set! cpp (string-append cpp " -D" 
			      (regexp-replace* "[ \"]" def "'\\0'")))]]
   [args (file)
	 (set! file-in file)])

  (xform #t cpp
	 file-in
	 file-out
	 palm? pgc? pgc-really?
	 precompiling-header? precompiled-header
	 show-info? output-depends-info?
	 gc-variable-stack-through-funcs?))



