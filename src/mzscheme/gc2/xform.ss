;; This program reads MzScheme/MrEd C/C++ source and transforms it
;; to work with precise garbage collection or(!) PalmOS. The source
;; is C-pre-processed first, then run though a `lex'-like lexer,
;; ctok.ss.
;;
;; It probably won't work for other C/C++ code, because it
;; doesn't bother *parsing* the source. Instead, it relies on
;; various heuristics that work for MzScheme/MrEd code.
;;
;; There are also some input hacks, such as START_XFORM_SKIP.
;; 
;; Notable assumptions:
;;  No calls of the form (f)(...).
;;  For arrays, records, and non-pointers, pass by address only.
;;  No gc-triggering code in .h files.
;;  No instance vars declared as function pointers without a typedef
;;    for the func ptr type.
;;
;; BUGS: Doesn't check for pointer comparisons where one of the
;;       comparees is a function call. This doesn't happen in
;;       MzScheme/MrEd (or, because of this bug, shouldn't!).
;;
;;       Passing the address of a pointer is dangerous; make sure
;;       that the pointer is used afterward, otherwise it pointer
;;       might not get updated during GC.
;;
;;       A "return;" can get converted to "{ <something>; return; };",
;;       which can break "if (...) return; else ...".

;; To call for Precise GC:
;;   mzscheme -qr xform.ss [--setup] [--precompile] [--precompiled <file>] [--notes] [--depends] [--cgc] <cpp> <src> <dest>
;;
;;   Or: Set the XFORM_PRECOMP=yes environment variable to imply --precompile
;;       Set the XFORM_USE_PRECOMP=<file> to imply --precompiled <file>
;;
;; To call for Palm:
;;   mzscheme -qr xform.ss [--setup] [--notes] [--depends] --palm <cpp> <src> <dest> <mapdest>

;; General code conventions:
;;   e means a list of tokens, often ending in a '|;| token
;;   -e means a reversed list of tokens

(if (string=? "--setup"
	      (vector-ref (current-command-line-arguments) 0))

    ;; Setup an xform-collects tree for running xform.
    ;; Delete existing xform-collects tree if it's for an old version
    (begin
      (unless (and (file-exists? "xform-collects/version.ss")
		   (equal? (version)
			   (with-input-from-file "xform-collects/version.ss" read))
		   (>= (file-or-directory-modify-seconds (build-path "xform-collects/xform/xform-mod.ss"))
		       (file-or-directory-modify-seconds (build-path (current-load-relative-directory) "xform-mod.ss"))))
		      
	(load-relative "setup.ss"))
      
      (current-library-collection-paths (list (build-path (current-directory) "xform-collects")))
      
      (error-print-width 100)
      
      (dynamic-require '(lib "xform-mod.ss" "xform") #f))

    ;; Otherwise, we assume that it's ok to use the collects
    (dynamic-require (build-path (current-load-relative-directory)
				 "xform-mod.ss")
		     #f))

