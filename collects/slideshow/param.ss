
(module param mzscheme
  (require (lib "unitsig.ss")
	   "sig.ss"
	   "cmdline.ss"
	   "viewer.ss")

  (provide current-slideshow-linker)

  (define current-slideshow-linker 
    (make-parameter 
     (lambda (core@)
       (compound-unit/sig
	(import)
	(link [CONFIG : cmdline^ (cmdline@)]
	      [CORE : core^ (core@ (CONFIG : config^) VIEWER)]
	      [VIEWER : viewer^ (viewer@ CONFIG CORE)])
	(export (open CORE) (unit (CONFIG : config^) config) (unit VIEWER viewer)))))))
