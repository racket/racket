
(module setup-go mzscheme
  (require "setup-cmdline.ss"
	   (lib "unitsig.ss")

	    "option-sig.ss"
	    "setup-unit.ss"
	    "option-unit.ss"
	    (lib "cm.ss"))

  (define-values/invoke-unit/sig setup-option^
    setup:option@)

  (define-values (x-flags x-specific-collections x-archives)
    (parse-cmdline (current-command-line-arguments)))

  ;; Pseudo-option:
  (define (all-users on?)
    (when on?
      (current-target-plt-directory-getter
       (lambda (preferred main-collects-parent-dir choices) 
	 main-collects-parent-dir))))

  ;; Converting parse-cmdline results into parameter settings:
  (define (do-flag name param)
    (let ([a (assq name x-flags)])
      (when a
	(param (cadr a)))))
  (define-syntax all-flags
    (syntax-rules ()
      [(_ f ...) (begin
		   (do-flag 'f f)
		   ...)]))
  (all-flags clean
	     make-zo
	     call-install
	     make-launchers
	     make-so
	     verbose
	     make-verbose
	     trust-existing-zos
	     pause-on-errors
	     force-unpacks
	     all-users
	     compile-mode)

  (specific-collections x-specific-collections)
  (archives x-archives)
  (specific-planet-dirs '())

  (require (lib "launcher-sig.ss" "launcher")
	   (lib "launcher-unit.ss" "launcher")

	   (lib "dynext-sig.ss" "dynext")
	   (lib "dynext-unit.ss" "dynext"))

  (require (lib "sig.ss" "compiler")
	   (lib "option-unit.ss" "compiler")
	   (lib "compiler-unit.ss" "compiler"))

  (invoke-unit/sig
   (compound-unit/sig
    (import (SOPTION : setup-option^))
    (link [launcher : launcher^ (launcher@ dcompile dlink)]
	  [dcompile : dynext:compile^ (dynext:compile@)]
	  [dlink : dynext:link^ (dynext:link@)]
	  [dfile : dynext:file^ (dynext:file@)]
	  [option : compiler:option^ (compiler:option@)]
	  [compiler : compiler^ (compiler@
				 option
				 dcompile
				 dlink
				 dfile)]
	  [setup : () (setup@
		       SOPTION
		       compiler
		       option
		       launcher)])
    (export))
   setup-option^))
