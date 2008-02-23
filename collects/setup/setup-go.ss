
(module setup-go mzscheme
  (require "setup-cmdline.ss"
	   mzlib/unit

	    "option-sig.ss"
	    "setup-unit.ss"
	    "option-unit.ss"
	    mzlib/cm)

  (define-values/invoke-unit/infer setup:option@)

  (define-values (x-flags x-specific-collections x-specific-planet-packages x-archives)
    (parse-cmdline (current-command-line-arguments)))

  ;; Pseudo-option:
  (define (all-users on?)
    (when on?
      (current-target-plt-directory-getter
       (lambda (preferred main-collects-parent-dir choices) 
	 main-collects-parent-dir))))

  ;; Converting parse-cmdline results into parameter settings:
  (define (do-flag name param)
    (cond [(assq name x-flags) => (lambda (a) (param (cadr a)))]))
  (define-syntax all-flags
    (syntax-rules () [(_ f ...) (begin (do-flag 'f f) ...)]))
  (all-flags clean
	     make-zo
	     call-install
	     call-post-install
	     make-launchers
	     verbose
	     make-verbose
	     trust-existing-zos
	     pause-on-errors
	     force-unpacks
	     all-users
	     compile-mode
             make-docs
             make-user
             make-planet
             doc-pdf-dest)

  (specific-collections x-specific-collections)
  (archives x-archives)
  (specific-planet-dirs x-specific-planet-packages)

  (require (lib "launcher-sig.ss" "launcher")
	   (lib "launcher-unit.ss" "launcher")

	   dynext/dynext-sig
	   dynext/dynext-unit)

  (require compiler/sig
	   compiler/option-unit
	   compiler/compiler-unit)

  (invoke-unit
   (compound-unit/infer
    (import (SOPTION : setup-option^))
    (export)
    (link launcher@ dynext:compile@ dynext:link@ dynext:file@
          compiler:option@ compiler@ setup@))
   (import setup-option^)))
