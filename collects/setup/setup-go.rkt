(module setup-go mzscheme
  (require "setup-cmdline.rkt"
           mzlib/unit

           "option-sig.rkt"
           "setup-unit.rkt"
           "option-unit.rkt"
           compiler/cm)

  (define-values/invoke-unit/infer setup:option@)

  (define-values (short-name x-flags x-specific-collections x-specific-planet-packages x-archives)
    (parse-cmdline (current-command-line-arguments)))

  ;; Pseudo-option:
  (define (all-users on?)
    (when on?
      (current-target-plt-directory-getter
       (lambda (preferred main-collects-parent-dir choices) 
	 main-collects-parent-dir))))

  ;; Converting parse-cmdline results into parameter settings:
  (set-flag-params x-flags
                   ;; these are not defined in option-unit
                   all-users trust-existing-zos)
  (specific-collections x-specific-collections)
  (archives x-archives)
  (specific-planet-dirs x-specific-planet-packages)

  (setup-program-name short-name)

  (require launcher/launcher-sig
	   launcher/launcher-unit

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
