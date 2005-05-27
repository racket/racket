
(module compiler mzscheme
  (require (lib "unitsig.ss"))
  
  (require "sig.ss")

  (require (lib "compile-sig.ss" "dynext"))
  (require (lib "link-sig.ss" "dynext"))
  (require (lib "file-sig.ss" "dynext"))
  ;;
  (require (lib "compile.ss" "dynext"))
  (require (lib "link.ss" "dynext"))
  (require (lib "file.ss" "dynext"))

  (require "option.ss")

  (require "compiler-unit.ss")

  (define-values/invoke-unit/sig compiler^
    compiler@
    #f
    compiler:option^
    dynext:compile^
    dynext:link^
    dynext:file^)

  (provide-signature-elements compiler^))

