;; info.ss for mysterx collection

(module info (lib "infotab.ss" "setup")
  (define name "MysterX")
  (define doc.txt "doc.txt")
  (define help-desk-message "Mz/Mr: (require (lib \"mysterx.ss\" \"mysterx\"))")
  (define blurb
    (list
     "MysterX is an extension that lets you use Scheme to script "
     "ActiveX controls and other COM components under Windows. "
     "MysterX also has a programmable Web browser with support for "
     "Dynamic HTML."))
  (define compile-omit-files '("mxdemo.ss"))
  (define post-install-collection "installer.ss"))
