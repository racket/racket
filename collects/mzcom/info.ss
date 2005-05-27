;; info.ss for mzcom collection

(module info (lib "infotab.ss" "setup")
  (define name "MzCOM")
  (define doc.txt "doc.txt")
  (define blurb
    (list
      "MzCOM is a COM class that makes Scheme available to any COM client."))
  (define release-version "299")
  (define release-iteration "0")
  (define post-install-collection "installer.ss"))
