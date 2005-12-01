(module info (lib "infotab.ss" "setup")
  (define name "SSL Driver")
  (define doc.txt "doc.txt")
  (define compile-omit-files '("mzssl.ss"))
  (define pre-install-collection "pre-installer.ss")
  (define blurb '("The SSL collection provides a driver for using the OpenSSL "
		  "secure connection library.")))
