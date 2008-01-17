; help collection
(module info setup/infotab
  (define name "Help")
  ;(define doc.txt "doc.txt")
  (define compile-subcollections
    '(("help" "private"))
#|
      ("help" "servlets")
      ("help" "servlets" "private")
      ("help" "servlets" "release")
      ("help" "servlets" "scheme")
      ("help" "servlets" "scheme" "misc")
|#
    )
  
  (define post-install-collection "installer.ss"))
