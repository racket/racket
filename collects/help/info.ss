; help collection
(module info (lib "infotab.ss" "setup")
  (define name "Help")
  (define doc.txt "doc.txt")
  (define compile-subcollections 
    '(("help" "private")
      ("help" "servlets")
      ("help" "servlets" "private")
      ("help" "servlets" "release")
      ("help" "servlets" "research")
      ("help" "servlets" "resources")
      ("help" "servlets" "scheme")
      ("help" "servlets" "scheme" "misc")))
  (define help-desk-message
    "Mr: (require (lib \"help-desk.ss\" \"help\"))")
  (define mred-launcher-libraries (list "help.ss"))
  (define mred-launcher-names (list "Help Desk"))
  (define install-collection "installer.ss"))
