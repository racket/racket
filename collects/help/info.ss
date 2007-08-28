; help collection
(module info (lib "infotab.ss" "setup")
  (define name "Help")
  (define doc.txt "doc.txt")
  (define compile-subcollections
    '(("help" "private")
      ("help" "servlets")
      ("help" "servlets" "private")
      ("help" "servlets" "release")
      ("help" "servlets" "scheme")
      ("help" "servlets" "scheme" "misc")))
  (define help-desk-message
    "Mr: (require (lib \"help-desk.ss\" \"help\"))")
  (define mred-launcher-libraries '("help.ss"))
  (define mred-launcher-names     '("Help Desk"))
  (define mzscheme-launcher-libraries '("help-desk-server.ss"))
  (define mzscheme-launcher-names     '("Help Desk Server"))
  (define install-collection "installer.ss")
  (define compile-omit-files '("launch.ss")))
