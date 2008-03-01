(module info (lib "infotab.ss" "setup")
  (define name "Srfi")
  (define blurb
    (list '(span (b "SRFI Extensions and SRFI Drafts")
                 (p (b "SRFI 42") " Eager comprehensions extended with the generators "
                    ":match, :plt-match, :let-value, :pairs, :do-until, and more")
                 (p (b "SRFI 78 Draft") "  Lightweight testing (pr 18-jan-2006)"))))
  (define primary-file "42.ss / 78.ss")
  (define doc.txt "doc.txt")
  (define categories '(devtools))
  (define required-core-version "360")
  (define version "1.2"))

