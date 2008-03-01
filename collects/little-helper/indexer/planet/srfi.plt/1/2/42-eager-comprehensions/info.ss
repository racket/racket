(module info (lib "infotab.ss" "setup")
  (define name "Srfi")
  (define blurb
    (list '(div (p (b "SRFI Extensions"))
                (p "This package provides extensions to:")
                (p (b "SRFI 42") "-  Eager comprehensions: "
                   "Extra generators :match, :plt-match, :let-value, :pairs, :do-until, and others"))))
  (define primary-file "42.ss")
  (define doc.txt "doc.txt")
  (define required-core-version field "360")
  (define categories '(development))
  (define version "1.2"))

