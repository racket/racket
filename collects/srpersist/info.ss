;; info.ss for srpersist collection

;; no .zo compilation necessary, since all the
;; real code is in C++

(module info setup/infotab
  (define name "SrPersist")
  (define doc.txt "doc.txt")
  (define compile-omit-files '("info.ss" "srpersist.ss")))
