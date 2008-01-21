(module info setup/infotab
  (define name "frtime")
;  (define doc.txt "doc.txt")

  (define scribblings '(("frtime.scrbl" ())))  
  (define compile-subcollections (list (list "frtime" "demos" "gui")))
  (define tools (list "frtime-tool.ss"))
  (define tool-icons (list '("clock.png" "frtime")))
  (define tool-names (list "FrTime Languages")))
