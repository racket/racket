(module teachpacks mzscheme
  (require "private/util.ss"
           "../private/get-help-url.ss"
           "../private/manuals.ss"
	   (lib "servlet.ss" "web-server"))

  (provide interface-version timeout start)

  (define interface-version 'v1)
  (define timeout +inf.0)

  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html
      (head (title "Teachpacks"))
      (BODY
       (H1 "Teachpacks")
       (UL (LI (B (A ((HREF ,(get-manual-index "teachpack")))
                     "Teachpacks for \"How to Design Programs\"")))
           (LI (B (A ((HREF ,(get-manual-index "teachpack-htdc")))
                     "Teachpacks for \"How to Design Classes\""))))))))
