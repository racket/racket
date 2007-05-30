(module teachpacks mzscheme
  (require "private/util.ss"
           "../private/get-help-url.ss"
           "../private/manuals.ss"
	   (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       `(html
         (head (title "Teachpacks"))
         (body (h1 "Teachpacks")
               (ul (li (b (a ([href ,(get-manual-index "teachpack")])
                             "Teachpacks for \"How to Design Programs\"")))
                   (li (b (a ([href ,(get-manual-index "teachpack-htdc")])
                             "Teachpacks for \"How to Design Classes\""))))))))))