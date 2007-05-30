(module howtoprogram mzscheme
  (require "private/util.ss"
           "private/headelts.ss"
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
         (head ,hd-css ,@hd-links (title "Program Design"))
         (body
          (h1  "Program Design")
          ,(color-highlight `(h2  "For Students"))
          "The textbook " (i  "How to Design Programs")
          " provides an introduction to programming using the DrScheme"
          " environment.  The book is not distributed with DrScheme, but it"
          " is available online at "
          (pre "   " (a ([href "http://www.htdp.org/"] [target "_top"])
                        "http://www.htdp.org/"))
          (p)
          "Help Desk provides the following interactive support for the textbook:"
          (ul (li (b (a ([href "/servlets/teachpacks.ss"])
                        "Teachpack documentation"))))
          (p)
          ,(color-highlight `(h2  "For Experienced Programmers"))
          (ul (li (b (a ((href ,(get-manual-index "t-y-scheme")))
                        "Teach Yourself Scheme in Fixnum Days"))
                  ": For programmers with lots of experience in other languages"))
          ,(color-highlight `(h2 "For Teachers and Researchers"))
          (ul (li (b (a ([href "/servlets/research/why.ss"]) "Why DrScheme?"))
                  ": PLT's vision "))))))))