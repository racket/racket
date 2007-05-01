(module button mzscheme
 (require (lib "mred.ss" "mred")
          (lib "class.ss"))
 (provide interface-version timeout start)
 (define interface-version 'v1)
 (define timeout +inf.0)
 (define (start initial-request)
   (let ([b (new button% (label "Button"))])
   (list #"text/plain" "Button"))))
