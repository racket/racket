(module run-tests mzscheme

  (require "honu-tests.ss"
           )

  (provide test/text test/graphical)

  (define (test/text)
    ((dynamic-require '(planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 1)) 'test/text-ui)
     honu-tests))

  (define (test/graphical)
    ((dynamic-require '(planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 1 1)) 'test/graphical-ui)
     honu-tests))

  )


  

  