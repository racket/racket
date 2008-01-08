(module 3-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 2)))
  (require (lib "contract.ss"))
  (require "3.ss")
  
  (define d
    (put (put (put (initialize (flat-contract integer?) =) 'a 2) 'b 2) 'c 1))

  (test/text-ui
   (make-test-suite 
    "dictionaries"
    (make-test-case 
     "value for"
     (assert = (value-for d 'b) 2))
    (make-test-case 
     "has?"
     (assert-false (has? (remove d 'b) 'b)))
    (make-test-case 
     "count"
     (assert = 3 (count d))))))
 