(module 5-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 2)))
  (require (lib "contract.ss"))
  (require queue)
  
  (define s (put (put (initialize (flat-contract integer?) =) 2) 1))
  
  (test/text-ui
   (make-test-suite 
    "queue"
    (make-test-case 
     "empty"
     (assert-true (is-empty? (initialize (flat-contract integer?) =))))
    (make-test-case 
     "put"
     (assert-true (queue? s)))
    (make-test-case 
     "count"
     (assert = (count s) 2))
    (make-test-case 
     "put exn"
     #;(assert-exn exn:fail:contract?
                   (push (initialize (flat-contract integer?)) 'a))
       (assert-true (with-handlers ([exn:fail:contract? (lambda _ #t)])
                      (put (initialize (flat-contract integer?)) 'a)
                      #f)))
    (make-test-case 
     "remove" 
     (assert-true (queue? (remove s))))
    (make-test-case 
     "head"
     (assert = (head s) 2))))
  )
