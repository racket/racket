#;
(exn-pred "'success")

;; PR 12434
(module B typed/racket

 (require/typed racket
   [opaque NN number?]
   [pi NN])
  
  (define a string)
 ;; => 'failure
  (ann (match pi
         [(list n) 'success]
         [other 'failure])
       (U #;'success 'failure)))
