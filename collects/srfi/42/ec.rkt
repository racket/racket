(module ec mzscheme
  (require "ec-core.scm" "extra-generators.scm")
  (provide 
   ;; generators.scm 
   define-generator
   define-indexed-generator-with-append
   define-indexed-generator-without-append
   ;; (the generators present in the reference implementation)
   :list :integers :string :bytes :vector
   :range :real-range :char-range
   :port
   :let :parallel :until :do :while

   (all-from "extra-generators.scm")
   
   ;; comprehensions.scm
   
   define-comprehension
   define-derived-comprehension
   
   list-ec
   do-ec
   append-ec
   string-ec
   string-append-ec
   vector-ec
   vector-of-length-ec
   fold3-ec
   fold-ec
   sum-ec
   product-ec
   min-ec
   max-ec
   last-ec
   first-ec
   any?-ec
   every?-ec

   ;; dispatching.scm
   :dispatched
   :generator-proc
   dispatch-union
   make-initial-:-dispatch
   :-dispatch-ref 
   :-dispatch-set!
   :
   
   ;; expansion.scm
   add-index
   )
  
  )
