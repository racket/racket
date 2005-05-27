(module aux-class mzscheme 
  (require (lib "class.ss"))
  
  (provide 
   (all-from (lib "class.ss")) 
   define/abstract ; (define/abstract <identifier>) :: <definition> 
   )
  
  (define-syntax define/abstract 
    (syntax-rules ()
      [(define/abstract id)
       (define/public id (lambda x (error 'id "abstract")))]))
  
  )