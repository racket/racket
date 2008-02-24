(module aux-class mzscheme 
  (require mzlib/class)
  
  (provide 
   (all-from mzlib/class) 
   define/abstract ; (define/abstract <identifier>) :: <definition> 
   )
  
  (define-syntax define/abstract 
    (syntax-rules ()
      [(define/abstract id)
       (define/public id (lambda x (error 'id "abstract")))]))
  
  )
