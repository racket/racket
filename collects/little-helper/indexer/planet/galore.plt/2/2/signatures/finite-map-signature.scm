;;; finite-map-signature.scm  --  Jens Axel Søgaard

(module finite-map-signature mzscheme
  (provide provide-finite-map)
  
  (require-for-syntax "macro-utilities.scm")
  
  (define-syntax (provide-finite-map stx)
    (syntax-case stx ()
      [(_)
       (with-captures stx 
          (finite-map? count delete delete* delete-all difference 
                       elements empty     empty?       equal=?   exported-finite-map fold fold/key   get
                       insert   insert*   intersection keys list->finite-map member? occurences
                       select   singleton size         size-distinct
                       union
                       ; srfi-42
                       :finite-map finite-map-ec)
          (quasisyntax/loc stx
            (begin
              (require (prefix : (lib "contract.ss")))
              (define compare/c     (:flat-named-contract 'compare-function procedure?))
              (define finite-map/c  (:flat-named-contract 'finite-map finite-map?))
              (define list/c        (:flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
              (define boolean/c     (:flat-named-contract 'boolean boolean?))
              (define any/c         :any/c)
              ; srfi-42
              ;(provide :finite-map finite-map-ec)
              #,(quasisyntax/loc stx
                  (:provide/contract 
                   (count
                    (:-> any/c finite-map/c    :natural-number/c))
                   (delete           
                    (:-> :any/c finite-map/c   finite-map/c))
                   (delete-all           
                    (:-> :any/c finite-map/c   finite-map/c))
                   (delete*          
                    (:-> list/c finite-map/c   finite-map/c))
                   (elements         
                    (:-> finite-map/c list/c))
                   (empty 
                    (:case-> (:-> finite-map/c) 
                             (:-> compare/c finite-map/c)))
                   (empty?           
                    (:-> finite-map/c   boolean/c))
                   (equal=?          
                    (:-> finite-map/c finite-map/c   boolean/c))
                   (finite-map?             
                    (:-> any/c boolean/c))
                   (fold             
                    (:-> (:-> any/c any/c   any/c) any/c finite-map/c   any/c))
                   (fold/key
                    (:-> (:-> any/c :any/c any/c   any/c) any/c finite-map/c   any/c))
                   (get              
                    (:-> any/c finite-map/c any/c))
                   (insert           
                    (:-> any/c any/c finite-map/c   finite-map/c))
                   (insert*           
                    (:-> list/c finite-map/c   finite-map/c))
                   (intersection     
                    (:-> finite-map/c finite-map/c   finite-map/c))
                   (keys
                    (:-> finite-map/c list/c))
           #;      (list->finite-map 
                    (:case-> (:-> list/c finite-map/c) 
                             (:-> compare/c list/c finite-map/c)))
                   (member?          
                    (:-> :any/c finite-map/c   boolean/c))
                   (difference       
                    (:-> finite-map/c finite-map/c   finite-map/c))
                   (select
                    (:-> finite-map/c  :any/c))
                   (singleton 
                    (:case-> (:-> :any/c any/c finite-map/c) 
                             (:-> compare/c :any/c any/c finite-map/c)))
                   (size             
                    (:-> finite-map/c  :natural-number/c))
                   (union            
                    (:-> finite-map/c finite-map/c   finite-map/c))
                   )))))]))
  )
