;;; bag-signature.scm  --  Jens Axel Søgaard

(module bag-signature mzscheme
  (provide provide-bag)
  
  (require-for-syntax "macro-utilities.scm")
  
  (define-syntax (provide-bag stx)
    (syntax-case stx ()
      [(_)
       (with-captures stx 
         (bag bag? count delete delete* delete-all difference 
          elements empty empty? equal=? -bag fold fold/no   get
          insert   insert* intersection list->bag member? occurences
            select       singleton size size-distinct
          subbag? union
          ; srfi-42
          :bag bag-ec)
         (quasisyntax/loc stx
           (begin
             (require (prefix : (lib "contract.ss")))
             (define compare/c  (:flat-named-contract 'compare-function procedure?))
             (define bag/c      (:flat-named-contract 'bag bag?))
             (define ne-bag/c   (:flat-named-contract 'non-empty-bag (lambda (o) (and (bag? o) (not (empty? o))))))
             (define list/c     (:flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
             (define boolean/c  (:flat-named-contract 'boolean boolean?))
             (define any/c      :any/c)
             ; srfi-42
             (provide :bag bag-ec)
             #,(quasisyntax/loc stx
             (:provide/contract 
              (bag?             
               (:-> any/c boolean/c))
              (count
               (:-> any/c bag/c   :natural-number/c))
              (delete           
               (:-> :any/c bag/c   bag/c))
              (delete-all           
               (:-> :any/c bag/c   bag/c))
              (delete*          
               (:-> list/c bag/c   bag/c))
              (elements         
               (:-> bag/c list/c))
              (empty 
               (:case-> (:-> bag/c) 
                        (:-> compare/c bag/c)))
              (empty?           
               (:-> bag/c   boolean/c))
              (equal=?          
               (:-> bag/c bag/c   boolean/c))
              (fold             
               (:-> (:-> any/c any/c   any/c) any/c bag/c   any/c))
              (fold/no
               (:-> (:-> any/c :natural-number/c any/c   any/c) any/c bag/c   any/c))
              (get              
               (:-> any/c bag/c any/c))
              (insert           
               (:-> any/c bag/c   bag/c))
              (insert*          
               (:-> list/c bag/c   bag/c))
              (intersection     
               (:-> bag/c bag/c   bag/c))
              (list->bag 
               (:case-> (:-> list/c bag/c) 
                        (:-> compare/c list/c bag/c)))
              (member?          
               (:-> :any/c bag/c   boolean/c))
              (difference       
               (:-> bag/c bag/c   bag/c))
              (rename -bag bag
                      (:->* () :any/c (bag/c)))
              (select
               (:-> ne-bag/c  :any/c))
              (singleton 
               (:case-> (:-> :any/c bag/c) 
                        (:-> compare/c :any/c bag/c)))
              (size             
               (:-> bag/c  :natural-number/c))
              (subbag?          
               (:-> bag/c bag/c   boolean/c))
              (union            
               (:-> bag/c bag/c   bag/c))
              )))))]))
  )
   