;;; set-signature.scm  --  Jens Axel Søgaard

(module set-signature mzscheme
  (provide provide-set)
  
  (require-for-syntax "macro-utilities.scm")
  
  (define-syntax (provide-set stx)
    (syntax-case stx ()
      [(_)
       (with-captures stx 
         (delete delete* delete-all delete-min difference 
                 elements empty empty? equal=? find-min fold get
                 insert insert/combiner  insert* insert*/combiner intersection intersection/combiner
                 list->set list->set/combiner member?
                 select set set? singleton size
                 subset? union
                 ; srfi-42
                 :set set-ec)
         (quasisyntax/loc stx
           (begin
             (require (prefix : (lib "contract.ss")))
             (define compare/c  (:flat-named-contract 'compare-function procedure?))
             (define set/c      (:flat-named-contract 'set set?))
             (define list/c     (:flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
             (define boolean/c  (:flat-named-contract 'boolean boolean?))
             (define any/c      :any/c)
             (define combiner/c (:-> any/c any/c  any/c))
             ; srfi-42
             (provide :set set-ec)
             #,(quasisyntax/loc stx
                 (:provide/contract 
                  (delete
                   (any/c set/c . :-> . set/c))
                  (delete*   
                   (list/c set/c . :-> . set/c))
                  (delete-all
                   (any/c set/c . :-> . set/c))
                  (delete-min
                   (set/c . :-> . set/c))
                  (difference        
                   (set/c set/c . :-> . set/c))
                  (elements          
                   (:-> set/c list/c))
                  (empty 
                   (:case-> (:-> set/c) 
                            (:-> compare/c set/c)))
                  (empty? 
                   (:-> set/c boolean/c))
                  (equal=? 
                   (:-> set/c set/c   boolean/c))
                  (find-min
                   (:-> set/c any/c))
                  (fold              
                   (:-> (:-> any/c any/c any/c) any/c set/c any/c)) 
                  (get               
                   (:-> any/c set/c any/c))
                  (insert            
                   (:-> any/c set/c   set/c))
                  (insert/combiner
                   (:-> any/c set/c (:-> any/c any/c  any/c)   set/c))
                  (insert*           
                   (:-> list/c set/c   set/c))
                  (insert*/combiner           
                   (:-> list/c set/c combiner/c  set/c))
                  (intersection      
                   (:-> set/c set/c   set/c))
                  (intersection/combiner      
                   (:-> set/c set/c combiner/c    set/c))
                  (list->set 
                   (:case-> (:-> list/c set/c) 
                            (:-> compare/c any/c set/c)))
                  (list->set/combiner 
                   (:case-> (:-> list/c combiner/c   set/c) 
                            (:-> compare/c any/c combiner/c   set/c)))
                  (member?   
                   (:-> any/c set/c boolean/c))              
                  (set       
                   (:->* () any/c (set/c)))
                  (set?      
                   (:-> any/c boolean/c))
                  (singleton 
                   (:case-> (:-> any/c set/c) 
                            (:-> compare/c any/c set/c)))
                  (size              
                   (:-> set/c :natural-number/c))
                  (subset?   
                   (set/c set/c . :-> . boolean/c))
                  (select
                   (:-> set/c  any/c))
                  (union             
                   (set/c set/c . :-> . set/c))
                  )))))]))
  )
   