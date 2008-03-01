;;; heap-signature.scm  --  Jens Axel Søgaard  --  6th nov 2005

(module heap-signature mzscheme
  (provide provide-heap)
  
  (require-for-syntax "macro-utilities.scm")
  
  (define-syntax (provide-heap stx)
    (syntax-case stx ()
      [(_)
       (with-captures stx 
         (count
          delete
          delete-all
          delete-min
          elements
          empty
          empty? 
          find-min
          fold
          get
          -heap
          heap?
          heap-ec
          :heap
          insert
          insert*
          list->heap
          union
          remove
          select
          singleton
          size)
         (quasisyntax/loc stx
           (begin
             (require (prefix : (lib "contract.ss")))
             (define compare/c  (:flat-named-contract 'compare-function procedure?))
             (define heap/c     (:flat-named-contract 'heap heap?))
             (define list/c     (:flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
             (define boolean/c  (:flat-named-contract 'boolean boolean?))
             (define any/c      :any/c)
             ; srfi-42
             (provide :heap heap-ec)
             #,(quasisyntax/loc stx
             (:provide/contract 
              (count
               (:-> any/c heap/c   :natural-number/c))
              (delete
               (:-> any/c heap/c   heap/c))
              (delete-all
               (:-> any/c heap/c   heap/c))
              (delete-min 
               (:-> heap/c   heap/c))
              (elements
               (:-> heap/c   list/c))
              (empty   
               (:case-> (:->             heap/c) 
                        (:-> compare/c   heap/c)))
              (empty?             
               (:-> heap/c   boolean/c))
              (find-min
               (:-> heap/c   any/c))
              (fold             
               (:-> (:-> any/c any/c  any/c) any/c heap/c    any/c))
              (get               
               (:-> any/c heap/c any/c))
              (rename -heap heap
                      (:->* () :any/c (heap/c)))
              (heap?
               (:-> :any/c boolean/c))
              (insert            
               (:-> any/c heap/c    heap/c))
              (insert*           
               (:-> list/c heap/c   heap/c))
              (list->heap
               (:case-> (:-> list/c            heap/c) 
                        (:-> compare/c list/c  heap/c)))
              (singleton
               (:case-> (:-> any/c            heap/c) 
                        (:-> compare/c any/c  heap/c)))
              (select
               (:-> heap/c  any/c))
              (size 
               (:-> heap/c  :natural-number/c))
              (union 
               (:-> heap/c heap/c   heap/c)))))))]))
  )