;;; priority-queue-signature.scm  --  Jens Axel Søgaard  --  6th nov 2005

(module priority-queue-signature mzscheme
  (provide provide-priority-queue)
  
  (require-for-syntax "macro-utilities.scm")
  
  (define-syntax (provide-priority-queue stx)
    (syntax-case stx ()
      [(_)
       (with-captures stx 
         (delete-min
          elements
          elements+priorities
          empty
          empty? 
          find-min
          find-min-priority
          fold
          priority-queue?
          ;priority-queue-ec
          ;:priority-queue
          insert
          insert*
          size
          union)
         (quasisyntax/loc stx
           (begin
             (require (prefix : (lib "contract.ss")))
             (define compare/c  (:flat-named-contract 'compare-function procedure?))
             (define pq/c       (:flat-named-contract 'priority-queue priority-queue?))
             (define list/c     (:flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
             (define boolean/c  (:flat-named-contract 'boolean boolean?))
             (define number/c   (:flat-named-contract 'number number?))
             (define any/c      :any/c)
             ; srfi-42
             ;(provide :heap heap-ec)
             ; values
             #,(quasisyntax/loc stx
                 (:provide/contract 
                  (delete-min
                   (:-> pq/c  pq/c))
                  (elements
                   (:-> pq/c   list/c))
                  (elements+priorities
                   (:->* (pq/c) (list/c list/c)))
                  (empty 
                   (:case-> (:-> pq/c) 
                            (:-> compare/c pq/c)))
                  (empty?             
                   (:-> pq/c   boolean/c))
                  (find-min
                   (:-> pq/c   any/c))
                  (find-min-priority
                   (:-> pq/c   number/c))
                  (fold             
                   (:-> (:-> any/c any/c  any/c) any/c pq/c    any/c))
                  (insert            
                   (:-> any/c number/c pq/c    pq/c))
                  (insert*           
                   (:-> list/c list/c pq/c   pq/c))
                  (priority-queue?
                   (:-> :any/c boolean/c))
                  (size 
                   (:-> pq/c  :natural-number/c))
                  (union 
                   (:-> pq/c pq/c   pq/c))
                  )))))]))
  )