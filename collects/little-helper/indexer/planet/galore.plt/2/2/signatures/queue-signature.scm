;;; queue-signature.scm  --  Jens Axel Søgaard

(module queue-signature mzscheme
  (provide provide-queue
           provide-deque)
  
  (require-for-syntax "macro-utilities.scm")
  
  ;     ###                                  
  ;    #   #                                 
  ;   #     # ##  ##   ####   ##  ##   ####  
  ;   #     #  #   #  #    #   #   #  #    # 
  ;   #     #  #   #  ######   #   #  ###### 
  ;   #     #  #   #  #        #   #  #      
  ;    #   #   #  ##  #        #  ##  #      
  ;     ###     ## ##  #####    ## ##  ##### 
  ;     #####                                
  
  (define-syntax (provide-queue stx)
    (syntax-case stx ()
      [(_)
       (with-captures stx 
         (elements
          empty
          empty?
          first
          first+remove
          fold
          insert
          insert*
          insert-last
          remove
          remove-first
          size
          queue?
          queue-ec
          :queue)
         (quasisyntax/loc stx
           (begin
             (require (prefix : (lib "contract.ss")))
             (define compare/c  (:flat-named-contract 'compare-function procedure?))
             (define queue/c    (:flat-named-contract 'queue queue?))
             (define list/c     (:flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
             (define boolean/c  (:flat-named-contract 'boolean boolean?))
             (define any/c      :any/c)
             ; srfi-42
             (provide :queue queue-ec)
             ; values
             (provide empty)
             
             #,(quasisyntax/loc stx
                 (:provide/contract 
                  (elements
                   (:-> queue/c     list/c))
                  (empty?             
                   (:-> queue/c     boolean/c))
                  (first 
                   (:-> queue/c     any/c))
                  (first+remove
                   (:->* (queue/c)  (any/c queue/c)))
                  (fold             
                   (:-> (:-> any/c any/c  any/c) any/c queue/c    any/c)) 
                  ; insert = insert-last
                  (insert            
                   (:-> any/c queue/c    queue/c))
                  (insert-last
                   (:-> any/c queue/c    queue/c))
                  (insert*           
                   (:-> list/c queue/c   queue/c))
                  ; remove = remove-first
                  (remove
                   (:-> queue/c  any/c))
                  (remove-first
                   (:-> queue/c   queue/c))
                  (size              
                   (:-> queue/c   :natural-number/c))
                  (queue?   
                   (:-> :any/c    boolean/c))
                  )))))]))
  
  ;   ####                                   
  ;    #  #                                  
  ;    #   #   ####    ### ## ##  ##   ####  
  ;    #   #  #    #  #   ##   #   #  #    # 
  ;    #   #  ######  #    #   #   #  ###### 
  ;    #   #  #       #    #   #   #  #      
  ;    #  #   #       #   ##   #  ##  #      
  ;   ####     #####   ### #    ## ##  ##### 
  ;                        #                 
  ;                       ###                

  ; TODO: Figure ot how to use (provide-queue) below, without
  ;       getting an 
  ;           "expand: unbound variable in module in: queue?
  ;       error in batched-deque.scm.
  
  (define-syntax (provide-deque stx)
    (syntax-case stx ()
      [(_)
       (with-captures stx 
         (;queue
          elements
          empty
          empty?
          first
          first+remove
          fold
          insert
          insert*
          insert-last
          remove
          remove-first
          size
          queue?
          queue-ec
          :queue
          ; deque
          deque?
          deque-ec
          :deque
          insert-first
          remove-last
          last)
         (quasisyntax/loc stx
           (begin
             (require (prefix : (lib "contract.ss")))
             (define compare/c  (:flat-named-contract 'compare-function procedure?))
             (define deque/c    (:flat-named-contract 'deque deque?))
             (define list/c     (:flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
             (define boolean/c  (:flat-named-contract 'boolean boolean?))
             (define any/c      :any/c)
             ; srfi-42
             (provide :deque deque-ec
                      :queue queue-ec)
             ; values
             (provide empty)
             #,(quasisyntax/loc stx
                 (begin
                   ;; queue
                   (:provide/contract 
                    (elements
                     (:-> deque/c     list/c))
                    (empty?             
                     (:-> deque/c     boolean/c))
                    (first 
                     (:-> deque/c     any/c))
                    (first+remove
                     (:->* (deque/c)  (any/c deque/c)))
                    (fold             
                     (:-> (:-> any/c any/c  any/c) any/c deque/c    any/c)) 
                    ; insert = insert-last
                    (insert            
                     (:-> any/c deque/c    deque/c))
                    (insert-last
                     (:-> any/c deque/c    deque/c))
                    (insert*           
                     (:-> list/c deque/c   deque/c))
                    ; remove = remove-first
                    (remove
                     (:-> deque/c  any/c))
                    (remove-first
                     (:-> deque/c   deque/c))
                    (size              
                     (:-> deque/c   :natural-number/c))
                    (queue?   
                     (:-> :any/c    boolean/c))
                    )
                   ;; dequeue
                   (:provide/contract 
                    (deque?
                     (:-> deque/c         boolean/c))
                    (insert-first
                     (:-> any/c deque/c   any/c))
                    (remove-last
                     (:-> deque/c         deque/c))
                    (last
                     (:-> deque/c         any/c))
                    )))))
           )]))
  
)

