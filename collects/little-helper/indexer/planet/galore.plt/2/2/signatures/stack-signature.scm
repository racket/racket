(module stack-signature mzscheme
  (provide provide-stack)
  
  (require-for-syntax "macro-utilities.scm")
  
  (define-syntax (provide-stack stx)
    (syntax-case stx ()
      [(_)
       (with-captures stx 
         (elements
          empty
          empty?
          insert
          insert*
          remove ; = remove-first
          remove-first
          first
          first+remove
          fold
          size
          stack?
          ; srfi-42
          stack-ec
          :stack
          )
         (quasisyntax/loc stx
           (begin
             (require (prefix : (lib "contract.ss")))
             (define compare/c  (:flat-named-contract 'compare-function procedure?))
             (define stack/c    (:flat-named-contract 'stack stack?))
             (define list/c     (:flat-named-contract 'list (lambda (o) (or (null? o) (pair? o)))))
             (define boolean/c  (:flat-named-contract 'boolean boolean?))
             (define any/c      :any/c)
             ; srfi-42
             (provide :stack stack-ec)
             ; values
             (provide empty)
             ;;; SYNONYMS
             (provide (rename insert push))
             
             #,(quasisyntax/loc stx
                 (:provide/contract 
              (elements
               (:-> stack/c   list/c))
              (empty?             ; stack -> boolean
               (:-> stack/c   boolean/c))
              (insert            ; element stack -> stack
               (:-> any/c stack/c    stack/c))
              (insert*           ; (list element) stack -> stack
               (:-> list/c stack/c   stack/c))
              ; remove = remove-first
              (remove 
               (:-> stack/c   stack/c))
              (remove-first
               (:-> stack/c   stack/c))
              (first
               (:-> stack/c  any/c))
              (first+remove
               (:->* (stack/c)  (any/c stack/c)))
              (fold              ; (element alpha -> alpha) alpha stack -> alpha
               (:-> (:-> any/c any/c  any/c) any/c stack/c    any/c)) 
              (size              ; stack -> integer
               (:-> stack/c  :natural-number/c))
              (stack?              ; object -> boolean
               (:-> :any/c boolean/c))
              )))))]))
  )
