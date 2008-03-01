;;; list-stack.scm  --  Jens Axel Soegaard  --  6th nov 2005

(module list-stack mzscheme
  (require (only (lib "list.ss") foldl)
           (lib "42.ss" "srfi"))
  
  (define-struct stack (elements)) 
  
  (define empty (make-stack '()))
  
  (define (empty? s)
    (null? (stack-elements s)))
  
  (define (insert x s)
    (make-stack (cons x (stack-elements s))))
  
  (define (insert* xs s)
    (foldl insert s xs))
  
  (define (remove-first s)
    (when (empty? s)
      (error 'remove-first "remove-first called on empty list"))
    (make-stack (cdr (stack-elements s))))
  
  (define (remove s)
    (remove-first s))
  
  (define (first s)
    (when (empty? s)
      (error 'first "first called on empty list"))
    (car (stack-elements s)))
  
  (define (first+remove s)
    (when (empty? s)
      (error 'first+remove "remove-first called on empty list"))
    (let ([elements (stack-elements s)])
      (values (car elements)
              (make-stack (cdr elements)))))
  
  (define (elements s)
    (if (empty? s)
        '()
        (let-values ([(f s) (first+remove s)])
          (cons f (elements s)))))
  
  (define (fold f b s)
    (if (empty? s)
        b
        (let-values ([(x xs) (first+remove s)])
          (fold f (f x b) xs))))
  
  (define (size s)
    (length (stack-elements s)))
  
  ;; support for srfi-42

  (define-syntax stack-ec
    (syntax-rules ()
      [(stack-ec etc1 etc ...)
       (fold-ec empty etc1 etc ... insert)]))

  (define-syntax :stack
    (syntax-rules (index)
      ((:stack cc var (index i) arg)
       (:parallel cc (:stack var arg) (:integers i)) )
      ((:stack cc var arg)
       (:do cc
            (let ())
            ((t arg))
            (not (empty? t))
            (let ((var (first t))))
            #t
            ((remove-first t)) ))))

  (define (:stack-dispatch args)
    (cond
      [(null? args)
       'stack]
      [(and  (stack? (car args)))
       (:generator-proc (:stack (car args)))]
      [else
       #f]))
  
  (:-dispatch-set! 
   (dispatch-union (:-dispatch-ref) :stack-dispatch))

  (require "signatures/stack-signature.scm")
  (provide-stack)
  )
