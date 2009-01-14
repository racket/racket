;;;
;;; GENERATORS
;;;


(module generator-struct mzscheme
  (provide generator     
           generator?
           generator-clause->loop     
           make-generator)
  
  ; A GENERATOR has a name, and function form->loop,
  ; that takes a syntax-object representing an instance
  ; of a generator clause as input. For example 
  ; #'(:list x (list 1 2 3)). The function form->loop 
  ; returns a loop structure.
  (define-struct generator (name clause->loop)))
