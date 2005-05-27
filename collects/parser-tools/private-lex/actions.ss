(module actions mzscheme
  (provide (all-defined))
  (require (lib "stx.ss" "syntax"))
    
  ;; get-special-action: (syntax-object list) syntax-object syntax-object  -> syntax-object
  ;; Returns the first action from a rule of the form ((which-special) action)
  (define (get-special-action rules which-special none)
    (cond
      ((null? rules) none)
      (else
       (syntax-case (car rules) ()
         (((special) act)
          (module-or-top-identifier=? (syntax special) which-special)
          (syntax act))
         (_ (get-special-action (cdr rules) which-special none))))))
  

  
  )
