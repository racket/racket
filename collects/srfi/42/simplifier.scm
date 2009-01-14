;;;
;;; SIMPLIFIER
;;;

(module simplifier mzscheme
  (provide ec-simplify)
  
  ; (ec-simplify <expression>)
  ;   generates potentially more efficient code for <expression>.
  ;   The macro handles if, (begin <command>*), and (let () <command>*)
  ;   and takes care of special cases.
  ; 
  ; NOTE: Turns out the JIT optimizer doesn't handle (not (not x)) 
  ;       and (if (not x) expr1 expr2) => (if x expr2 expr1)
  ;       yet.
  
  #;(define-syntax ec-simplify
      (syntax-rules ()
        [(_ . (more)) more]))
  
  (require-for-syntax (lib "stx.ss" "syntax"))
  
  (define-syntax (ec-simplify stx)
    (syntax-case* stx (if not let begin #%top #%app) module-or-top-identifier=?
      
      ; one- and two-sided if
      
      ; literal <test>
      ((ec-simplify (if #t consequent))
       #'consequent )
      ((ec-simplify (if #f consequent))
       #'(if #f #f) )
      ((ec-simplify (if #t consequent alternate))
       #'consequent )
      ((ec-simplify (if #f consequent alternate))
       #'alternate )
      
      ; (not (not <test>))
      ((ec-simplify (if (not (not test)) consequent))
       #'(ec-simplify (if test consequent)) )
      
      ((ec-simplify (if (#%app not (not test)) consequent))
       #'(ec-simplify (if test consequent)) )
      
      ((ec-simplify (if (not (#%app not test)) consequent))
       #'(ec-simplify (if test consequent)) )
      
      ((ec-simplify (if (#%app not (#%app not test)) consequent))
       #'(ec-simplify (if test consequent)) )
      
      ((ec-simplify (if (not (not test)) consequent alternate))
       #'(ec-simplify (if test consequent alternate)) )
      
      ((ec-simplify (if (#%app not (not test)) consequent alternate))
       #'(ec-simplify (if test consequent alternate)) )
      
      ((ec-simplify (if (not (#%app not test)) consequent alternate))
       #'(ec-simplify (if test consequent alternate)) )
      
      ((ec-simplify (if (#%app not (#%app not test)) consequent alternate))
       #'(ec-simplify (if test consequent alternate)) )
      
      
      ; (let () <command>*) 
      
      ; empty <binding spec>*
      #;((ec-simplify (let () command ...))
         #'(ec-simplify (begin command ...)) )
      ; NOTE: Removed. let introduces a new scope, begin doesn't
      
      
      ; begin 
      
      ; flatten use helper (ec-simplify 1 done to-do)
      ((ec-simplify (begin command ...))
       #'(ec-simplify 1 () (command ...)) )
      ((ec-simplify 1 done ((begin to-do1 ...) to-do2 ...))
       #'(ec-simplify 1 done (to-do1 ... to-do2 ...)) )
      ((ec-simplify 1 (done ...) (to-do1 to-do ...))
       #'(ec-simplify 1 (done ... to-do1) (to-do ...)) )
      
      ; exit helper
      ((ec-simplify 1 () ())
       #'(if #f #f) )
      ((ec-simplify 1 (command) ())
       #'command )
      ((ec-simplify 1 (command1 command ...) ())
       #'(begin command1 command ...) )
      
      ; anything else
      
      ((ec-simplify expression)
       #'expression ))))
