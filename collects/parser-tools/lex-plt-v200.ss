(module lex-plt-v200 mzscheme
  (require (lib "lex.ss" "parser-tools")
           (prefix : (lib "lex-sre.ss" "parser-tools")))
  
  (provide epsilon
           ~
           (rename :* *)
           (rename :+ +)
           (rename :? ?)
           (rename :or :)
           (rename :& &)
           (rename :: @)
           (rename :~ ^)
           (rename :/ -))
           
  (define-lex-trans epsilon
    (syntax-rules ()
      ((_) "")))
  
  (define-lex-trans ~
    (syntax-rules ()
      ((_ re) (complement re)))))
  
  
