(module lex-plt-v200 mzscheme
  (require parser-tools/lex
           (prefix : parser-tools/lex-sre))
  
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
  
  
