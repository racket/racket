(module support "../../typed-scheme.ss"
  (require (for-syntax scheme/base))
  (provide sqr test first second third fourth string->sexpr rest foldl)
  
  (define: (sqr [a : number]) : number (* a a))
  
  (define-type-alias SExp (mu s (Un Number Boolean String Symbol (Listof s))))  
  
  (define-syntax (test stx) #'#f)
  (pdefine: (a) (first [x : (Listof a)]) : a (car x))
  (pdefine: (a) (second [x : (Listof a)]) : a (car (cdr x)))
  (pdefine: (a) (third [x : (Listof a)]) : a (car (cdr (cdr x))))
  (pdefine: (a) (fourth [x : (Listof a)]) : a (car (cdr (cdr (cdr x)))))
  (pdefine: (a) (rest [x : (Listof a)]) : (Listof a) (cdr x))
  (define: (string->sexpr [s : String]) : Sexp
    (read (open-input-string s)))
  
  #;(define: (list-of [f : (Any -> Any)]) : Any
    (lambda: ([l : List]) (andmap f l)))
  
  )
