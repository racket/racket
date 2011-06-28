#lang racket

(require redex/reduction-semantics)

(define-language bytecode
  (e (loc n)
     (loc-noclr n)
     (loc-clr n)
     (loc-box n)
     (loc-box-noclr n)
     (loc-box-clr n)
     
     (let-one e e)
     (let-void n e)
     (let-void-box n e)
     
     (boxenv n e)
     (install-value n e e)
     (install-value-box n e e)
     
     (application e e ...)
     (seq e e e ...)
     (branch e e e)
     (let-rec (l ...) e)
     (indirect x)
     (proc-const (τ ...) e)
     (case-lam l ...)
     l
     v)
  
  (l (lam (τ ...) (n ...) e))
  
  (v number
     void
     'variable
     b)
  
  (τ val ref)
  (n natural)
  (b #t #f)
  ((x y) variable))

(provide bytecode)
