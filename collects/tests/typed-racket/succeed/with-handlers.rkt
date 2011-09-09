
#lang typed-scheme

(define: (f [i : Integer]) : (Pair String Char)
 (cons "foo" #\space))

(define: (is-happiness-a-warm-gun?) : Boolean
 (with-handlers ([integer? (lambda: ([x : Any]) #t)])
   (f 42)
   #t))
