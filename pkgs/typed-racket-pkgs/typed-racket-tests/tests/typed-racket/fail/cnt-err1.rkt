#;
(exn-pred exn:fail:contract? ".*cons/c.*" #rx".*contract.*\\(->.*")

#lang scheme/load

(module tree typed-scheme
 (define-type-alias Tree (Rec T (U (Pair T T) Number)))

 (: tree-sum (Tree -> Number))
 (define (tree-sum t)
   (cond
     [(number? t) t]
     [else (+ (tree-sum (car t))
              (tree-sum (cdr t)))]))

 (provide tree-sum))

(module client scheme
 (require 'tree)
 (define (try-it bad?)
   (if bad?
       (tree-sum (cons 5 #f))
       (tree-sum (cons 5 6))))
 (provide try-it))

(require 'client)

(try-it #t)
