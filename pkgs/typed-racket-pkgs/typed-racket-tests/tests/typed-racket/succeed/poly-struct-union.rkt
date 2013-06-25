
#lang racket/load


(module test typed/racket


 (provide a:list-helper)

 (define-type (Append-List+ elem) (U (a:singleton elem) (a:join elem)))

 (define-struct: (elem) a:join
   ((left : (Append-List+ elem)) (right : (Append-List+ elem)))
   #:transparent)

 (define-struct: (elem) a:singleton
   ((elem : elem)) #:transparent)

 (: a:list-helper (All (elem) ((Pair elem (Listof elem)) -> (Append-List+ elem))))
 (define (a:list-helper elems)
   (cond
     ((empty? (cdr elems)) (make-a:singleton (car elems)))
     (else (make-a:join (make-a:singleton (car elems))
                        (a:list-helper (cdr elems))))))
 )

(module test2 racket
 (require 'test)
 (a:list-helper (list 1 2)))

(require 'test2)
