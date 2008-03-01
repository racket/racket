;;; raw-red-black-tree.ss   --  Jens Axel Søgaard  --  3rd nov 2003

;;; PURPOSE

; This is an implementation of sets using red/black trees
; as representation. 

;;; HISTORY

; This is direct port of Jean-Christophe Filliatre's implementation
; of red-black trees in Ocaml.

;; 22nd jan 2004 [soegaard]
;   - added set? 
;   - fixed bug in inter-list reported by Pinku Surana  
;; 15th feb 2005 [soegaard]
;   - numerous modifications to handle the case were
;     elm= is finer than elm> and elm<
;   - fixed serious bug in unbalanced-left
;     (one sub tree was discarded, the other cloned)
;  17th feb 2005 [soegaard]
;   - fixed bug in diff-list introduced (hopefully) the 15th
;  2nd nov 2005 [soegaard]
;   - changed from unit to module/compare approach
;   - renamed from red-black-tree-set.scm to raw-red-black-tree-set.scm
;  2nd may 2006 [sstrickl]
;   - fixed error in insert/combiner (replacing a black node turned it red)

;;; LICENSE

;  Rbset: Sets implemented as red-black trees.
;  Copyright (C) 2000 Jean-Christophe FILLIATRE
;  
;  This software is free software; you can redistribute it and/or
;  modify it under the terms of the GNU Library General Public
;  License version 2, as published by the Free Software Foundation.
;  
;  This software is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;  
;  See the GNU Library General Public License version 2 for more details
;  (enclosed in the file LGPL).
; 
;
;(*i $Id: rbset.ml,v 1.8 2003/07/03 12:14:54 filliatr Exp $ i*)

;;; SETS IMPLEMENTED AS REB-BLACK TREES.

(module raw-red-black-tree-set mzscheme
  
  (require (all-except (lib "list.ss") empty empty? remove remove* filter)
           (lib "match.ss")
           (lib "67.ss" "srfi"))
  
  (require "raw-set-signature.scm")
  (provide-raw-set)
  
  
  (define list:foldl foldl)
  
  ;;; DATA DEFINITION
  
  ; A RED/BLACK TREE is either
  ;     1.  empty
  ; or  2.  (make-B l x r)     
  ; or  3.  (make-R l x r)
  ; where l and r are red/black trees and x is an element.
  
  (define empty '())                         ; considered black 
  (define empty? null?)
  
  (define-struct B (l x r) (make-inspector))   ; Black tree
  (define-struct R (l x r) (make-inspector))   ; Red tree
  ; Constructor shorthands
  (define (B- l x r) (make-B l x r))
  (define (R- l x r) (make-R l x r))
  
  ; type predicate
  (define (red-black-tree? s)
    (or (null? s) (B? s) (R? s)))
  
  ; for debugging
  (define (->sexp t)
    (define -> ->sexp)
    (match t
      [() '()]
      [($ B l x r) `(B ,(-> l) ,x ,(-> r))]
      [($ R l x r) `(R ,(-> l) ,x ,(-> r))]))
  
  
  ;;; INVARIANTS
  
  ;  (* Invariants: (1) a red node has no red son, and (2) any path from the 
  ;     root to a leaf has the same number of black nodes *)
  ;
  ;  (* Note the use of two constructors [Black] and [Red] to save space
  ;     (resulting in longer code at a few places, e.g. in function [remove]).
  ;     These red-black trees saves 20\% of space w.r.t Ocaml's AVL, which
  ;     store the height into a fourth argument. *)
  
  ;  type elt = Ord.t
  ;  type t = Empty | Black of t * elt * t | Red of t * elt * t
  
  ;  (*s For debug only: checks whether a tree is properly colored *)
  
  ; check : rbt -> integer
  ;   checks invariants and return black height,
  ;   if the invariants are fulfilled
  (define (check s)
    (match s
      [()     0]
      [($ R ($ R _ _ _) _ _)  (error "Red node with red parent" s)]
      [($ R _ _ ($ R _ _ _))  (error "Red node with red parent" s)]
      [($ B l _ r)            (let ([height-left  (check l)]
                                    [height-right (check r)])
                                (if (not (= height-left height-right))
                                    (error)
                                    (+ height-left 1)))]
      [($ R l _ r)            (let ([height-left  (check l)]
                                    [height-right (check r)])
                                (if (not (= height-left height-right))
                                    (error)
                                    height-left))]))
  
  ;;; SET OPERATIONS
  
  (define (member? cmp x s)
    (match s
      [()            #f]
      [($ B l v r)   (if3 (cmp x v)
                          (member? cmp x l)
                          #t
                          (member? cmp x r))]
      [($ R l v r)   (if3 (cmp x v)
                          (member? cmp x l)
                          #t
                          (member? cmp x r))]))
  
  (define (get cmp x s)
    (match s
      [()            #f]
      [($ B l v r)   (if3 (cmp x v)
                          (get cmp x l)
                          v
                          (get cmp x r))]
      [($ R l v r)   (if3 (cmp x v)
                          (get cmp x l)
                          v
                          (get cmp x r))]))
  
  (define (find-min s)
    (match s
      [($ B () v _) v]
      [($ R () v _) v]
      [($ B l _ _)  (find-min l)]
      [($ R l _ _)  (find-min l)]
      [()           (error 'find-min "an empty set does not have an mimimum element")]))
  
  (define (singleton x)
    (B- empty x empty))
  
  (define (list->set cmp xs)
    (list:foldl (lambda (x s) (insert cmp x s)) 
                empty xs))
  
  (define (list->set/combiner cmp xs combine)
    (list:foldl (lambda (x s) (insert/combiner cmp x s combine)) 
                empty xs))
  
  (define (set . xs)
    (list->set (current-compare) xs))
  
  (define -set set)

  ;; BALANCING
  
  (define (lbalance x1 x2 x3)
    (let ([z x2] [d x3])
      (match x1
        [($ R ($ R a x b) y c)  (R- (B- a x b) y (B- c z d))]
        [($ R a x ($ R b y c))  (R- (B- a x b) y (B- c z d))]
        [_                      (B- x1 x2 x3)])))
  
  (define (rbalance x1 x2 x3)
    (let ([a x1] [x x2])
      (match x3
        [($ R ($ R b y c) z d)  (R- (B- a x b) y (B- c z d))]
        [($ R b y ($ R c z d))  (R- (B- a x b) y (B- c z d))]
        [_                      (B- x1 x2 x3)])))

  ;; INSERTION
  
  (define (insert cmp x s)
    (define (ins s)
      (match s
        [()           (R- empty x empty)]
        [($ R a y b)  (if3 (cmp x y)
                           (R- (ins a) y b)
                           s
                           (R- a y (ins b)))]
        [($ B a y b)  (if3 (cmp x y)
                           (lbalance (ins a) y b)
                           s
                           (rbalance a y (ins b)))]))
    (let ([s1 (ins s)])
      ; color the root black
      (match s1
        [($ B _ _ _) s1]
        [($ R a y b) (B- a y b)]
        [()          (error)])))
  
  (define (insert/combiner cmp x s combine)
    (define (ins s)
      (match s
        [()           (R- empty x empty)]
        [($ R a y b)  (if3 (cmp x y)
                           (R- (ins a) y b)
                           (R- a (combine x y) b)
                           (R- a y (ins b)))]
        [($ B a y b)  (if3 (cmp x y)
                           (lbalance (ins a) y b)
                           ;; This is not a new node, but rather a value
                           ;; replacement in an existing node.  Because
                           ;; the tree structure stays the same, keep the
                           ;; blackness of this node instead of making
                           ;; this node red (because it's a "new" node).
                           (B- a (combine x y) b)
                           (rbalance a y (ins b)))]))
    (let ([s1 (ins s)])
      ; color the root black
      (match s1
        [($ B _ _ _) s1]
        [($ R a y b) (B- a y b)]
        [()          (error)])))
  
  (define (insert* cmp xs s)
    (list:foldl (lambda (x acc) (insert cmp x acc)) s xs))
  
  (define (insert*/combiner cmp xs s combine)
    (list:foldl (lambda (x s) (insert/combiner cmp x s combine)) s xs))
  
  ;; REMOVAL
  
  ;  (* [unbalanced_left] repares invariant (2) when the black height of the 
  ;     left son exceeds (by 1) the black height of the right son *)
  ; [original spelling kept -- a quote is a quote ]
  
  (define (unbalanced-left s)
    (match s
      [($ R ($ B t1 x1 t2) x2 t3)              (values (lbalance (R- t1 x1 t2) x2 t3) #f)]
      [($ B ($ B t1 x1 t2) x2 t3)              (values (lbalance (R- t1 x1 t2) x2 t3) #t)]
      [($ B ($ R t1 x1 ($ B t2 x2 t3)) x3 t4)  (values (B- t1 x1 (lbalance (R- t2 x2 t3) x3 t4)) #f)]
      [_                                       (error)]))
  
  ;  (* [unbalanced_right] repares invariant (2) when the black height of the 
  ;     right son exceeds (by 1) the black height of the left son *)
  
  (define (unbalanced-right s)
    (match s
      [($ R t1 x1 ($ B t2 x2 t3))             (values (rbalance t1 x1 (R- t2 x2 t3)) #f)]
      [($ B t1 x1 ($ B t2 x2 t3))             (values (rbalance t1 x1 (R- t2 x2 t3)) #t)]
      [($ B t1 x1 ($ R ($ B t2 x2 t3) x3 t4)) (values (B- (rbalance t1 x1 (R- t2 x2 t3)) x3 t4) #f)]
      [_                                      (error)]))
  
  
  ;  (* [remove_min s = (s',m,b)] extracts the minimum [m] of [s], [s'] being the
  ;     resulting set, and indicates with [b] whether the black height has
  ;     decreased *)
  
  (define (remove-min s)
    (match s
      [()                         (error "remove-min: Called on empty set")]
      ;  minimum is reached 
      [($ B () x ())           (values empty x #t)]
      [($ B () x ($ R l y r))  (values (B- l y r) x #f)]
      [($ B () _ ($ B _ _ _))  (error)]
      [($ R () x r)            (values r x #f)]
      ;  minimum is recursively extracted from [l]
      [($ B l x r)                (let-values ([(l1 m d) (remove-min l)])
                                    (let ([t (B- l1 x r)])
                                      (if d
                                          (let-values ([(t d1) (unbalanced-right t)])
                                            (values t m d1))
                                          (values t m #f))))]
      [($ R l x r)                (let-values ([(l1 m d) (remove-min l)])
                                    (let ([t (R- l1 x r)])
                                      (if d
                                          (let-values ([(t d1) (unbalanced-right t)])
                                            (values t m d1))
                                          (values t m #f))))]))


  (define (blackify s)
    (match s
      [($ R l x r)  (values (B- l x r) #f)]
      [_            (values s #t)]))
  
  ;  (* [remove_aux x s = (s',b)] removes [x] from [s] and indicates with [b] 
  ;     whether the black height has decreased *)
  
  (define (remove cmp x s)
    (define (remove-aux s)
      (match s
        [()           (values empty #f)]
        [($ B l y r)  (if3 (cmp x y)
                           (let-values ([(l1 d) (remove-aux l)])
                             (let ([t (B- l1 y r)])
                               (if d
                                   (unbalanced-right t)
                                   (values t #f))))
                           
                           (match r
                             [()    (blackify l)]
                             [_     (let-values ([(r1 m d) (remove-min r)])
                                      (let ([t (B- l m r1)])
                                        (if d
                                            (unbalanced-left t)
                                            (values t #f))))])
                           
                           (let-values ([(r1 d) (remove-aux r)])
                             (let ([t (B- l y r1)])
                               (if d
                                   (unbalanced-left t)
                                   (values t #f)))))]
        [($ R l y r)  (if3 (cmp x y)
                           (let-values ([(l1 d) (remove-aux l)])
                             (let ([t (R- l1 y r)])
                               (if d
                                   (unbalanced-right t)
                                   (values t #f))))
                           (match r
                             [()   (values l #f)]
                             [_    (let-values ([(r1 m d) (remove-min r)])
                                     (let ([t (R- l m r1)])
                                       (if d
                                           (unbalanced-left t)
                                           (values t #f))))])
                           (let-values ([(r1 d) (remove-aux r)])
                             (let ([t (R- l y r1)])
                               (if d
                                   (unbalanced-left t)
                                   (values t #f)))))]))
    (let-values ([(s1 ignore) (remove-aux s)])
      s1))
  
  (define (remove* cmp xs s)
    (list:foldl (lambda (x acc) (remove cmp x acc)) s xs))
  
  
  ;;  THE SORTED LIST OF ELEMENTS
  
  (define (elements s)
    (define (elements-aux s accu)
      (match s
        [()           accu]
        [($ B l v r)  (elements-aux l (cons v (elements-aux r accu)))]
        [($ R l v r)  (elements-aux l (cons v (elements-aux r accu)))]))
    (elements-aux s '()))
  
  ;  (*s The functions [union], [inter], [diff] and [compare] are implemented
  ;      over the lists of elements. So we need first a function to build a
  ;      set from a list. *)
  ;
  ;  (*s Building a red-black tree from a sorted list in reverse order.
  ;      The result is a complete binary tree, where all nodes are black, 
  ;      except the bottom line which is red.  *)
  
  (define (log2 n)
    (floor (/ (log (exact->inexact n))
              (log 2.0))))
  
  (define (of-list sl)  ; sl = sorted list in reverse order
    ; build tree with n nodes on level k ?
    (define (build sl n k)
      (cond
        [(= k 0)  (cond
                    [(= n 0) (cons '() sl)]
                    [else    (match sl
                               [()       (error)]
                               [(x . sl) (cons (R- empty x empty) sl)])])]
        [else     (let ([n1 (quotient (- n 1) 2)])
                    (match (build sl n1 (- k 1))
                      [(_ . ())       (error)]
                      [(l . (x . sl)) (match-let ([(r . sl) (build sl (sub1 (- n n1)) (- k 1))])
                                        (cons (B- r x l) sl))]))]))
    (let ([n (length sl)])
      (if (= n 0)
          empty ; TODO TODO TOD error
          (car (build sl n (log2 n))))))
  
  
  ;  (*s Merges two sorted lists, into a sorted list in reverse order *)
  
  (define (list:member? cmp x l)
    (and (not (null? l))
         (or (=? cmp x (car l)))
         (not (<? x (car l)))
         (list:member? cmp l x)))
  
  (define (reverse-append l1 l2)
    (cond
      [(null? l1)  l2]
      [else        (reverse-append (cdr l1) (cons (car l1) l2))]))
  
  ;;; UNION
  
  (define (union-list cmp l1 l2)
    (define (merge-aux acc l1 l2)
      (cond
        [(null? l1) (reverse-append l2 acc)]
        [(null? l2) (reverse-append l1 acc)]
        [else       (if3 (cmp (car l1) (car l2))
                         (merge-aux (cons (car l1) acc) (cdr l1) l2)
                         (merge-aux acc (cdr l1) l2)
                         (merge-aux (cons (car l2) acc) l1 (cdr l2)))]))
    (merge-aux '() l1 l2))
  
  (define (union cmp s1 s2)
    (of-list (union-list cmp (elements s1) (elements s2))))

  (define (union-list/combiner cmp l1 l2 combine)
    (define (merge-aux acc l1 l2)
      (cond
        [(null? l1) (reverse-append l2 acc)]
        [(null? l2) (reverse-append l1 acc)]
        [else       (if3 (cmp (car l1) (car l2))
                         (merge-aux (cons (car l1) acc) (cdr l1) l2)
                         (cons (combine (car l1) (car l2)) 
                               (merge-aux acc (cdr l1) (cdr l2)))
                         (merge-aux (cons (car l2) acc) l1 (cdr l2)))]))
    (merge-aux '() l1 l2))
  
  (define (union/combiner cmp s1 s2 combine)
    (of-list (union-list/combiner cmp (elements s1) (elements s2) combine)))
  
  ;  (*s Intersects two sorted lists, into a sorted list in reverse order *)
  
  (define (inter-list cmp l1 l2)
    (define (inter-aux acc l1 l2)
      (cond
        [(null? l1)  acc]
        [(null? l2)  acc]
        [else        (if3 (cmp (car l1) (car l2))
                          (inter-aux acc (cdr l1) l2)
                          (inter-aux (cons (car l1) acc) (cdr l1) (cdr l2))
                          (inter-aux acc l1 (cdr l2)))]))
    (inter-aux '() l1 l2))
  
  (define (intersection cmp s1 s2)
    (of-list (inter-list cmp (elements s1) (elements s2))))

  (define (list-intersection/combiner cmp xs ys combine)
    ; xs and ys are cmp-sorted
    (cond
      [(null? xs) '()]
      [(null? ys) '()]
      [else       (let ([x (car xs)] [y (car ys)])
                    (if3 (cmp x y)
                         (list-intersection/combiner cmp (cdr xs) ys combine)
                         (cons (combine x y) (list-intersection/combiner cmp (cdr xs) (cdr ys) combine))
                         (list-intersection/combiner cmp xs (cdr ys) combine)))]))
  
  (define (intersection/combiner cmp s1 s2 combine)
    (of-list (list-intersection/combiner cmp (elements s1) (elements s2) combine)))

  
  ;  (*s Difference of two sorted lists, into a sorted list in reverse order *)
  
  (define (diff-list cmp l1 l2)
    (define (diff-aux acc l1 l2)
      (cond
        [(null? l1)  acc]
        [(null? l2)  (reverse-append l1 acc)]
        [else        (if3 (cmp (car l1) (car l2))
                          (diff-aux (cons (car l1) acc) (cdr l1) l2)
                          (diff-aux acc (cdr l1) (cdr l2))
                          (diff-aux acc l1 (cdr l2)))]))
    (diff-aux '() l1 l2))
  
  (define (difference cmp s1 s2)
    (of-list (diff-list cmp (elements s1) (elements s2))))
  
  ;  (*s Comparison. *)

  (define (equal=? cmp s1 s2)
    (and (= (size s1) (size s2))
         (subset? cmp s1 s2)    
         (subset? cmp s2 s1)))
  
  ;  (*s Subset. Copied from Ocaml's sets *)
  
  (define (subset? cmp s1 s2)
    ; NOTE: All the actions are the same.
    ;       one could use an or-pattern 
    (cond
      [(null? s1) #t]
      [(null? s2) #f]
      [else       (let ([t2 s2])
                    (match s1
                      [($ B l1 v1 r1)  (match s2
                                         [($ B l2 v2 r2) (if3 (cmp v1 v2)
                                                           (and (subset? cmp (B- l1 v1 empty) l2)
                                                                (subset? cmp r1 t2))
                                                           (and (subset? cmp l1 l2)
                                                                (subset? cmp r1 r2))
                                                           (and (subset? cmp (B- empty v1 r1) r2)
                                                                (subset? cmp l1 t2)))]
                                         [($ R l2 v2 r2) (if3 (cmp v1 v2) 
                                                           (and (subset? cmp (B- l1 v1 empty) l2)
                                                                (subset? cmp r1 t2))
                                                           (and (subset? cmp l1 l2)
                                                                (subset? cmp r1 r2))
                                                           (and (subset? cmp (B- empty v1 r1) r2)
                                                                (subset? cmp l1 t2)))])]
                      [($ R l1 v1 r1)  (match s2
                                         [($ B l2 v2 r2) (if3 (cmp v1 v2)
                                                              (and (subset? cmp (B- l1 v1 empty) l2)
                                                                   (subset? cmp r1 t2))
                                                              (and (subset? cmp l1 l2)
                                                                   (subset? cmp r1 r2))
                                                              (and (subset? cmp (B- empty v1 r1) r2)
                                                                   (subset? cmp l1 t2)))]
                                         [($ R l2 v2 r2) (if3 (cmp v1 v2)
                                                            (and (subset? cmp (B- l1 v1 empty) l2)
                                                                 (subset? cmp r1 t2))
                                                            (and (subset? cmp l1 l2)
                                                                 (subset? cmp r1 r2))
                                                            (and (subset? cmp (B- empty v1 r1) r2)
                                                                 (subset? cmp l1 t2)))])]))]))


  
  ;  (*s Other functions *)
  
  (define (all? p s)
    (match s
      [()           #t]
      [($ B l v r)  (and (p v) (all? p l) (all? p r))]
      [($ R l v r)  (and (p v) (all? p l) (all? p r))]))
  
  (define (exists? p s)
    (match s
      [()           #f]
      [($ B l v r)  (or (p v) (exists? p l) (exists? p r))]
      [($ R l v r)  (or (p v) (exists? p l) (exists? p r))]))
  
  (define (filter cmp p s)
    (define (filt acc s)
      (match s
        [()           acc]
        [($ B l v r)  (filt (filt (if (p v) (insert cmp v acc) acc) l) r)]
        [($ R l v r)  (filt (filt (if (p v) (insert cmp v acc) acc) l) r)]))
    (filt empty s))
  
  ;  let partition p s =
  ;    let rec part (t, f as accu) = function
  ;      | Empty -> accu
  ;      | Black (l, v, r) | Red (l, v, r) ->
  ;	  part (part (if p v then (add v t, f) else (t, add v f)) l) r 
  ;    in
  ;    part (Empty, Empty) s
  
  
  (define (size s)
    (match s
      [()          0]
      [($ B l _ r)  (+ (size l) 1 (size r))]
      [($ R l _ r)  (+ (size l) 1 (size r))]))

  (define (minimum s)
    (match s
      [()            (error "min: No minimum element in an empty set.")]
      [($ B () v _)  v]
      [($ R () v _)  v]
      [($ B l _ _)   (minimum l)]
      [($ R l _ _)   (minimum l)]))
  
  (define (maximum s)
    (match s
      [()            (error "max: No maximum element in an empty set.")]
      [($ B _ v ())  v]
      [($ R _ v ())  v]
      [($ B _ _ r)   (maximum r)]
      [($ R _ _ r)   (maximum r)]))
  
  (define (select s)
    (minimum s))
  
  (define (:for-each f s)
    (match s
      [()   ()]
      [($ B l v r) (begin
                     (:for-each f l)
                     (f v)
                     (:for-each f r))]
      [($ R l v r) (begin
                     (:for-each f l)
                     (f v)
                     (:for-each f r))]))
  
  (define (fold-right f acc s)
    (match s 
      [()  acc]
      [($ B l v r)  (fold-right f (f v (fold-right f acc r)) l)]
      [($ R l v r)  (fold-right f (f v (fold-right f acc r)) l)]))
  
  (define (fold-left f acc s)
    (match s 
      [()  acc]
      [($ B l v r)  (fold-left f (f v (fold-left f acc l)) r)]
      [($ R l v r)  (fold-left f (f v (fold-left f acc l)) r)]))
  
  (define (fold f b s)
    (fold-left f b s))
  
  (define (set? o)
    (or (empty? o)
        (B? o)
        (R? o)))

  )
