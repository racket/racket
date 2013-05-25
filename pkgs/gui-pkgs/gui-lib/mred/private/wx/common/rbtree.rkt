#lang racket/base

;;; red-black-tree.rkt -- Jens Axel S�gaard and Carl Eastlund -- 3rd nov 2003

;;; PURPOSE

; This is an implementation of red/black trees, based on the galore.plt code

;;; HISTORY

; This is direct port of Jean-Christophe Filliatre's implementation
; of red-black trees in Ocaml.

;; 13th jan 2010 [mflatt]
;   - simplified for incorporation into MrEd;
;     something like this should be in `scheme', instead.
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
;  5th apr 2006 [cce]
;   - copied from module to class approach
;   - renamed to red-black-tree (from raw-red-black-tree-set)
;   - inlined the provide declaration
;   - fixed errors in the commented contracts for empty and get
;  2nd may 2006 [sstrickl]
;   - fixed error in insert/combiner (replacing a black node turned it red)
;  5th may 2006 [cce]
;   - udpated license statement regarding permission to use LGPL v2.1

;;; LICENSE

;  Rbset: Sets implemented as red-black trees.
;  Copyright (C) 2000 Jean-Christophe FILLIATRE
; 
;  This software is free software; you can redistribute it and/or
;  modify it under the terms of the GNU Library General Public
;  License version 2, as published by the Free Software Foundation.
;
;  5th May 2006: Jean-Christophe Filliatre has given express written
;  permission to redistribute and/or modify this software under the terms
;  of any newer version of the GNU LGPL.
; 
;  This software is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
; 
;  See the GNU Library General Public License version 2 for more details
;  (enclosed in the file LGPL).
;

;; SETS IMPLEMENTED AS REB-BLACK TREES.

(require racket/match
         (for-syntax racket/base))
(define-match-expander $
  (lambda (stx)
    (syntax-case stx ()
      [(_ id pat ...) #'(struct id (pat ...))])))

(define-syntax-rule (if3 v less same more)
  (let ([x v])
    (cond
     [(x . < . 0) less]
     [(x . = . 0) same]
     [else more])))

(provide rbtree-get                   ; compare element set -> element/f
         rbtree-insert                ; compare element set -> set
         rbtree-remove                ; compare element set -> set
         rbtree-min                   ; set -> element
         )
  
  
;; DATA DEFINITION
  
;; A RED/BLACK TREE is either
;;     1.  empty
;; or  2.  (make-B l x r)    
;; or  3.  (make-R l x r)
;; where l and r are red/black trees and x is an element.

(define empty '())                         ; considered black
(define empty? null?)
  
(define-struct B (l x r) #:transparent)   ; Black tree
(define-struct R (l x r) #:transparent)   ; Red tree
;; Constructor shorthands
(define (B- l x r) (make-B l x r))
(define (R- l x r) (make-R l x r))
  
;; type predicate
(define (red-black-tree? s)
  (or (null? s) (B? s) (R? s)))

 
;; for debugging
(define (->sexp t)
  (define -> ->sexp)
  (match t
    ['() '()]
    [($ B l x r) `(B ,(-> l) ,x ,(-> r))]
    [($ R l x r) `(R ,(-> l) ,x ,(-> r))]))

  
;; INVARIANTS
  
;;  (* Invariants: (1) a red node has no red son, and (2) any path from the
;;     root to a leaf has the same number of black nodes *)
;;
;;  (* Note the use of two constructors [Black] and [Red] to save space
;;     (resulting in longer code at a few places, e.g. in function [remove]).
;;     These red-black trees saves 20\% of space w.r.t Ocaml's AVL, which
;;     store the height into a fourth argument. *)

;;  type elt = Ord.t
;;  type t = Empty | Black of t * elt * t | Red of t * elt * t

;;  (*s For debug only: checks whether a tree is properly colored *)

;; check : rbt -> integer
;;   checks invariants and return black height,
;;   if the invariants are fulfilled
#;
(define (check s)
  (match s
    ['()    0]
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
  
;; SET OPERATIONS
  
(define (rbtree-get cmp x s)
  (match s
    ['()            #f]
    [($ B l v r)   (if3 (cmp x v)
                        (rbtree-get cmp x l)
                        v
                        (rbtree-get cmp x r))]
    [($ R l v r)   (if3 (cmp x v)
                        (rbtree-get cmp x l)
                        v
                        (rbtree-get cmp x r))]))
  
(define (rbtree-min s)
  (match s
    [($ B '() v _) v]
    [($ R '() v _) v]
    [($ B l _ _)  (rbtree-min l)]
    [($ R l _ _)  (rbtree-min l)]
    ['()          (error 'rbtree-min "an empty set does not have an mimimum element")]))

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
  
(define (rbtree-insert cmp x s)
  (define (ins s)
    (match s
      ['()          (R- empty x empty)]
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
      ['()         (error)])))
  
;; REMOVAL
  
;;  (* [unbalanced_left] repares invariant (2) when the black height of the
;;     left son exceeds (by 1) the black height of the right son *)
;; [original spelling kept -- a quote is a quote ]
  
(define (unbalanced-left s)
  (match s
    [($ R ($ B t1 x1 t2) x2 t3)              (values (lbalance (R- t1 x1 t2) x2 t3) #f)]
    [($ B ($ B t1 x1 t2) x2 t3)              (values (lbalance (R- t1 x1 t2) x2 t3) #t)]
    [($ B ($ R t1 x1 ($ B t2 x2 t3)) x3 t4)  (values (B- t1 x1 (lbalance (R- t2 x2 t3) x3 t4)) #f)]
    [_                                       (error 'unbalanced-left
                                                    (format "Black height of both sons were the same: ~a"
                                                            (->sexp s)))]))
  
;;  (* [unbalanced_right] repares invariant (2) when the black height of the
;;     right son exceeds (by 1) the black height of the left son *)

(define (unbalanced-right s)
  (match s
    [($ R t1 x1 ($ B t2 x2 t3))             (values (rbalance t1 x1 (R- t2 x2 t3)) #f)]
    [($ B t1 x1 ($ B t2 x2 t3))             (values (rbalance t1 x1 (R- t2 x2 t3)) #t)]
    [($ B t1 x1 ($ R ($ B t2 x2 t3) x3 t4)) (values (B- (rbalance t1 x1 (R- t2 x2 t3)) x3 t4) #f)]
    [_                                      (error 'unbalanced-right 
                                                   (format "Black height of both sons were the same: ~a"
                                                           (->sexp s)))]))
  
  

;;  (* [remove_min s = (s',m,b)] extracts the minimum [m] of [s], [s'] being the
;;     resulting set, and indicates with [b] whether the black height has
;;     decreased *)

(define (remove-min s)
  (match s
    ['()                         (error "remove-min: Called on empty set")]
    ;;  minimum is reached
    [($ B '() x '())           (values empty x #t)]
    [($ B '() x ($ R l y r))  (values (B- l y r) x #f)]
    [($ B '() _ ($ B _ _ _))  (error)]
    [($ R '() x r)            (values r x #f)]
    ;; minimum is recursively extracted from [l]
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

;;  (* [remove_aux x s = (s',b)] removes [x] from [s] and indicates with [b]
;;     whether the black height has decreased *)
  
(define (rbtree-remove cmp x s)
  (define (remove-aux s)
    (match s
      ['()          (values empty #f)]
      [($ B l y r)  (if3 (cmp x y)
                         (let-values ([(l1 d) (remove-aux l)])
                           (let ([t (B- l1 y r)])
                             (if d
                                 (unbalanced-right t)
                                 (values t #f))))
                         
                         (match r
                           ['()   (blackify l)]
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
                           ['()  (values l #f)]
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
