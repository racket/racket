#lang scheme/base

(require "../utils/utils.ss")

(require (rep type-rep)
	 (utils tc-utils)
	 (types utils subtype abbrev printer comparison)
         scheme/match mzlib/trace)

(provide Un)

(define (make-union* set)
  (match set
    [(list t) t]
    [_ (make-Union set)]))

(define empty-union (make-Union null))

(define (flat t)
  (match t
    [(Union: es) es]
    [_ (list t)]))    

(define (remove-subtypes ts)
  (let loop ([ts* ts] [result '()])
    (cond [(null? ts*) (reverse result)]
          [(ormap (lambda (t) (subtype (car ts*) t)) result) (loop (cdr ts*) result)]
          [else (loop (cdr ts*) (cons (car ts*) result))])))

(define Un
  (case-lambda 
    [() empty-union]
    [(t) t]
    [args
     ;; a is a Type (not a union type)
     ;; b is a List[Type]
     (define (union2 a b)     
       (define b* (make-union* b))
       (cond 
         [(subtype a b*) (list b*)]
         [(subtype b* a) (list a)]            
         [else (cons a b)]))
     (let ([types (remove-dups (sort (apply append (map flat args)) type<?))])
       (cond
         [(null? types) (make-union* null)]
         [(null? (cdr types)) (car types)]           
         [else (make-union* (foldr union2 '() (remove-subtypes types)))]))]))

(define (u-maker args) (apply Un args))

(set-union-maker! u-maker)

