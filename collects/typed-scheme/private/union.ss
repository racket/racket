#lang scheme/base

(require "type-rep.ss" "subtype.ss" "tc-utils.ss"
         "type-effect-printer.ss" "rep-utils.ss"
         "type-comparison.ss"
         scheme/match mzlib/trace)

(provide Un #;(rename *Un Un))

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
    [args
     ;; a is a Type (not a union type)
     ;; b is a List[Type]
     (define (union2 a b)     
       (define b* (make-union* b))
       (cond 
         [(subtype a b*) (list b*)]
         [(subtype b* a) (list a)]            
         [else (cons a b)]))
     #;(union-count!)    
     (let ([types (remove-dups (sort (apply append (map flat args)) type<?))])
       (cond
         [(null? types) (make-union* null)]
         [(null? (cdr types)) (car types)]           
         [(ormap Values? types)
          (if (andmap Values? types)
              (make-Values (apply map Un (map Values-types types)))
              (int-err "Un: should not take the union of multiple values with some other type: ~a" types))]
         [else (make-union* #;(remove-subtypes types) (foldr union2 null (remove-subtypes types)))]))]))

#;(defintern (Un-intern args) (lambda (_ args) (apply Un args)) args)

#;(define (*Un . args) (Un-intern args))

;(trace Un)

(define (u-maker args) (apply Un args))

;(trace u-maker)
(set-union-maker! u-maker)

