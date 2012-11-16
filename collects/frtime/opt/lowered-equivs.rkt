;; This module defines all the logic necessary for working with lowered
;; equivalents at the syntactic level.  That is, it treats functions simply 
;; as syntactic identifiers.
#lang racket/base

(provide (except-out (all-defined-out)
                     module-identifier=?))

(define module-identifier=? free-identifier=?)

(define lowered-equiv-suffix ":lowered-equiv")

;; Given an identifier for a normal binding, return the identifier
;; to be used for the lowered equivalent of that binding.
(define (make-lowered-equiv-id id-stx)
  (datum->syntax
   id-stx
   (string->symbol
    (format "~a~a" (syntax-e id-stx) lowered-equiv-suffix))))

;; does the given string end with the given suffix?
(define (string-ends-with str suffix)
  (string=? (substring str (max 0 (- (string-length str)
                                     (string-length suffix))))
            suffix))

;; is the given identifier a lowered equiv identifier?
(define (lowered-equiv-id? id-stx)
  (and (identifier? id-stx)
       (string-ends-with (symbol->string (syntax-e id-stx))
                         lowered-equiv-suffix)))

;; strip the lowered-equiv suffix from an identifier
(define (lowered-equiv-id->lifted-id id-stx)
  (let ([name (symbol->string (syntax-e id-stx))])
    (datum->syntax 
     id-stx
     (string->symbol
      (substring name 0 (- (string-length name)
                           (string-length lowered-equiv-suffix)))))))

;; Exception used to indicate that an expression cannot be lowered because
;; it has no lowered equivalent.  
(define-struct exn:no-lowered-equiv (reason))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equiv maps translate function names to the name of a lowered equivalent.
;; Equiv maps are represented as a list of (func . lowered-equiv) pairs.

;; empty equiv map
(define (empty-equiv-map)
  (list))

;; add a new func/lowered-equiv mapping to an equiv map (overwrites any 
;; existing mapping)
(define (add-equiv-map old-equiv-map new-func new-lowered-func)
  (cons (cons new-func new-lowered-func)
        old-equiv-map))

;; remove a func/lowered-equiv mapping from an equiv map (no effect if
;; the func isn't actually in the mapping)
(define (del-equiv-map old-equiv-map func-to-remove)
  (filter (lambda (pair)
            (not (module-identifier=? (car pair) func-to-remove)))
          old-equiv-map))

;; remove a list of funcs from an equiv map
(define (del-equiv-map* old-equiv-map ids-to-remove)
  (foldl del-equiv-map old-equiv-map ids-to-remove))

;; Returns the lowered-equiv for a function, or #f if there is none.
(define (lookup-lowered-equiv equiv-map func)
  (if (null? equiv-map)
      #f
      (if (module-identifier=? (caar equiv-map) func)
          (cdar equiv-map)
          (lookup-lowered-equiv (cdr equiv-map) func))))

;; Returns the lowered-equiv for a function, or throws exn:no-lowered-equiv.
(define (get-lowered-equiv equiv-map func)
  (let ([ret (lookup-lowered-equiv equiv-map func)])
    (if ret
        ret
        (raise (make-exn:no-lowered-equiv 
                (format "no lowered equiv for ~s" (syntax->datum func)))))))

;; convert syntax of the form ((func lowered-equiv) ...) to an equiv map
(define (stx-to-equiv-map stx)
  (syntax-case stx ()
    (() (empty-equiv-map))
    (((lifted lowered) rest ...)
     (add-equiv-map (stx-to-equiv-map #'(rest ...)) #'lifted #'lowered))))

;; convert an equiv map to syntax of the form ((func lowered-equiv) ...)
(define (equiv-map-to-stx equiv-map)
  (datum->syntax #'here
                 (map (lambda (pair) (list (car pair) (cdr pair)))
                      equiv-map)))

;; combine two equiv maps
(define (union-equiv-maps . equiv-maps)
  (apply append equiv-maps))

;; convert a list of symbols to an equiv map, by searching for symbols
;; that have a matching lowered equivalent symbol.  All other symbols
;; are ignored.
(define (symbol-list-to-equiv-map symbol-list)
  (foldl (lambda (func equiv-map)
           (if (lowered-equiv-id? func)
               (add-equiv-map
                equiv-map
                (lowered-equiv-id->lifted-id func) func)
               equiv-map))
         (empty-equiv-map) 
         symbol-list))
