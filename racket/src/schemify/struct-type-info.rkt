#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "mutated-state.rkt"
         "simple.rkt"
         "find-known.rkt")

(provide (struct-out struct-type-info)
         struct-type-info-rest-properties-list-pos
         make-struct-type-info
         pure-properties-list?)

(struct struct-type-info (name parent
                               immediate-field-count
                               field-count
                               pure-constructor?
                               authentic?
                               prefab-immutables ; #f or immutable expression to be quoted
                               rest)) ; argument expressions after auto-field value
(define struct-type-info-rest-properties-list-pos 0)

;; Parse `make-struct-type` forms, returning a `struct-type-info`
;; if the parse succeed:
(define (make-struct-type-info v prim-knowns knowns imports mutated)
  (match v
    [`(make-struct-type (quote ,name) ,parent ,fields 0 #f . ,rest)
     ;; Note: auto-field count must be zero, because a non-zero count involves
     ;; an arity-reduced procedure
     (let ([u-name (unwrap name)]
           [u-parent (let ([u-parent (unwrap parent)])
                       (or (extract-struct-typed-from-checked u-parent)
                           u-parent))])
       (and (symbol? u-name)
            (or (not u-parent)
                (known-struct-type?
                 (find-known u-parent prim-knowns knowns imports mutated)))
            (exact-nonnegative-integer? fields)
            (let ([prefab-imms
                   ;; The inspector argument needs to be missing or duplicable,
                   ;; and if it's not known to produce a value other than 'prefab,
                   ;; the list of immutables must be duplicable:
                   (match rest
                     [`() 'non-prefab]
                     [`(,_) 'non-prefab]
                     [`(,_ #f . ,_) 'non-prefab]
                     [`(,_ (current-inspector) . ,_) 'non-prefab]
                     [`(,_ 'prefab ,_ ',immutables . ,_) immutables]
                     [`(,_ 'prefab ,_) '()]
                     [`(,_ 'prefab) '()]
                     [`,_ #f])]
                  [parent-sti (and u-parent (find-known u-parent prim-knowns knowns imports mutated))])
              (define (includes-property? name)
                (and (pair? rest)
                     (match (car rest)
                       [`(list (cons ,props ,vals) ...)
                        (for/or ([prop (in-list props)])
                          (eq? (unwrap prop) name))]
                       [`,_ #f])))
              (and prefab-imms
                   (struct-type-info name
                                     parent
                                     fields
                                     (+ fields (if u-parent
                                                   (known-struct-type-field-count parent-sti)
                                                   0))
                                     ;; no guard & no prop:chaperone-unsafe-undefined => pure constructor
                                     (and (or (not u-parent)
                                              (known-struct-type-pure-constructor? parent-sti))
                                          (or ((length rest) . < . 5)
                                              (not (unwrap (list-ref rest 4))))
                                          (not (includes-property? 'prop:chaperone-unsafe-undefined)))
                                     (includes-property? 'prop:authentic)
                                     (if (eq? prefab-imms 'non-prefab)
                                         #f
                                         prefab-imms)
                                     rest)))))]
    [`(let-values () ,body)
     (make-struct-type-info body prim-knowns knowns imports mutated)]
    [`,_ #f]))

;; Check whether `e` has the shape of a property list that uses only
;; properties where the property doesn't have a guard or won't invoke
;; a guarded procedure
(define (pure-properties-list? e prim-knowns knowns imports mutated)
  (match e
    [`(list (cons ,props ,vals) ...)
     (for/and ([prop (in-list props)]
               [val (in-list vals)])
       (let ([u-prop (unwrap prop)])
         (and (symbol? u-prop)
              (or (known-struct-type-property/immediate-guard?
                   (find-known u-prop prim-knowns knowns imports mutated)))
              (simple? val prim-knowns knowns imports mutated))))]
    [`null #t]
    [`'() #t]
    [`,_ #f]))

;; Recognide
;;  (let ((<tmp> <id>))
;;     (if (struct-type? <tmp?)
;;         <tmp>
;;         ....))
;; and return <id>. This happens when `#:parent`
;; is used in `struct` instead of specifying a parent
;; name next to the struct name.
(define (extract-struct-typed-from-checked e)
  (match e
    [`(let-values ([(,tmp1) ,id])
        (if (struct-type? ,tmp2)
            ,tmp3
            ,_))
     (define u-tmp1 (unwrap tmp1))
     (and (eq? u-tmp1 (unwrap tmp2))
          (eq? u-tmp1 (unwrap tmp3))
          (let ([u (unwrap id)])
            (and (symbol? u)
                 u)))]
    [`,_ #f]))
