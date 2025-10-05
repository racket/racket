#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "simple.rkt"
         "find-known.rkt"
         "lambda.rkt"
         "unwrap-let.rkt")

(provide (struct-out struct-type-info)
         struct-type-info-rest-properties-list-pos
         make-struct-type-info
         pure-properties-list
         add-struct-type-property-known)

(struct struct-type-info (name parent
                               immediate-field-count
                               field-count
                               pure-constructor?
                               authentic?
                               sealed?
                               prefab-immutables ; #f or immutable expression to be quoted
                               non-prefab-immutables ; #f or immutable expression to be quoted
                               constructor-name-expr  ; an expression
                               rest) ; argument expressions after auto-field value
  #:authentic)
(define struct-type-info-rest-properties-list-pos 0)

;; Parse `make-struct-type` forms, returning a `struct-type-info`
;; if the parse succeed:
(define (make-struct-type-info v prim-knowns knowns imports mutated)
  (match (unwrap-let v)
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
            ((length rest) . <= . 6)
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
              (define (handle-proc-spec proc-spec imms)
                (cond
                  [(not proc-spec) imms]
                  [(exact-nonnegative-integer? proc-spec) (cons proc-spec imms)]
                  [(lambda? proc-spec) imms]
                  [else
                   (let ([proc-spec (unwrap proc-spec)])
                     (and
                      (symbol? proc-spec)
                      (let ([k (find-known proc-spec prim-knowns knowns imports mutated)])
                        (cond
                          [(not k) #f]
                          [(known-literal? k)
                           (let ([v (known-literal-value k)])
                             (and (or (not v) (exact-nonnegative-integer? v))
                                  (handle-proc-spec v imms)))]
                          [(known-procedure? k) imms]
                          [else #f]))))]))
              (define constructor-name-expr (and ((length rest) . > . 5)
                                                 (list-ref rest 5)))
              (define non-prefab-imms
                (and (eq? prefab-imms 'non-prefab)
                     (match rest
                       [`() '()]
                       [`(,_) '()]
                       [`(,_ ,_) '()]
                       [`(,_ ,_ ,proc-spec)
                        (handle-proc-spec proc-spec '())]
                       [`(,_ ,_ ,proc-spec ',immutables . ,_)
                        (handle-proc-spec proc-spec immutables)]
                       [`,_ #f])))
              (and (if (eq? prefab-imms 'non-prefab)
                       non-prefab-imms
                       prefab-imms)
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
                                     (includes-property? 'prop:sealed)
                                     (if (eq? prefab-imms 'non-prefab)
                                         #f
                                         prefab-imms)
                                     non-prefab-imms
                                     constructor-name-expr
                                     rest)))))]
    [`,_ #f]))

;; Check the degree to which `e` has the shape of a property list,
;; and for each property--value pair, whether the property is known
;; to be one that that doesn't have a guard or won't invoke
;; a guarded procedure. If `e` has the rgith shape, the result is
;; `(list (list* <bool> <key> <val>) ...)` where the <bool> is
;; `#t` if `<key>` is known to be such a property, `#f` otherwise.
(define (pure-properties-list e prim-knowns knowns imports mutated simples)
  (match e
    [`(list (cons ,props ,vals) ...)
     (for/list ([prop (in-list props)]
                [val (in-list vals)])
       (define u-prop (unwrap prop))
       (define nice-prop?
         (and (symbol? u-prop)
              (known-struct-type-property/immediate-guard?
               (find-known u-prop prim-knowns knowns imports mutated))
              (simple? val prim-knowns knowns imports mutated simples #f
                       #:pure? #f)))
       (list* nice-prop? prop val))]
    [`null null]
    [`'() null]
    [`,_ #f]))

;; Recognize
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

(define (add-struct-type-property-known prop:s s-ref s? immediate-guard? knowns)
  (define type (string->uninterned-symbol (symbol->string (unwrap prop:s))))
  (let* ([knowns (hash-set knowns (unwrap s-ref) (known-accessor 2 type))]
         [knowns (hash-set knowns (unwrap s?) (known-predicate 2 type))])
    (if immediate-guard?
        (hash-set knowns (unwrap prop:s) (known-struct-type-property/immediate-guard))
        knowns)))
