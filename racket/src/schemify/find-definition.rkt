#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "struct-type-info.rkt"
         "optimize.rkt"
         "infer-known.rkt")

(provide find-definitions)

;; Record top-level functions and structure types, and returns
;;  (values knowns struct-type-info-or-#f)
(define (find-definitions v prim-knowns knowns imports mutated optimize?)
  (match v
    [`(define-values (,id) ,orig-rhs)
     (define rhs (if optimize?
                     (optimize orig-rhs prim-knowns knowns imports mutated)
                     orig-rhs))
     (values
      (let ([k (infer-known rhs v #t id knowns prim-knowns imports mutated)])
        (if k
            (hash-set knowns (unwrap id) k)
            knowns))
      #f)]
    [`(define-values (,struct:s ,make-s ,s? ,acc/muts ...) ; pattern from `struct` or `define-struct`
       (let-values (((,struct: ,make ,? ,-ref ,-set!) ,rhs))
         (values ,struct:2
                 ,make2
                 ,?2
                 ,make-acc/muts ...)))
     (define info (and (wrap-eq? struct: struct:2)
                       (wrap-eq? make make2)
                       (wrap-eq? ? ?2)
                       (make-struct-type-info rhs prim-knowns knowns imports mutated)))
     (cond
      [info
       (define type (gensym (symbol->string (unwrap make-s))))
       (let* ([knowns (hash-set knowns
                                (unwrap make-s)
                                (if (struct-type-info-pure-constructor? info)
                                    (known-constructor (arithmetic-shift 1 (struct-type-info-field-count info)) type)
                                    a-known-constant))]
              [knowns (hash-set knowns
                                (unwrap s?)
                                (known-predicate 2 type))]
              [knowns
               (for/fold ([knowns knowns]) ([id (in-list acc/muts)]
                                            [maker (in-list make-acc/muts)])
                 (cond
                  [(wrap-eq? (wrap-car maker) -ref)
                   (hash-set knowns (unwrap id) (known-accessor 2 type))]
                  [else
                   (hash-set knowns (unwrap id) (known-mutator 4 type))]))])
         (values (hash-set knowns (unwrap struct:s) (known-struct-type type
                                                                       (struct-type-info-field-count info)
                                                                       (struct-type-info-pure-constructor? info)))
                 info))]
      [else (values knowns #f)])]
    [`(define-values (,struct:s ,make-s ,s? ,s-ref ,s-set!) ,rhs) ; direct use of `make-struct-type`
     (define info (make-struct-type-info rhs prim-knowns knowns imports mutated))
     (cond
      [info
       (define type (gensym (symbol->string (unwrap make-s))))
       (values
        (let* ([knowns (hash-set knowns
                                 (unwrap make-s)
                                 (if (struct-type-info-pure-constructor? info)
                                     (known-constructor (arithmetic-shift 1 (struct-type-info-field-count info)) type)
                                     a-known-constant))]
               [knowns (hash-set knowns
                                 (unwrap s?)
                                 (known-predicate 2 type))])
          ;; For now, we don't try to track the position-consuming accessor or mutator
          (hash-set knowns (unwrap struct:s) (known-struct-type type
                                                                (struct-type-info-field-count info)
                                                                (struct-type-info-pure-constructor? info))))
        info)]
      [else (values knowns #f)])]
    [`(define-values (,prop:s ,s? ,s-ref)
       (make-struct-type-property ,_ . ,rest))
     (define type (gensym (symbol->string prop:s)))
     (values
      (let* ([knowns (hash-set knowns (unwrap s-ref) (known-accessor 2 type))]
             [knowns (hash-set knowns (unwrap s?) (known-predicate 2 type))])
        ;; Check whether the property type has an immediate (or no) guard:
        (cond
         [(or (null? (unwrap rest))
              (and (not (wrap-car rest))
                   (null? (unwrap (wrap-cdr rest)))))
          (hash-set knowns (unwrap prop:s) (known-struct-type-property/immediate-guard))]
         [else knowns]))
      #f)]
    [`,_ (values knowns #f)]))
