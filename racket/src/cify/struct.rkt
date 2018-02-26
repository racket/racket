#lang racket/base
(require "match.rkt"
         "ref.rkt")

(provide extract-structs
         (struct-out struct-info)
         (struct-out struct-constructor)
         (struct-out struct-predicate)
         (struct-out struct-accessor)
         (struct-out struct-mutator)
         (struct-out struct-property-accessor))

(struct struct-info (struct-id field-count authentic? pure-constructor?))
(struct struct-constructor (si))
(struct struct-predicate (si))
(struct struct-accessor (si pos))
(struct struct-mutator (si pos))

(struct struct-property-accessor (property-id))

(define (extract-structs e)
  (let loop ([e e] [knowns #hasheq()])
    (match e
      [`(define-values (,struct:s ,make-s ,s? ,acc/muts ...)
          (call-with-values
           ,mk
           (lambda (,struct: ,make ,?1 ,-ref ,-set!)
             (values ,struct:2
                     ,make2
                     ,?2
                     ,make-acc/muts ...))))
       (parse-make-struct-type
        mk
        (lambda (name-expr parent-expr fields rest)
          (define name (and (symbol? name-expr)
                            (hash-ref knowns name-expr #f)))
          (define parent (and (symbol? parent-expr)
                              (hash-ref knowns parent-expr #f)))
          (define (includes-property? name)
            (and (pair? rest)
                 (match (car rest)
                   [`(list (cons ,props ,vals) ...)
                    (and (memq name props) #t)]
                   [`,_ #f])))
          (cond
            [(and name
                  (or (not parent-expr) (struct-info? parent))
                  (exact-nonnegative-integer? fields)
                  (= (length acc/muts) (length make-acc/muts)))
             (define parent-field-count (if parent (struct-info-field-count parent) 0))
             (define pure-constructor?
               ;; pure if no guard and no prop:chaperone-unsafe-undefined
               (and (or (not parent)
                        (struct-info-pure-constructor? parent))
                    (or ((length rest) . < . 5)
                        (not (list-ref rest 4)))
                    (not (includes-property? 'prop:chaperone-unsafe-undefined))))
             (define si (struct-info
                         struct:s
                         (+ fields parent-field-count)
                         (includes-property? 'prop:authentic)
                         pure-constructor?))
             (let* ([knowns (hash-set knowns struct:s si)]
                    [knowns (hash-set knowns make-s (struct-constructor si))]
                    [knowns (hash-set knowns s? (struct-predicate si))])
               (for/fold ([knowns knowns]) ([acc/mut (in-list acc/muts)]
                                            [make-acc/mut (in-list make-acc/muts)])
                 (match make-acc/mut
                   [`(make-struct-field-accessor ,id ,pos . ,_)
                    (if (eq? (unref id) -ref)
                        (hash-set knowns acc/mut (struct-accessor si (+ pos parent-field-count)))
                        knowns)]
                   [`(make-struct-field-mutator ,id ,pos . ,_)
                    (if (eq? (unref id) -set!)
                        (hash-set knowns acc/mut (struct-mutator si (+ pos parent-field-count)))
                        knowns)]
                   [`,_ knowns])))]
            [else knowns]))
        (lambda () knowns))]
      [`(define-values (,prop:p ,p? ,p-ref)
          (make-struct-type-property ,name))
       (hash-set knowns p-ref (struct-property-accessor prop:p))]
      [`(define ,id ,rhs)
       (match rhs
         [`(quote ,s)
          (cond
            [(symbol? s) (hash-set knowns id s)]
            [else (hash-set knowns id '#:non-procedure)])]
         [`(hash . ,_) (hash-set knowns id '#:non-procedure)]
         [`(hasheq . ,_) (hash-set knowns id '#:non-procedure)]
         [`(hasheqv . ,_) (hash-set knowns id '#:non-procedure)]
         [`(gensym . ,_) (hash-set knowns id '#:non-procedure)]
         [`,_ knowns])]
      [`(begin . ,es)
       (for/fold ([knowns knowns]) ([e (in-list es)])
         (loop e knowns))]
      [`,_ knowns])))

(define (parse-make-struct-type e success-k fail-k)
  (match e
    [`(lambda ()
        (make-struct-type ,name-expr ,parent-expr ,fields 0 #f . ,rest))
     (success-k name-expr parent-expr fields rest)]
    [`(letrec ,_ ,e)
     (parse-make-struct-type e success-k fail-k)]
    [`(let ,_ ,e)
     (parse-make-struct-type e success-k fail-k)]
    [`,_ (fail-k)]))
    
