#lang racket/base
(require "match.rkt"
         "wrap.rkt"
         "struct-type-info.rkt"
         "mutated-state.rkt"
         "find-definition.rkt")

(provide struct-convert
         struct-convert-local)

(define (struct-convert form prim-knowns knowns imports mutated
                        schemify no-prompt?)
  (match form
    [`(define-values (,struct:s ,make-s ,s? ,acc/muts ...)
        (let-values (((,struct: ,make ,?1 ,-ref ,-set!) ,mk))
          (values ,struct:2
                  ,make2
                  ,?2
                  ,make-acc/muts ...)))
     ;; Convert a `make-struct-type` binding into a 
     ;; set of bindings that Chez's cp0 recognizes,
     ;; and push the struct-specific extra work into
     ;; `struct-type-install-properties!`
     (define sti (and (wrap-eq? struct: struct:2)
                      (wrap-eq? make make2)
                      (wrap-eq? ?1 ?2)
                      (make-struct-type-info mk prim-knowns knowns imports mutated)))
     (cond
       [(and sti
             ;; make sure `struct:` isn't used too early, since we're
             ;; reordering it's definition with respect to some arguments
             ;; of `make-struct-type`:
             (simple-mutated-state? (hash-ref mutated (unwrap struct:) #f))
             ;; If any properties, need the first LHS to be non-set!ed, because that will
             ;; let us reject multi-return from continuation capture in property expressions
             (or no-prompt?
                 (null? (struct-type-info-rest sti))
                 (not (set!ed-mutated-state? (hash-ref mutated (unwrap struct:s) #f)))))
        (define can-impersonate? (not (struct-type-info-authentic? sti)))
        (define raw-s? (if can-impersonate? (gensym (unwrap s?)) s?))
        `(begin
           (define ,struct:s (make-record-type-descriptor ',(struct-type-info-name sti)
                                                          ,(schemify (struct-type-info-parent sti) knowns)
                                                          ,(if (not (struct-type-info-prefab-immutables sti))
                                                               #f
                                                               `(structure-type-lookup-prefab-uid
                                                                 ',(struct-type-info-name sti)
                                                                 ,(schemify (struct-type-info-parent sti) knowns)
                                                                 ,(struct-type-info-immediate-field-count sti)
                                                                 0 #f
                                                                 ',(struct-type-info-prefab-immutables sti)))
                                                          #f
                                                          #f
                                                          ',(for/vector ([i (in-range (struct-type-info-immediate-field-count sti))])
                                                              `(mutable ,(string->symbol (format "f~a" i))))))
           ,@(if (null? (struct-type-info-rest sti))
                 null
                 `((define ,(gensym)
                     (struct-type-install-properties! ,struct:s
                                                      ',(struct-type-info-name sti)
                                                      ,(struct-type-info-immediate-field-count sti)
                                                      0
                                                      ,(schemify (struct-type-info-parent sti) knowns)
                                                      ,@(schemify-body schemify knowns (struct-type-info-rest sti))))))
           (define ,make-s ,(let ([ctr `(record-constructor
                                         (make-record-constructor-descriptor ,struct:s #f #f))])
                              (if (struct-type-info-pure-constructor? sti)
                                  ctr
                                  `(struct-type-constructor-add-guards ,ctr ,struct:s ',(struct-type-info-name sti)))))
           (define ,raw-s? (record-predicate ,struct:s))
           ,@(if can-impersonate?
                 `((define ,s? (lambda (v) (if (,raw-s? v) #t ($value (if (impersonator? v) (,raw-s? (impersonator-val v)) #f))))))
                 null)
           ,@(for/list ([acc/mut (in-list acc/muts)]
                        [make-acc/mut (in-list make-acc/muts)])
               (define raw-acc/mut (if can-impersonate? (gensym (unwrap acc/mut)) acc/mut))
               (match make-acc/mut
                 [`(make-struct-field-accessor ,(? (lambda (v) (wrap-eq? v -ref))) ,pos ',field-name)
                  (define raw-def `(define ,raw-acc/mut (record-accessor ,struct:s ,pos)))
                  (if can-impersonate?
                      `(begin
                         ,raw-def
                         (define ,acc/mut
                           (lambda (s) (if (,raw-s? s)
                                           (,raw-acc/mut s)
                                           ($value (impersonate-ref ,raw-acc/mut ,struct:s ,pos s
                                                                    ',(struct-type-info-name sti) ',field-name))))))
                      raw-def)]
                 [`(make-struct-field-mutator ,(? (lambda (v) (wrap-eq? v -set!))) ,pos ',field-name)
                  (define raw-def `(define ,raw-acc/mut (record-mutator ,struct:s ,pos)))
                  (define abs-pos (+ pos (- (struct-type-info-field-count sti)
                                            (struct-type-info-immediate-field-count sti))))
                  (if can-impersonate?
                      `(begin
                         ,raw-def
                         (define ,acc/mut
                           (lambda (s v) (if (,raw-s? s)
                                             (,raw-acc/mut s v)
                                             ($value (impersonate-set! ,raw-acc/mut ,struct:s ,pos ,abs-pos s v
                                                                       ',(struct-type-info-name sti) ',field-name))))))
                      raw-def)]
                 [`,_ (error "oops")]))
           (define ,(gensym)
             (begin
               (register-struct-constructor! ,make-s)
               (register-struct-predicate! ,s?)
               ,@(for/list ([acc/mut (in-list acc/muts)]
                            [make-acc/mut (in-list make-acc/muts)])
                   (match make-acc/mut
                     [`(make-struct-field-accessor ,_ ,pos ,_)
                      `(register-struct-field-accessor! ,acc/mut ,struct:s ,pos)]
                     [`(make-struct-field-mutator ,_ ,pos ,_)
                      `(register-struct-field-mutator! ,acc/mut ,struct:s ,pos)]
                     [`,_ (error "oops")]))
               (void))))]
       [else #f])]
    [`,_ #f]))

(define (struct-convert-local form #:letrec? [letrec? #f]
                              prim-knowns knowns imports mutated
                              schemify
                              #:unsafe-mode? unsafe-mode?)
  (match form
    [`(,_ ([,ids ,rhs]) ,bodys ...)
     (define defn `(define-values ,ids ,rhs))
     (define new-seq
       (struct-convert defn
                       prim-knowns knowns imports mutated
                       schemify #t))
     (and new-seq
          (match new-seq
            [`(begin . ,new-seq)
             (define-values (new-knowns info)
               (find-definitions defn prim-knowns knowns imports mutated unsafe-mode?
                                 #:optimize? #f))
             (cond
               [letrec?
                `(letrec* ,(let loop ([new-seq new-seq])
                             (match new-seq
                               [`() null]
                               [`((begin ,forms ...) . ,rest)
                                (loop (append forms rest))]
                               [`((define ,id ,rhs) . ,rest)
                                (cons `[,id ,rhs] (loop rest))]))
                   ,@(schemify-body schemify new-knowns bodys))]
               [else
                (let loop ([new-seq new-seq])
                  (match new-seq
                    [`()
                     (define exprs (schemify-body schemify new-knowns bodys))
                     (if (and (pair? exprs) (null? (cdr exprs)))
                         (car exprs)
                         `(begin ,@exprs))]
                    [`((begin ,forms ...) . ,rest)
                     (loop (append forms rest))]
                    [`((define ,id ,rhs) . ,rest)
                     `(let ([,id ,rhs])
                        ,(loop rest))]))])]))]
    [`,_ #f]))

(define (schemify-body schemify knowns l)
  (for/list ([e (in-list l)])
    (schemify e knowns)))
