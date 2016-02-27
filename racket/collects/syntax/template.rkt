#lang racket/base
(require "stx.rkt"
         racket/struct
         (for-template racket/base
                       "private/template-runtime.rkt"))

(provide transform-template)

;; A template map descibres the structure of a template
;; in terms of where pattern variables are replaced.
;;
;; Walk a map and a template in parallel, and you see
;; these map cases:
;;
;;  - #f => corresponding template portion is constant
;;  - #t => corresponding template portion is a pattern variable
;;  - (cons map1 map2) => template part is a pair
;;                        which substitutions in one side
;;                        or the other
;; - (vector map) => template portion is a vector,
;;                   contents like the list in map
;; - (box map) => template portion is a box with substitution
;; - #s(ellipses elem count map) => template portion is an ellipses-generated list
;; - #s(ellipses-quote map) => template has a quoting ellipses
;; - #s(prefab v map) => template portion is a prefab

(define-struct ellipses (elem count rest) #:prefab #:omit-define-syntaxes)
(define-struct ellipses-quote (rest) #:prefab #:omit-define-syntaxes)
(define-struct prefab (key fields) #:prefab #:omit-define-syntaxes)

(define (datum->syntax* stx d)
  (datum->syntax stx d stx stx stx))

(define (make-template-map tmpl const-leaf?)
  (let loop ([tmpl tmpl]
             [in-ellipses? #f])
    (syntax-case tmpl ()
      [(ellipses expr)
       (and (not in-ellipses?)
            (identifier? #'ellipses)
            (free-identifier=? #'ellipses #'(... ...)))
       (make-ellipses-quote (loop #'expr #t))]
      [(expr ellipses . rest)
       (and (not in-ellipses?)
            (identifier? #'ellipses)
            (free-identifier=? #'ellipses #'(... ...)))
       (let-values ([(elem) (loop #'expr #f)]
                    [(rest count)
                     (let rloop ([rest #'rest][count 1])
                       (syntax-case rest ()
                         [(ellipses . rest)
                          (and (identifier? #'ellipses)
                               (free-identifier=? #'ellipses #'(... ...)))
                          ;; keep going:
                          (rloop #'rest (add1 count))]
                         [else (values (loop rest #f) count)]))])
         (make-ellipses elem count rest))]
      [(a . b) (let ([a (loop #'a in-ellipses?)]
                     [b (loop #'b in-ellipses?)])
                 (and (or a b (not const-leaf?))
                      (cons a b)))]
      [#(a ...) (let ([as (loop (syntax->list #'(a ...))
                                in-ellipses?)])
                  (and (or as (not const-leaf?))
                       (vector as)))]
      [#&(a) (let ([as (loop #'a in-ellipses?)])
               (and (or as (not const-leaf?))
                    (box as)))]
      [a
       (identifier? #'a)
       (syntax-pattern-variable? (syntax-local-value #'a (lambda () #f)))]
      [_ 
       (let ([k (prefab-struct-key (syntax-e tmpl))])
         (and k
              (let ([as (loop (struct->list (syntax-e tmpl)) in-ellipses?)])
                (and (or as (not const-leaf?))
                     (make-prefab k as)))))])))

(define (template-map-collect tmap template s->d leaf->d pvar->d)
  (let loop ([tmap tmap][template template])
    (cond
     [(not tmap) (if (syntax? template)
                     (box (leaf->d template))
                     #f)]
     [(eq? tmap #t) (pvar->d template)]
     [(pair? tmap)
      (if (syntax? template)
          (vector (s->d template)
                  (loop (car tmap) (stx-car template))
                  (loop (cdr tmap) (stx-cdr template)))
          (cons (loop (car tmap) (stx-car template))
                (loop (cdr tmap) (stx-cdr template))))]
     [(vector? tmap)
      (cons (s->d template)
            (loop (vector-ref tmap 0)
                  (vector->list (syntax-e template))))]
     [(box? tmap)
      (cons (s->d template)
            (loop (unbox tmap)
                  (syntax-e template)))]
     [(ellipses? tmap)
      (let ([rest (let loop ([rest (stx-cdr template)]
                             [count (ellipses-count tmap)])
                    (if (zero? count)
                        rest
                        (loop (stx-cdr rest) (sub1 count))))])
        (if (syntax? template)
            (vector (s->d template)
                    (loop (ellipses-elem tmap) (stx-car template))
                    (loop (ellipses-rest tmap) rest))
            (cons (loop (ellipses-elem tmap) (stx-car template))
                  (loop (ellipses-rest tmap) rest))))]
     [(ellipses-quote? tmap)
      (loop (ellipses-quote-rest tmap) (stx-car (stx-cdr template)))]
     [(prefab? tmap)
      (cons (s->d template)
            (loop (prefab-fields tmap)
                  (struct->list (syntax-e template))))]
     [else (error "template-map-collect fall-through")])))

(define (group-ellipses tmap template)
  (let loop ([tmap tmap][template template])
    (cond
     [(boolean? tmap) template]
     [(pair? tmap)
      (let ([p (cons (loop (car tmap) (stx-car template))
                     (loop (cdr tmap) (stx-cdr template)))])
        (if (syntax? template)
            (datum->syntax* template p)
            p))]
     [(vector? tmap)
      (datum->syntax* template
                      (list->vector
                       (loop (vector-ref tmap 0)
                             (vector->list (syntax-e template)))))]
     [(box? tmap)
      (datum->syntax* template
                      (box
                       (loop (unbox tmap)
                             (syntax-e template))))]
     [(ellipses? tmap)
      (let ([rest 
             (loop (ellipses-rest tmap)
                   (let loop ([rest (stx-cdr template)]
                              [count (ellipses-count tmap)])
                     (if (zero? count)
                         rest
                         (loop (stx-cdr rest) (sub1 count)))))]
            [elem (loop (ellipses-elem tmap) (stx-car template))])
        (let ([new `((,elem ,@(for/list ([i (in-range (ellipses-count tmap))])
                                #'(... ...)))
                     . ,rest)])
          (if (syntax? template)
              (datum->syntax* template new)
              new)))]
     [(ellipses-quote? tmap)
      (datum->syntax* template
                      (list (stx-car template)
                           (loop (ellipses-quote-rest tmap) (stx-car (stx-cdr template)))))]
     [(prefab? tmap)
      (datum->syntax* 
       template
       (apply
        make-prefab-struct
        (prefab-key tmap)
        (loop (prefab-fields tmap)
              (struct->list (syntax-e template)))))]
     [else (error "group-ellipses fall-through")])))

(define (transform-template template-stx
                            #:save s->d
                            #:restore-stx d->s
                            #:leaf-save [leaf->d s->d]
                            #:leaf-restore-stx [leaf->s #'(lambda (data stx) stx)]
                            #:leaf-datum-stx [leaf-datum #'values]
                            #:pvar-save [pvar->d (lambda (x) #f)]
                            #:pvar-restore-stx [pvar->s #'(lambda (d s) s)]
                            #:cons-stx [pcons cons]
                            #:ellipses-end-stx [ellipses-end #'values]
                            #:constant-as-leaf? [const-leaf? #f])
  (let* ([tmap (make-template-map template-stx const-leaf?)]
         [grouped-template
          ;; Convert tmpl to group ...-created repetitions together,
          ;;  so that `unwrap' can tell which result came from which
          ;;  template:
          (group-ellipses tmap template-stx)]
         [data (template-map-collect tmap template-stx
                                     s->d leaf->d pvar->d)])
    #`(if #f
          ;; Process tmpl first, so that syntax errors are reported
          ;; usinf the original source.
          (syntax #,template-stx)
          ;; Apply give d->s to result:
          (template-map-apply '#,tmap 
                              #,d->s #,leaf->s #,leaf-datum #,pvar->s #,pcons #,ellipses-end
                              '#,data
                              (syntax #,grouped-template)))))
