#lang scheme/base

;; don't provide reduction-relation directly, so that we can use that for the macro's name.
(provide reduction-relation-lang
         reduction-relation-make-procs
         reduction-relation-rule-names
         reduction-relation-lws
         reduction-relation-procs
         build-reduction-relation
         reduction-relation?
         empty-reduction-relation
         make-rewrite-proc rewrite-proc? rewrite-proc-name rewrite-proc-lhs rewrite-proc-id
         (struct-out rule-pict))

(define-struct rule-pict (arrow lhs rhs label side-conditions fresh-vars pattern-binds))

;; type proc = (exp exp (any -> any) (listof any) -> (listof any)))
;;   a proc is a `cached' version of a make-proc, specialized to a particular langugage
;;   since that first application does all the work of compiling a pattern (wrt to a language),
;;   we want to avoid doing it multiple times, so it is cached in a reduction-relation struct


(define-values (make-rewrite-proc rewrite-proc? rewrite-proc-name rewrite-proc-lhs rewrite-proc-id)
  (let ()
    (define-values (type constructor predicate accessor mutator) 
      (make-struct-type 'rewrite-proc #f 4 0 #f '() #f 0))
    (values constructor 
            predicate 
            (make-struct-field-accessor accessor 1 'name)
            (make-struct-field-accessor accessor 2 'lhs)
            (make-struct-field-accessor accessor 3 'id))))

;; lang : compiled-language
;; make-procs = (listof (compiled-lang -> proc))
;; rule-names : (listof sym)
;; procs : (listof proc)
(define-struct reduction-relation (lang make-procs rule-names lws procs))

(define empty-reduction-relation (make-reduction-relation 'empty-reduction-relations-language
                                                          '()
                                                          '()
                                                          '()
                                                          '()))

;; the domain pattern isn't actually used here.
;; I started to add it, but didn't finish. -robby
(define (build-reduction-relation orig-reduction-relation lang make-procs rule-names lws domain-pattern)
  (cond
    [orig-reduction-relation
     (let* ([new-names (map rewrite-proc-name make-procs)]
            [all-make-procs
             (append (filter (λ (x) (or (not (rewrite-proc-name x))
                                        (not (member (rewrite-proc-name x) new-names))))
                             (reduction-relation-make-procs orig-reduction-relation))
                     make-procs)])
       (make-reduction-relation lang 
                                all-make-procs
                                (append (reduction-relation-rule-names orig-reduction-relation)
                                        rule-names)
                                lws ;; only keep new lws for typesetting
                                (map (λ (make-proc) (make-proc lang)) all-make-procs)))]
    [else
     (make-reduction-relation lang make-procs rule-names lws (map (λ (make-proc) (make-proc lang)) make-procs))]))
