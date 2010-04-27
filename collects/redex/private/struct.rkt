#lang scheme/base

(require "matcher.ss")

;; don't provide reduction-relation directly, so that we can use that for the macro's name.
(provide reduction-relation-lang
         reduction-relation-make-procs
         reduction-relation-rule-names
         reduction-relation-lws
         reduction-relation-procs
         build-reduction-relation
         reduction-relation?
         empty-reduction-relation
         make-rewrite-proc rewrite-proc? rewrite-proc-name 
         rewrite-proc-lhs rewrite-proc-lhs-src rewrite-proc-id
         (struct-out rule-pict))

(define-struct rule-pict (arrow lhs rhs label side-conditions/pattern-binds fresh-vars))

;; type proc = (exp exp (any -> any) (listof any) -> (listof any)))
;;   a proc is a `cached' version of a make-proc, specialized to a particular langugage
;;   since that first application does all the work of compiling a pattern (wrt to a language),
;;   we want to avoid doing it multiple times, so it is cached in a reduction-relation struct


(define-values (make-rewrite-proc 
                rewrite-proc? 
                rewrite-proc-name rewrite-proc-lhs rewrite-proc-lhs-src rewrite-proc-id)
  (let ()
    (define-values (type constructor predicate accessor mutator) 
      (make-struct-type 'rewrite-proc #f 5 0 #f '() #f 0))
    (values constructor 
            predicate 
            (make-struct-field-accessor accessor 1 'name)
            (make-struct-field-accessor accessor 2 'lhs)
            (make-struct-field-accessor accessor 3 'lhs-src)
            (make-struct-field-accessor accessor 4 'id))))

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
  (let* ([make-procs/check-domain
          (let loop ([make-procs make-procs]
                     [i 0])
            (cond
              [(null? make-procs) null]
              [else
               (let ([make-proc (car make-procs)])
                 (cons (make-rewrite-proc
                        (λ (lang)
                          (let ([compiled-domain-pat (compile-pattern lang domain-pattern #f)]
                                [proc (make-proc lang)])
                            (λ (tl-exp exp f acc)
                              (unless (match-pattern compiled-domain-pat tl-exp)
                                (error 'reduction-relation "relation not defined for ~s" tl-exp))
                              (let ([ress (proc tl-exp exp f acc)])
                                (for-each
                                 (λ (res)
                                   (let ([term (cadr res)])
                                     (unless (match-pattern compiled-domain-pat term)
                                       (error 'reduction-relation "relation reduced to ~s via ~a, which is outside its domain"
                                              term
                                              (let ([name (rewrite-proc-name make-proc)])
                                                (if name
                                                    (format "the rule named ~a" name)
                                                    (format "rule #~a (counting from 0)" i)))))))
                                 ress)
                                ress))))
                        (rewrite-proc-name make-proc)
                        (rewrite-proc-lhs make-proc)
                        (rewrite-proc-lhs-src make-proc)
                        (rewrite-proc-id make-proc))
                       (loop (cdr make-procs)
                             (+ i 1))))]))])
    (cond
      [orig-reduction-relation
       (let* ([new-names (map rewrite-proc-name make-procs)]
              [all-make-procs
               (append
                (filter (λ (x) (or (not (rewrite-proc-name x))
                                   (not (member (rewrite-proc-name x) new-names))))
                        (reduction-relation-make-procs orig-reduction-relation))
                make-procs/check-domain)])
         (make-reduction-relation lang 
                                  all-make-procs
                                  (append rule-names
                                          (reduction-relation-rule-names orig-reduction-relation))
                                  lws ;; only keep new lws for typesetting
                                  (map (λ (make-proc) (make-proc lang)) all-make-procs)))]
      [else
       (make-reduction-relation lang make-procs/check-domain rule-names lws 
                                (map (λ (make-proc) (make-proc lang)) 
                                     make-procs/check-domain))])))
