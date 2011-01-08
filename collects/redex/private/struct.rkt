#lang racket

(require "matcher.ss")

;; don't provide reduction-relation directly, so that we can use that for the macro's name.
(provide reduction-relation-lang
         reduction-relation-make-procs
         reduction-relation-rule-names
         reduction-relation-lws
         reduction-relation-procs
         build-reduction-relation make-reduction-relation
         reduction-relation?
         empty-reduction-relation
         make-rewrite-proc rewrite-proc? rewrite-proc-name 
         rewrite-proc-lhs rewrite-proc-lhs-src rewrite-proc-id
         (struct-out rule-pict))

(define-struct rule-pict (arrow lhs rhs label computed-label side-conditions/pattern-binds fresh-vars))

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

(define (build-reduction-relation original language rules rule-names lws domain)
  (define combined-rules
    (if original
        (append 
         (filter (λ (rule)
                   (or (not (rewrite-proc-name rule))
                       (not (member (string->symbol (rewrite-proc-name rule)) rule-names))))
                 (reduction-relation-make-procs original))
         rules)
        rules))
  (define combined-rule-names
    (if original
        (remove-duplicates (append rule-names (reduction-relation-rule-names original)))
        rule-names))
  (define compiled-domain
    (compile-pattern language domain #f))
  (make-reduction-relation
   language combined-rules combined-rule-names lws
   (map (λ (rule)
          (define specialized (rule language))
          (λ (tl-exp exp f acc)
            (unless (match-pattern compiled-domain tl-exp)
              (error 'reduction-relation "relation not defined for ~s" tl-exp))
            (let ([ress (specialized tl-exp exp f acc)])
              (for-each
               (λ (res)
                 (let ([term (caddr res)])
                   (unless (match-pattern compiled-domain term)
                     (error 'reduction-relation "relation reduced to ~s via ~a, which is outside its domain"
                            term
                            (let ([name (rewrite-proc-name rule)])
                              (if name
                                  (format "the rule named ~a" name)
                                  "an unnamed rule"))))))
               ress)
              ress)))
        combined-rules)))
