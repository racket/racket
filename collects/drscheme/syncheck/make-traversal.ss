#lang scheme/base

(require "id-sets.ss"
         "annotate.ss")

;; make-traversal : -> (values (namespace syntax (union #f syntax) -> void)
;;                             (namespace string[directory] -> void))
;; returns a pair of functions that close over some state that
;; represents the top-level of a single program. The first value
;; is called once for each top-level expression and the second
;; value is called once, after all expansion is complete.
(define (make-traversal)
  (let* ([tl-low-binders (make-id-set)]
         [tl-high-binders (make-id-set)]
         [tl-low-varrefs (make-id-set)]
         [tl-high-varrefs (make-id-set)]
         [tl-low-tops (make-id-set)]
         [tl-high-tops (make-id-set)]
         [tl-templrefs (make-id-set)]
         [tl-requires (make-hash-table 'equal)]
         [tl-require-for-syntaxes (make-hash-table 'equal)]
         [tl-require-for-templates (make-hash-table 'equal)]
         [tl-require-for-labels (make-hash-table 'equal)]
         [expanded-expression
          (λ (user-namespace user-directory sexp jump-to-id)
            (parameterize ([current-load-relative-directory user-directory])
              (let ([is-module? (syntax-case sexp (module)
                                  [(module . rest) #t]
                                  [else #f])])
                (cond
                  [is-module?
                   (let ([low-binders (make-id-set)]
                         [high-binders (make-id-set)]
                         [varrefs (make-id-set)]
                         [high-varrefs (make-id-set)]
                         [low-tops (make-id-set)]
                         [high-tops (make-id-set)]
                         [templrefs (make-id-set)]
                         [requires (make-hash-table 'equal)]
                         [require-for-syntaxes (make-hash-table 'equal)]
                         [require-for-templates (make-hash-table 'equal)]
                         [require-for-labels (make-hash-table 'equal)])
                     (annotate-basic sexp user-namespace user-directory jump-to-id
                                     low-binders high-binders varrefs high-varrefs low-tops high-tops
                                     templrefs
                                     requires require-for-syntaxes require-for-templates require-for-labels) 
                     (annotate-variables user-namespace
                                         user-directory
                                         low-binders
                                         high-binders
                                         varrefs
                                         high-varrefs
                                         low-tops
                                         high-tops
                                         templrefs
                                         requires
                                         require-for-syntaxes
                                         require-for-templates
                                         require-for-labels))]
                  [else
                   (annotate-basic sexp user-namespace user-directory jump-to-id
                                   tl-low-binders tl-high-binders
                                   tl-low-varrefs tl-high-varrefs 
                                   tl-low-tops tl-high-tops
                                   tl-templrefs
                                   tl-requires
                                   tl-require-for-syntaxes
                                   tl-require-for-templates
                                   tl-require-for-labels)]))))]
         [expansion-completed
          (λ (user-namespace user-directory)
            (parameterize ([current-load-relative-directory user-directory])
              (annotate-variables user-namespace
                                  user-directory
                                  tl-low-binders
                                  tl-high-binders
                                  tl-low-varrefs
                                  tl-high-varrefs
                                  tl-low-tops
                                  tl-high-tops
                                  tl-templrefs
                                  tl-requires
                                  tl-require-for-syntaxes
                                  tl-require-for-templates
                                  tl-require-for-labels)))])
    (values expanded-expression expansion-completed)))