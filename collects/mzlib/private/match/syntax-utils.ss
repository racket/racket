(module syntax-utils mzscheme
  ;; Useful utilities on syntax objects
  
  (provide (all-defined))
  
   ;;! (function stx-length
  ;;          (form (syntax-length syntax-obj) -> int)
  ;;          (contract syntax-object -> int)
  ;;          (example (syntax-length (syntax iraq war idiocy)) -> 3))
  ;;  Returns the length of the top-level syntax list.
  (define (stx-length stx) (length (syntax->list stx)))
  
  ;;! (function stx-?
  ;;          (form (stx? test val) -> bool)
  ;;          (contract ((any -> bool) syntax-object) -> bool)
  ;;          (example (stx-? number? (syntax 4)) -> #t))
  ;;  Applies predicate test to the syntax object val and returns the resulting
  ;; boolean value.
  (define (stx-? test val) (test (syntax-object->datum val)))
  
  ;;!(function stx-equal?
  ;;          (form (stx-equal? a b) -> bool)
  ;;          (contract (syntax-object syntax-object) -> bool)
  ;;          (example (stx-equal? (syntax 5) (syntax 5)) -> #t))
  ;; Check the equality of two syntax objects by after applying
  ;; syntax-object->datum to the objects first.  Checks equaltiy of
  ;; syntax objects after they have had all syntax data stripped away.
  (define (stx-equal? a b)
    (equal? (syntax-object->datum a)
            (syntax-object->datum b)))
  
  ;;!(function get-exp-var
  ;;          (form (get-exp-var) -> syntax)
  ;;          (contract () -> syntax)
  ;;          (example (get-exp-var) -> (syntax exp754)))
  ;; This function just produces unique identifiers for expressions.
  (define (get-exp-var) #`#,(gensym 'exp))
  
  
  ;; syntax-map : (stx -> b)  stx-list -> listof[b]
  ;; maps a function over a syntax object that represents a list
  (define (syntax-map f stx-l)
    (map f (syntax->list stx-l)))
  
  )