(module parser-actions mzscheme
  (require "grammar.rkt")
  (provide (all-defined-except make-reduce make-reduce*)
           (rename make-reduce* make-reduce))

  ;; An action is 
  ;;  - (make-shift int)
  ;;  - (make-reduce prod runtime-action)
  ;;  - (make-accept)
  ;;  - (make-goto int)
  ;;  - (no-action)
  ;; A reduce contains a runtime-reduce so that sharing of the reduces can
  ;; be easily transferred to sharing of runtime-reduces.
  
  (define-struct action () (make-inspector))
  (define-struct (shift action) (state) (make-inspector))
  (define-struct (reduce action) (prod runtime-reduce) (make-inspector))
  (define-struct (accept action) () (make-inspector))
  (define-struct (goto action) (state) (make-inspector))
  (define-struct (no-action action) () (make-inspector))
  
  (define (make-reduce* p)
    (make-reduce p
                 (vector (prod-index p)
                         (gram-sym-symbol (prod-lhs p))
                         (vector-length (prod-rhs p)))))

  ;; A runtime-action is
  ;; non-negative-int        (shift)
  ;; (vector int symbol int) (reduce)
  ;; 'accept                 (accept)
  ;; negative-int            (goto)
  ;; #f                      (no-action)
  
  (define (action->runtime-action a)
    (cond
      ((shift? a) (shift-state a))
      ((reduce? a) (reduce-runtime-reduce a))
      ((accept? a) 'accept)
      ((goto? a) (- (+ (goto-state a) 1)))
      ((no-action? a) #f)))
  
  (define (runtime-shift? x) (and (integer? x) (>= x 0)))
  (define runtime-reduce? vector?)
  (define (runtime-accept? x) (eq? x 'accept))
  (define (runtime-goto? x) (and (integer? x) (< x 0)))

  (define runtime-shift-state values) 
  (define (runtime-reduce-prod-num x) (vector-ref x 0))
  (define (runtime-reduce-lhs x) (vector-ref x 1))
  (define (runtime-reduce-rhs-length x) (vector-ref x 2))
  (define (runtime-goto-state x) (- (+ x 1)))

  )
