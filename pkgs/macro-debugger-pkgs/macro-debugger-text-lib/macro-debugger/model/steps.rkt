#lang racket/base
(provide (struct-out protostep)
         (struct-out step)
         (struct-out misstep)
         (struct-out remarkstep)
         (struct-out state)
         (struct-out bigframe)
         context-fill
         state-term
         step-term1
         step-term2
         misstep-term1
         bigframe-term
         step-type?
         step-type->string
         rewrite-step?
         rename-step?)

;; A ReductionSequence is (listof Step)
;; A Step is one of
;;  - (make-step StepType State State)
;;  - (make-misstep StepType State exn)
;;  - (make-remarkstep StepType State (listof (U string syntax 'arrow)))
(define-struct protostep (type s1) #:transparent)
(define-struct (step protostep) (s2) #:transparent)
(define-struct (misstep protostep) (exn) #:transparent)
(define-struct (remarkstep protostep) (contents) #:transparent)

;; A State is
;;  (make-state stx stxs Context BigContext (listof id) (listof id) (listof stx) nat/#f)
(define-struct state (e foci ctx lctx binders uses frontier seq) #:transparent)

;; A Context is a list of Frames
;; A Frame is (syntax -> syntax)

;; A BigContext is (list-of BigFrame)
;; A BigFrame is (make-bigframe Context Syntaxes Syntax)
(define-struct bigframe (ctx foci e))

;; context-fill : Context Syntax -> Syntax
(define (context-fill ctx stx)
  (datum->syntax
   #f
   (let loop ([ctx ctx] [stx stx])
     (if (null? ctx)
         stx
         (let ([frame0 (car ctx)])
           (loop (cdr ctx) (frame0 stx)))))))

(define (state-term s)
  (context-fill (state-ctx s) (state-e s)))

(define (step-term1 s)
  (state-term (protostep-s1 s)))
(define (step-term2 s)
  (state-term (step-s2 s)))

(define (misstep-term1 s)
  (state-term (protostep-s1 s)))

(define (bigframe-term bf)
  (context-fill (bigframe-ctx bf) (bigframe-e bf)))

;; A StepType is a simple in the following alist.

(define step-type-meanings
  '((macro            . "Macro transformation")
    
    (rename-lambda    . "Rename formal parameters")
    (rename-case-lambda . "Rename formal parameters")
    (rename-let-values . "Rename bound variables")
    (rename-letrec-values . "Rename bound variables")
    (rename-lsv       . "Rename bound variables")
    (lsv-remove-syntax . "Remove syntax bindings")

    (resolve-variable . "Resolve variable (remove extra marks)")
    (tag-module-begin . "Tag #%module-begin")
    (tag-app          . "Tag application")
    (tag-datum        . "Tag datum")
    (tag-top          . "Tag top-level variable")
    (capture-lifts    . "Capture lifts")
    (provide          . "Expand provide-specs")

    (local-lift       . "Macro lifted expression to top-level")
    (module-lift      . "Macro lifted declaration to end of module")
    (block->letrec    . "Transform block to letrec")
    (splice-block     . "Splice block-level begin")
    (splice-module    . "Splice module-level begin")
    (splice-lifts     . "Splice definitions from lifted expressions")
    (splice-module-lifts . "Splice lifted module declarations")

    (remark           . "Macro made a remark")
    (track-origin     . "Macro called syntax-track-origin")

    (error            . "Error")))

(define (step-type->string x)
  (cond [(assq x step-type-meanings) => cdr]
        [(string? x) x]
        [else (error 'step-type->string "not a step type: ~s" x)]))

(define step-type?
  (let ([step-types (map car step-type-meanings)])
    (lambda (x)
      (and (memq x step-types) #t))))

(define (rename-step? x)
  (memq (protostep-type x) 
        '(rename-lambda
          rename-case-lambda
          rename-let-values
          rename-letrec-values
          rename-lsv
          track-origin)))

(define (rewrite-step? x)
  (and (step? x) (not (rename-step? x))))
