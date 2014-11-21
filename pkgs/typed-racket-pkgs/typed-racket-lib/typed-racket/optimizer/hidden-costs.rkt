#lang racket/base

(require syntax/parse unstable/sequence
         (for-template racket/base)
         "../utils/utils.rkt"
         (optimizer utils logging)
         (types abbrev struct-table))

(provide hidden-cost-log-expr)

;; not an exhaustive list
(define-literal-syntax-class hidden-port-parameter-function
  (display displayln newline write write-byte write-bytes print printf))
(define-literal-syntax-class hidden-random-parameter-function
  (random))
(define-literal-syntax-class regexp-function
  (regexp-match regexp-try-match regexp-match-positions regexp-match?
   regexp-match-peek regexp-match-peek-positions regexp-match-peek-immediate
   regexp-match-peek-positions-immediate regexp-match-peek-positions
   regexp-match/end regexp-match-positions/end regexp-match-peek-positions/end
   regexp-match-peek-positions-immediate/end regexp-split))

;; This syntax class does not perform optimization.
;; It only logs operations with hidden costs, for use by Optimization Coach.
(define-syntax-class hidden-cost-log-expr
  #:commit
  #:literal-sets (kernel-literals)

  ;; Log functions that access parameters implicitly (e.g. `display', which
  ;; accesses `current-output-port').
  (pattern (#%plain-app op:hidden-port-parameter-function args:opt-expr ...)
    ;; The function is not getting its output port as argument.
    ;; Since the port is first arg for some functions, second for
    ;; others, we're conservative, and look for a port in any position.
    #:when (for/and ([arg (in-syntax #'(args ...))])
             (not (subtypeof? arg -Output-Port)))
    #:do [(log-optimization-info "hidden parameter" #'op)]
    #:with opt (syntax/loc this-syntax (op args.opt ...)))
  ;; This one only fires if the call to `random' didn't get optimized
  ;; (which logs the hidden cost itself), i.e. (random <Integer>) .
  (pattern (#%plain-app op:hidden-random-parameter-function args:opt-expr ...)
    ;; see above
    #:when (for/and ([arg (in-syntax #'(args ...))])
             (not (subtypeof? arg -Pseudo-Random-Generator)))
    #:do [(log-optimization-info "hidden parameter (random)" #'op)]
    #:with opt (syntax/loc this-syntax (op args.opt ...)))

  ;; Log calls to struct constructors, so that OC can report those used in
  ;; hot loops.
  ;; Note: Sometimes constructors are wrapped in `#%expression', need to watch
  ;;  for that too.
  (pattern (#%plain-app (~and op-part (~or op:id (#%expression op:id)))
                        args:opt-expr ...)
    #:when (let ([constructor-for (syntax-property #'op 'constructor-for)])
             (or (and constructor-for (struct-constructor? constructor-for))
                 (struct-constructor? #'op)))
    #:do [(log-optimization-info "struct constructor" #'op)]
    #:with opt (syntax/loc this-syntax (op-part args.opt ...)))

  ;; regexp-match (or other regexp operation) with non-regexp pattern argument
  ;; (i.e. string or bytes)
  (pattern (#%plain-app op:regexp-function pattern-arg:opt-expr
                        args:opt-expr ...)
    #:when (not (or (subtypeof? #'pattern-arg -Regexp)
                    (subtypeof? #'pattern-arg -Byte-Regexp)))
    #:do [(log-optimization-info "non-regexp pattern" #'pattern-arg)]
    #:with opt (syntax/loc this-syntax (op pattern-arg.opt args.opt ...))))
