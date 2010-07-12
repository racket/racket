#lang scheme/base

(require syntax/parse 
         syntax/id-table racket/dict
         (for-template scheme/base scheme/flonum scheme/fixnum scheme/unsafe/ops racket/private/for)
         "../utils/utils.rkt"
         (types abbrev type-table utils subtype)
         (optimizer utils fixnum float inexact-complex vector pair sequence struct dead-code))

(provide optimize-top)


(define-syntax-class opt-expr
  (pattern e:opt-expr*
           #:with opt (syntax-recertify #'e.opt this-syntax (current-code-inspector) #f)))

(define-syntax-class opt-expr*
  #:literal-sets (kernel-literals)

  ;; interesting cases, where something is optimized
  (pattern e:fixnum-opt-expr          #:with opt #'e.opt)
  (pattern e:float-opt-expr           #:with opt #'e.opt)
  (pattern e:inexact-complex-opt-expr #:with opt #'e.opt)
  (pattern e:vector-opt-expr          #:with opt #'e.opt)
  (pattern e:pair-opt-expr            #:with opt #'e.opt)
  (pattern e:sequence-opt-expr        #:with opt #'e.opt)
  (pattern e:struct-opt-expr          #:with opt #'e.opt)
  (pattern e:dead-code-opt-expr       #:with opt #'e.opt)
  
  ;; boring cases, just recur down
  (pattern (#%plain-lambda formals e:opt-expr ...)
           #:with opt #'(#%plain-lambda formals e.opt ...))
  (pattern (define-values formals e:opt-expr ...)
           #:with opt #'(define-values formals e.opt ...))
  (pattern (case-lambda [formals e:opt-expr ...] ...)
           #:with opt #'(case-lambda [formals e.opt ...] ...))
  (pattern (let-values ([ids e-rhs:opt-expr] ...) e-body:opt-expr ...)
           #:with opt #'(let-values ([ids e-rhs.opt] ...) e-body.opt ...))
  (pattern (letrec-values ([ids e-rhs:opt-expr] ...) e-body:opt-expr ...)
           #:with opt #'(letrec-values ([ids e-rhs.opt] ...) e-body.opt ...))
  (pattern (letrec-syntaxes+values stx-bindings ([(ids ...) e-rhs:opt-expr] ...) e-body:opt-expr ...)
           #:with opt #'(letrec-syntaxes+values stx-bindings ([(ids ...) e-rhs.opt] ...) e-body.opt ...))
  (pattern (kw:identifier expr ...)
           #:when (ormap (lambda (k) (free-identifier=? k #'kw))
                         (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%app #'#%expression
                               #'#%variable-reference #'with-continuation-mark))
           ;; we don't want to optimize in the cases that don't match the #:when clause
           #:with (expr*:opt-expr ...) #'(expr ...)
           #:with opt #'(kw expr*.opt ...))
  (pattern other:expr
           #:with opt #'other))

(define (optimize-top stx)
  (let ((port (if (and *log-optimizations?*
                       *log-optimizatons-to-log-file?*)
                  (open-output-file *optimization-log-file*
                                    #:exists 'append)
                  (current-output-port))))
    (begin0
      (parameterize ([current-output-port port]
                     [optimize (lambda (stx)
                                 (syntax-parse stx #:literal-sets (kernel-literals)
                                               [e:opt-expr
                                                (syntax/loc stx e.opt)]))])
        ((optimize) stx))
      (if (and *log-optimizations?*
               *log-optimizatons-to-log-file?*)
          (close-output-port port)
          #t))))
