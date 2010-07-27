#lang scheme/base

(require syntax/parse 
         syntax/id-table racket/dict
         racket/pretty
         (for-template scheme/base
                       scheme/flonum scheme/fixnum scheme/unsafe/ops
                       racket/private/for)
         "../utils/utils.rkt"
         (types abbrev type-table utils subtype)
         (optimizer utils number fixnum float inexact-complex vector string
                    pair sequence box struct dead-code apply unboxed-let))

(provide optimize-top)


(define-syntax-class opt-expr
  (pattern e:opt-expr*
           #:with opt (syntax-recertify #'e.opt this-syntax (current-code-inspector) #f)))

(define-syntax-class opt-expr*
  #:literal-sets (kernel-literals)

  ;; interesting cases, where something is optimized
  (pattern e:apply-opt-expr           #:with opt #'e.opt)
  (pattern e:number-opt-expr          #:with opt #'e.opt)
  (pattern e:fixnum-opt-expr          #:with opt #'e.opt)
  (pattern e:float-opt-expr           #:with opt #'e.opt)
  (pattern e:inexact-complex-opt-expr #:with opt #'e.opt)
  (pattern e:vector-opt-expr          #:with opt #'e.opt)
  (pattern e:string-opt-expr          #:with opt #'e.opt)
  (pattern e:pair-opt-expr            #:with opt #'e.opt)
  (pattern e:sequence-opt-expr        #:with opt #'e.opt)
  (pattern e:box-opt-expr             #:with opt #'e.opt)
  (pattern e:struct-opt-expr          #:with opt #'e.opt)
  (pattern e:dead-code-opt-expr       #:with opt #'e.opt)
  (pattern e:unboxed-let-opt-expr     #:with opt #'e.opt)
  
  ;; boring cases, just recur down
  (pattern ((~and op (~or (~literal #%plain-lambda) (~literal define-values)))
            formals e:expr ...)
           #:with opt #`(op formals #,@(map (optimize) (syntax->list #'(e ...)))))
  (pattern (case-lambda [formals e:expr ...] ...)
           ;; optimize all the bodies
           #:with (opt-parts ...)
           (map (lambda (part)
                  (let ((l (syntax->list part)))
                    (cons (car l)
                          (map (optimize) (cdr l)))))
                (syntax->list #'([formals e ...] ...)))
           #:with opt #'(case-lambda opt-parts ...))
  (pattern ((~and op (~or (~literal let-values) (~literal letrec-values)))
            ([ids e-rhs:expr] ...) e-body:expr ...)
           #:with (opt-rhs ...) (map (optimize) (syntax->list #'(e-rhs ...)))
           #:with opt #`(op ([ids opt-rhs] ...)
                            #,@(map (optimize) (syntax->list #'(e-body ...)))))
  (pattern (letrec-syntaxes+values stx-bindings
                                   ([(ids ...) e-rhs:expr] ...)
                                   e-body:expr ...)
           ;; optimize all the rhss
           #:with (opt-clauses ...)
           (map (lambda (clause)
                  (let ((l (syntax->list clause)))
                    (list (car l) ((optimize) (cadr l)))))
                (syntax->list #'([(ids ...) e-rhs] ...)))
           #:with opt #`(letrec-syntaxes+values
                         stx-bindings
                         (opt-clauses ...)
                         #,@(map (optimize) (syntax->list #'(e-body ...)))))
  (pattern (kw:identifier expr ...)
           #:when 
	   (for/or ([k (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%app #'#%expression
			     #'#%variable-reference #'with-continuation-mark)])
	     (free-identifier=? k #'kw))
           ;; we don't want to optimize in the cases that don't match the #:when clause
           #:with opt #`(kw #,@(map (optimize) (syntax->list #'(expr ...)))))
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
                     [optimize (syntax-parser
                                [e:expr
                                 #:when (and (not (syntax-property #'e 'typechecker:ignore))
                                             (not (syntax-property #'e 'typechecker:with-handlers)))
                                 #:with e*:opt-expr #'e
                                 #'e*.opt]
                                [e:expr #'e])])
        (let ((result ((optimize) stx)))
          (when *show-optimized-code*
            (pretty-print (syntax->datum result)))
          result))
      (when (and *log-optimizations?*
                 *log-optimizatons-to-log-file?*)
        (close-output-port port)))))
