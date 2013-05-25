#lang racket/base

(require syntax/parse
         (for-template racket/base racket/unsafe/ops)
         "../utils/utils.rkt"
         (utils tc-utils)
         (types abbrev)
         (optimizer utils logging))

(provide string-opt-expr string-expr bytes-expr)

(define-syntax-class string-expr
  #:commit
  (pattern e:expr
           #:when (isoftype? #'e -String)
           #:with opt ((optimize) #'e)))
(define-syntax-class bytes-expr
  #:commit
  (pattern e:expr
           #:when (isoftype? #'e -Bytes)
           #:with opt ((optimize) #'e)))

(define-syntax-class string-opt-expr
  #:commit
  (pattern (#%plain-app (~and op (~literal string-length)) s:string-expr)
           #:with opt
           (begin (log-optimization "string-length"
                                    "String check elimination."
                                    this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-string-length s.opt)))
  (pattern (#%plain-app (~and op (~literal bytes-length)) s:bytes-expr)
           #:with opt
           (begin (log-optimization "bytes-length"
                                    "Byte string check elimination."
                                    this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-bytes-length s.opt))))
