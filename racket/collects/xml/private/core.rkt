#lang racket/base
(require racket/runtime-path
         (for-syntax racket/base))

;; Core structures needed for `xml/xexpr'

(provide permissive-xexprs
         (struct-out source)
         (struct-out comment)
         (struct-out p-i)
         (struct-out cdata)
         valid-char?)

; permissive-xexprs : parameter bool
(define permissive-xexprs (make-parameter #f))

; support for serialization with contracts enforced on deserialization
(module serialization-support racket/base
  (require racket/serialize-structs
           (for-syntax racket/syntax
                       racket/struct-info
                       racket/base))
  (define ((cycles-not-allowed name))
    (error name "invalid serialization;\n cycles not allowed"))
  (define-for-syntax ((make-define-serializable-struct deserialize-info-mpi
                                                       get-checked-constructor)
                      stx)
    (syntax-case stx ()
      [(_ name-maybe-super (fld ...))
       (with-syntax* ([(name super-fld-ref ...)
                       (syntax-case #'name-maybe-super ()
                         [name
                          (identifier? #'name)
                          #'(name)]
                         [(name super)
                          #`(name
                             #,@(reverse (list-ref (extract-struct-info (syntax-local-value #'super))
                                                   3)))])]
                      [(name-fld-ref ...)
                       (for/list ([id (in-list (syntax->list #'(fld ...)))])
                         (format-id id "~a-~a" #'name id))]
                      [deserialize-info:name-v0
                       (format-id #'name "deserialize-info:~a-v0" #'name)])
         #`(begin
             (define-struct name-maybe-super (fld ...)
               #:property prop:serializable
               (make-serialize-info (λ (this)
                                      (vector (super-fld-ref this) ...
                                              (name-fld-ref this) ...))
                                    (cons 'deserialize-info:name-v0 #,deserialize-info-mpi)
                                    #f
                                    (or (current-load-relative-directory) (current-directory)))
               #:transparent)
             (module+ deserialize-info
               (provide deserialize-info:name-v0)
               (define deserialize-info:name-v0
                 (make-deserialize-info (#,get-checked-constructor name)
                                        (cycles-not-allowed 'name))))))]))
  (provide (for-syntax make-define-serializable-struct)))
(require 'serialization-support)
(define-runtime-module-path-index deserialize-info-mpi '(submod "." deserialize-info))
(module+ deserialize-info
  (define-runtime-module-path-index mpi-for-contracts "structures.rkt")
  (define-syntax-rule (get-checked-constructor name)
    (dynamic-require mpi-for-contracts 'name)))
(define-syntax define-xexpr-struct
  (make-define-serializable-struct #'deserialize-info-mpi #'get-checked-constructor))

; Source = (make-source Location Location)
(define-struct source (start stop)
  ; NOT define-xexpr-struct (and not serializable) because this is used as an abstract base type:
  ; if a subtype is not intended to be serializable, it shouldn't be serialized by inheritance
  #:transparent)

; Comment = (make-comment String)
(define-xexpr-struct comment (text))

; Processing-instruction = (make-p-i Location Location String String)
; also represents XMLDecl
(define-xexpr-struct (p-i source) (target-name instruction))

; Cdata = (make-cdata Location Location String)
(define-xexpr-struct (cdata source) (string))

; Section 2.2 of XML 1.1
; (XML 1.0 is slightly different and more restrictive)
(define (valid-char? i)
  (and (exact-nonnegative-integer? i)
       (or (<= #x1     i #xD7FF)
           (<= #xE000  i #xFFFD)
           (<= #x10000 i #x10FFFF))))
