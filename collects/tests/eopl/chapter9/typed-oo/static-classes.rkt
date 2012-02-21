#lang eopl

(require "lang.rkt")
(require "static-data-structures.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;; method type environments ;;;;;;;;;;;;;;;;

;; a method tenv looks like ((method-name type) ...)
;; each method will have a proc-type

;;;;;;;;;;;;;;;; static classes ;;;;;;;;;;;;;;;;

(define identifier? symbol?)

(define method-tenv?
  (list-of 
   (lambda (p)
     (and 
      (pair? p)
      (symbol? (car p))
      (type? (cadr p))))))

(define-datatype static-class static-class?
  (a-static-class
   (super-name (maybe identifier?))
   (interface-names (list-of identifier?))
   (field-names (list-of identifier?))
   (field-types (list-of type?))
   (method-tenv method-tenv?))
  (an-interface
   (method-tenv method-tenv?)))

;; method-tenv * id -> (maybe type)
(define maybe-find-method-type
  (lambda (m-env id)
    (cond
      ((assq id m-env) => cadr)
      (else #f))))

;; class-name * id -> type OR fail
(define find-method-type
  (lambda (class-name id)
    (let ((m (maybe-find-method-type 
              (static-class->method-tenv (lookup-static-class class-name))
              id)))
      (if m m
          (eopl:error 'find-method 
                      "unknown method ~s in class ~s"
                      id class-name)))))

;;;;;;;;;;;;;;;; the static class environment ;;;;;;;;;;;;;;;;

;; the-static-class-env will look like ((class-name static-class) ...)

(define the-static-class-env '())

(define is-static-class?
  (lambda (name)
    (assq name the-static-class-env)))

(define lookup-static-class                    
  (lambda (name)
    (cond
      ((assq name the-static-class-env)
       => (lambda (pair) (cadr pair)))
      (else (eopl:error 'lookup-static-class
                        "Unknown class: ~s" name)))))

(define empty-the-static-class-env!
  (lambda ()
    (set! the-static-class-env '())))

(define add-static-class-binding!
  (lambda (name sc)
    (set! the-static-class-env
          (cons
           (list name sc)
           the-static-class-env))))


;;;;;;;;;;;;;;;; class declarations, etc. ;;;;;;;;;;;;;;;;

;; first, pull out all the types and put them in
;; the-static-class-env.

;; initialize-static-class-env! : Listof(ClassDecl) -> Unspecified
;; Page: 362
(define initialize-static-class-env!
  (lambda (c-decls)
    (empty-the-static-class-env!)
    (add-static-class-binding!
     'object (a-static-class #f '() '() '() '()))
    (for-each add-class-decl-to-static-class-env! c-decls)))

;; add-class-decl-to-static-class-env! : ClassDecl -> Unspecified
;; Page 366
(define add-class-decl-to-static-class-env!
  (lambda (c-decl)
    (cases class-decl c-decl 
      (an-interface-decl (i-name abs-m-decls)
                         (let ((m-tenv
                                (abs-method-decls->method-tenv abs-m-decls)))
                           (check-no-dups! (map car m-tenv) i-name)
                           (add-static-class-binding!
                            i-name (an-interface m-tenv))))
      (a-class-decl (c-name s-name i-names
                            f-types f-names m-decls)
                    (let ((i-names
                           (append
                            (static-class->interface-names
                             (lookup-static-class s-name))
                            i-names))
                          (f-names
                           (append-field-names
                            (static-class->field-names
                             (lookup-static-class s-name))
                            f-names))
                          (f-types
                           (append
                            (static-class->field-types
                             (lookup-static-class s-name))
                            f-types))
                          (method-tenv
                           (let ((local-method-tenv
                                  (method-decls->method-tenv m-decls)))
                             (check-no-dups!
                              (map car local-method-tenv) c-name)
                             (merge-method-tenvs
                              (static-class->method-tenv
                               (lookup-static-class s-name))
                              local-method-tenv))))
                      (check-no-dups! i-names c-name)
                      (check-no-dups! f-names c-name)
                      (check-for-initialize! method-tenv c-name)
                      (add-static-class-binding! c-name
                                                 (a-static-class
                                                  s-name i-names f-names f-types method-tenv)))))))

(define abs-method-decls->method-tenv
  (lambda (abs-m-decls)
    (map 
     (lambda (abs-m-decl)
       (cases abstract-method-decl abs-m-decl
         (an-abstract-method-decl (result-type m-name arg-ids arg-types)
                                  (list m-name (proc-type arg-types result-type)))))
     abs-m-decls)))


(define method-decls->method-tenv
  (lambda (m-decls)
    (map 
     (lambda (m-decl)
       (cases method-decl m-decl
         (a-method-decl (result-type m-name arg-ids arg-types body)
                        (list m-name (proc-type arg-types result-type)))))
     m-decls)))

;; append-field-names :  Listof(FieldName) * Listof(FieldName) 
;;                       -> Listof(FieldName)
;; Page: 344
;; like append, except that any super-field that is shadowed by a
;; new-field is replaced by a gensym
(define append-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields)
      (else
       (cons 
        (if (memq (car super-fields) new-fields)
            (fresh-identifier (car super-fields))
            (car super-fields))
        (append-field-names
         (cdr super-fields) new-fields))))))

;; new methods override old ones.  
(define merge-method-tenvs
  (lambda (super-tenv new-tenv)
    (append new-tenv super-tenv)))

(define check-for-initialize!
  (lambda (method-tenv class-name)
    (unless (maybe-find-method-type method-tenv 'initialize)
      (eopl:error 'check-for-initialize!
                  "no initialize method in class ~s"
                  class-name))))


;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

(define static-class->super-name
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name interface-names
                                  field-names field-types method-types)
                      super-name)
      (else (report-static-class-extractor-error 'super-name sc)))))


(define static-class->interface-names                               
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name interface-names
                                  field-names field-types method-types)
                      interface-names)
      (else (report-static-class-extractor-error 'interface-names sc)))))


(define static-class->field-names
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name interface-names
                                  field-names field-types method-types)
                      field-names)
      (else (report-static-class-extractor-error 'field-names sc)))))

(define static-class->field-types
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name interface-names
                                  field-names field-types method-types)
                      field-types)
      (else (report-static-class-extractor-error 'field-types sc)))))

(define static-class->method-tenv
  (lambda (sc)
    (cases static-class sc
      (a-static-class (super-name interface-names
                                  field-names field-types method-tenv)
                      method-tenv)
      (an-interface (method-tenv) method-tenv))))

(define report-static-class-extractor-error
  (lambda (sym sc)
    (eopl:error 'static-class-extractors
                "can't take ~s of interface ~s"
                sym sc)))

;; Listof(SchemeVal) * SchemeVal -> Unspecified
(define check-no-dups!
  (lambda (lst blamee)
    (let loop ((rest lst))
      (cond
        ((null? rest) #t)
        ((memv (car rest) (cdr rest))
         (eopl:error 'check-no-dups! "duplicate found among ~s in class ~s" lst
                     blamee))
        (else (loop (cdr rest)))))))

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))

(define maybe
  (lambda (pred)
    (lambda (v)
      (or (not v) (pred v)))))
