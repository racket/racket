#lang eopl

(require "lang.rkt")

(provide rename-in-iface fresh-module-name)

(define rename-in-iface
  (lambda (m-type old new)
    (cases interface m-type
      (simple-iface (decls)
                    (simple-iface
                     (rename-in-decls decls old new))) )))

;; this isn't a map because we have let* scoping in a list of declarations
(define rename-in-decls
  (lambda (decls old new)
    (if (null? decls) '()
        (let ((decl (car decls))
              (decls (cdr decls)))
          (cases declaration decl
            (val-decl (name ty)
                      (cons
                       (val-decl name (rename-in-type ty old new))
                       (rename-in-decls decls old new)))
            (opaque-type-decl (name)
                              (cons
                               (opaque-type-decl name)
                               (if (eqv? name old)
                                   decls
                                   (rename-in-decls decls old new))))
            (transparent-type-decl (name ty)
                                   (cons
                                    (transparent-type-decl 
                                     name
                                     (rename-in-type ty old new))
                                    (if (eqv? name old)
                                        decls
                                        (rename-in-decls decls old new))))
            )))))

(define rename-in-type
  (lambda (ty old new)
    (let recur ((ty ty))
      (cases type ty 
        (named-type (id)
                    (named-type (rename-name id old new)))
        (qualified-type (m-name name)
                        (qualified-type
                         (rename-name m-name old new)
                         name))
        (proc-type (t1 t2)
                   (proc-type (recur t1) (recur t2)))
        (else ty)              ; this covers int, bool, and unknown.
        ))))

(define rename-name
  (lambda (name old new)
    (if (eqv? name old) new name)))

(define fresh-module-name
  (let ((sn 0))
    (lambda (module-name)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string module-name)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))


