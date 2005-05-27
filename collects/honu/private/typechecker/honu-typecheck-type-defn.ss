(module honu-typecheck-type-defn mzscheme
  
  (require (lib "list.ss" "srfi" "1"))
  
  (require "../../ast.ss")
  (require "../../utils.ss")
  (require "honu-type-utils.ss")
  (require "../../read-error-with-stx.ss")
  
  (provide honu-typecheck-type-defn)
  
  ;; P |- t_i   P |- t'_i   P |- mdec_i
  ;; ------------------------------------
  ;; P, J |- type t <: t_1 ... t_m {
  ;;           t'_1 fd_1; ...; t'_k fd_k;
  ;;           mdec_1; ...; mdec_n;
  ;;         }
  ;;      |=> type t <: t_1 ... t_m
  ;;            t'_1 fd_1; ...; t'_k fd_k;
  ;;            mdec_1; ...; mdec_n;
  ;;          }, J
  ;;
  ;; Neither the type definition nor the init-arg environment
  ;; are modified by the type definition typechecker.
  (define (honu-typecheck-type-defn tenv defn)
    (begin (check-supers tenv (honu-type-defn-supers defn))
           (check-field-decls tenv (filter (lambda (d)
                                            (honu-field-decl? d))
                                          (honu-type-defn-decls defn)))
           (check-method-decls tenv (filter (lambda (d)
                                             (honu-method-decl? d))
                                           (honu-type-defn-decls defn)))
           defn))
  
  (define (check-supers tenv super-list)
    (cond
      [(null? super-list) #t]
      [(not (honu-iface-type-in-tenv? tenv (car super-list)))
       (raise-read-error-with-stx
        "Type declared as supertype not found in program."
        (honu-ast-src-stx (car super-list)))]
      [else (check-supers tenv (cdr super-list))]))
  
  (define (check-field-decls tenv field-list)
    (cond
      [(null? field-list) #t]
      [(not (honu-type-in-tenv? tenv (honu-field-decl-type (car field-list))))
       (raise-read-error-with-stx
        "Type of field not found in program."
        (honu-ast-src-stx (honu-field-decl-type (car field-list))))]
      [else (check-field-decls tenv (cdr field-list))]))

  ;; P |- t          P |- t_i
  ;; ------------------------
  ;; P |- t md(t_1, ..., t_n)
  (define (check-method-decls tenv method-list)
    (cond
      [(null? method-list) #t]
      [(not (or (honu-top-type? (honu-method-decl-type (car method-list))) ; allow void for return type of method
                (honu-type-in-tenv? tenv (honu-method-decl-type (car method-list)))))
       (raise-read-error-with-stx
        "Return type of method not found in program."
        (honu-ast-src-stx (honu-method-decl-type (car method-list))))]
      [else (let loop ((arg-types (honu-method-decl-arg-types (car method-list))))
              (cond
                [(null? arg-types)
                 (check-method-decls tenv (cdr method-list))]
                [(not (honu-type-in-tenv? tenv (car arg-types)))
                 (raise-read-error-with-stx
                  "Type of argument for method not found in program."
                  (honu-ast-src-stx (car arg-types)))]
                [else (loop (cdr arg-types))]))]))
  )
