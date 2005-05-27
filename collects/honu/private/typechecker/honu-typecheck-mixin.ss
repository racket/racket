(module honu-typecheck-mixin mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (prefix list: (lib "list.ss"))
           (lib "struct.ss"))
  
  (require "../../ast.ss")
  (require "../../utils.ss")
  (require "../../tenv.ss")
  (require "honu-type-utils.ss")
  (require "honu-typecheck-class-utils.ss")
  (require "honu-typecheck-exp.ss")
  
  (require "../../read-error-with-stx.ss")
  
  (provide honu-typecheck-mixin)
  (define (honu-typecheck-mixin tenv mixin)
    (if (not (honu-iface-type-in-tenv? tenv (honu-mixin-type mixin)))
        (raise-read-error-with-stx
         "Type of this within mixin not defined in program."
         (honu-ast-src-stx (honu-mixin-type mixin))))
    (if (not (honu-iface-type-in-tenv? tenv (honu-mixin-arg-type mixin)))
        (raise-read-error-with-stx
         "Type mixin takes as argument not defined in program."
         (honu-ast-src-stx (honu-mixin-type mixin))))
    (if (not (<:_P tenv (honu-mixin-type mixin) (honu-mixin-arg-type mixin)))
        (raise-read-error-with-stx
         "Type of this within mixin not subtype of argument class."
         (honu-ast-src-stx (honu-mixin-type mixin))))
    (check-impl-types tenv (honu-mixin-impls mixin))
    (check-init-slots tenv (honu-mixin-init-names mixin) (honu-mixin-init-types mixin))
    (let*-values (((new-befores new-env new-cenv new-init-cenv)
                   (honu-typecheck-slotdefns tenv
                                             (extend-env (get-initial-env tenv) #'this (honu-mixin-type mixin))
                                             (empty-env)
                                             (fold (lambda (n t e)
                                                     (extend-env e n t))
                                                   (empty-env)
                                                   (honu-mixin-init-names mixin)
                                                   (honu-mixin-init-types mixin))
                                             (honu-mixin-defns-before mixin)))
                  ((new-super-new)
                   (honu-typecheck-super-new tenv new-env new-cenv new-init-cenv
                                             (honu-mixin-super-new mixin)
                                             (honu-mixin-with-names mixin)
                                             (honu-mixin-with-types mixin)))
                  ((new-cenv new-init-cenv)
                   (extend-cenvs new-cenv new-init-cenv
                                 (get-fields-and-methods tenv (honu-mixin-arg-type mixin))))
                  ((new-afters new-env new-cenv new-init-cenv)
                   (honu-typecheck-slotdefns tenv new-env new-cenv new-init-cenv
                                             (honu-mixin-defns-after mixin))))
      (check-impls-and-exports tenv new-cenv
                               (honu-mixin-type mixin)
                               (honu-mixin-impls mixin)
                               (honu-mixin-exports mixin))
      (copy-struct honu-mixin mixin
        (honu-mixin-defns-before new-befores)
        (honu-mixin-super-new new-super-new)
        (honu-mixin-defns-after  new-afters))))

  (define (sort-by-names names vals)
    (let ((sorted (list:quicksort (map cons names vals)
                                  (lambda (a b)
                                    (string<? (symbol->string (printable-key (car a)))
                                              (symbol->string (printable-key (car b))))))))
      (values (map car sorted) (map cdr sorted))))
  
  (define (honu-typecheck-super-new tenv env cenv init-cenv snew with-names with-types)
    (let-values (((arg-names arg-vals)
                  (sort-by-names (honu-super-new-arg-names snew)
                                 (honu-super-new-arg-vals  snew)))
                 ((with-names with-types)
                  (sort-by-names with-names with-types)))
      (begin
        (if (not (andmap tenv-key=? arg-names with-names))
            (raise-read-error-with-stx
             "Arguments to super_new do not match declared names in with clause."
             (honu-ast-src-stx snew)))
        (copy-struct honu-super-new snew
          (honu-super-new-arg-vals
           (map (lambda (dec-type val)
                             (let-values (((e1 t1)
                                           ((honu-typecheck-exp tenv env init-cenv) val)))
                               (if (<:_P tenv t1 dec-type)
                                   e1
                                   (raise-read-error-with-stx
                                    "Argument to super_new does not match declared type for name."
                                    (honu-ast-src-stx val)))))
                with-types arg-vals))))))
                                
  (define (extend-cenvs cenv init-cenv new-stuff)
    (if (null? new-stuff)
        (values cenv init-cenv)
        (extend-cenvs (extend-env cenv (caar new-stuff) (cdar new-stuff))
                      (extend-env init-cenv (caar new-stuff) (cdar new-stuff))
                      (cdr new-stuff))))
  )
