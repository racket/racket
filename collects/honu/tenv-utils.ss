(module tenv-utils mzscheme

  (require "read-error-with-stx.ss"
           "ast.ss"
           "tenv.ss"
           "private/typechecker/honu-type-utils.ss"
           (lib "plt-match.ss")
           (lib "list.ss" "srfi" "1"))

  (provide add-defns-to-tenv add-defn-to-tenv)
  (define (add-defns-to-tenv defns tenv)
    (for-each (lambda (d)
                (add-defn-to-tenv d tenv))
              defns))

  (define (add-defn-to-tenv defn tenv)
    (match defn
      [(struct honu-function (src-stx name t _ arg-types _))
       (extend-tenv name (make-tenv-func src-stx arg-types t) tenv)]
      [(struct honu-type-defn (src-stx name supers decls))
       (extend-tenv name (make-tenv-type src-stx supers decls) tenv)]
      [(struct honu-class (src-stx name t f? i-names i-types impls defns _))
       (extend-tenv name (make-tenv-class src-stx t impls
                                          (get-inits i-names i-types defns)
                                          f? #f) tenv)]
      [(struct honu-mixin (src-stx name type arg-type final? init-names init-types
               impls with-names with-types defns-before _ defns-after _))
       (extend-tenv name (make-tenv-mixin src-stx arg-type type impls
                                          (get-inits init-names init-types
                                                     (append defns-before 
                                                             defns-after))
                                          with-names with-types final?) tenv)]
      [(struct honu-subclass (src-stx name mixin base))
       (if (tenv-class-final? (get-class-entry base tenv))
           (raise-read-error-with-stx
            (format "Cannot apply mixin to final class ~a"
                    (printable-key base))
            base))
       (extend-tenv name (generate-subclass-tenv defn tenv) tenv)]))

  (define (get-inits init-names init-types defns)
    (let ([init-fields (filter (lambda (d)
                                 (honu-init-field? d))
                               defns)])
      (append (map (lambda (n t)
                     (make-tenv-init n t #t))
                   init-names init-types)
              (map (lambda (d)
                     (if (not (honu-init-field-value d))
                         (make-tenv-init (honu-init-field-name d)
                                         (honu-init-field-type d)
                                         #t)
                         (make-tenv-init (honu-init-field-name d)
                                         (honu-init-field-type d)
                                         #f)))
                   init-fields))))

  (define (generate-subclass-tenv defn tenv)
    (let ([base  (get-class-entry (honu-subclass-base defn)  tenv)]
          [mixin (get-mixin-entry (honu-subclass-mixin defn) tenv)])
      (if (not (<:_P tenv (tenv-class-sub-type base) (tenv-mixin-arg-type mixin)))
          (raise-read-error-with-stx
           (format "Class ~a is not of an appropriate type for mixin ~a"
                   (printable-key (honu-subclass-base defn))
                   (printable-key (honu-subclass-mixin defn)))
           (honu-subclass-base defn)))
      (let ([new-inits (remove-used-inits tenv defn
                                          (tenv-class-inits base)
                                          (tenv-mixin-used-names mixin)
                                          (tenv-mixin-used-types mixin))])
        (make-tenv-class (honu-ast-src-stx defn)
                         (tenv-mixin-sub-type mixin)
                         (tenv-mixin-impls mixin)
                         (append (tenv-mixin-inits mixin)
                                 new-inits)
                         (tenv-mixin-final? mixin)
                         (honu-subclass-base defn)))))

  (define (remove-used-inits tenv defn old-inits used-names used-types)
    (let loop ([old-inits  old-inits]
               [used-names used-names]
               [used-types used-types]
               [new-inits  '()])
      (if (null? old-inits)
          (if (not (null? used-names))
              (raise-read-error-with-stx
               (format "Class ~a does not have an init arg ~a with the correct type"
                       (printable-key (honu-subclass-base defn))
                       (printable-key (car used-names)))
               (honu-subclass-base defn))
              (reverse new-inits))
          (let* ([curr (car old-inits)]
                 [index (list-index (lambda (n)
                                      (tenv-key=? n (tenv-init-name curr)))
                                    used-names)])
            (if index
                (if (<:_P tenv (list-ref used-types index) (tenv-init-type curr))
                    (loop (cdr old-inits)
                          (append (take used-names index)
                                  (drop used-names (+ index 1)))
                          (append (take used-types index)
                                  (drop used-types (+ index 1)))
                          new-inits)
                    (if (tenv-init-optional? curr)
                        (loop (cdr old-inits)
                              used-names
                              used-types
                              (cons curr new-inits))
                        (raise-read-error-with-stx
                         (format "Mixin ~a needs an incompatible type for init arg ~a"
                                 (printable-key (honu-subclass-mixin defn))
                                 (printable-key (car used-names)))
                         (honu-subclass-mixin defn))))
                (loop (cdr old-inits)
                      used-names
                      used-types
                      (cons curr new-inits)))))))
  )
