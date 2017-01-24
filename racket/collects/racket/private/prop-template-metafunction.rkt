(module prop-template-metafunction '#%kernel
  (#%provide (protect metafunction
                      prop:metafunction
                      (rename prop:metafunction? metafunction?)
                      metafunction-accessor))
  
  ;; The prop:metafunction structure type property can contain an
  ;; identifier bound to the run-time metafunction procedure, or the index of a
  ;; field containing such an identifier.
  ;; At run-time, when processing the template, the syntax object whose first
  ;; element is a metafunction identifiers is passed to this metafunction
  ;; procedure.
  
  (define-values (prop:metafunction-guard)
    (lambda (val struct-type-info-list)
      (if (exact-nonnegative-integer? val)
          ;; Field index, the field must contain an identifier bound to the run-time
          ;; metafunction procedure.
          (let-values ([(make-struct-accessor) (cadddr struct-type-info-list)])
            (let-values ([(accessor) (make-struct-field-accessor make-struct-accessor val)])
              (λ (instance)
                (let-values ([(metafunction-internal-id) (accessor instance)])
                  ;; Check that the value contained in the field is correct
                  ;; (if (identifier? metafunction-internal-id) ...)
                  (if (if (syntax? metafunction-internal-id)
                          (symbol? (syntax-e metafunction-internal-id))
                          #f)
                      metafunction-internal-id
                      (raise-argument-error
                       'prop:metafunction-guard
                       (format (string-append "the value of the ~a-th field should be"
                                              " an identifier")
                               val)
                       metafunction-internal-id))))))
          ;;(if (identifier? val)
          (if (if (syntax? val) (symbol? (syntax-e val)) #f)
              ;; Identifier bound to the run-time metafunction procedure.
              (λ (_instance) val)
              ;; Otherwise, raise an error.
              (raise-argument-error
               'prop:metafunction-guard
               (string-append "an identifier, or an exact non-negative integer designating"
                              " a field index within the structure that should contain an"
                              " identifier.")
               val)))))
  
  (define-values (prop:metafunction
                  prop:metafunction?
                  metafunction-raw-accessor)
    (make-struct-type-property 'metafunction
                               prop:metafunction-guard))
  
  (define-values (metafunction-accessor)
    (lambda (instance . more-args)
      (let-values ([(raw) (if (null? more-args) ;; no failure-result given
                              (metafunction-raw-accessor instance)
                              (if (null? (cdr more-args))
                                  (let-values ([(failure-result) (car more-args)])
                                    (metafunction-raw-accessor instance failure-result))
                                  (error "invalid number of arguments [TODO]")))])
        (raw instance))))
  
  ;; A default struct type with prop:metafunction.
  ;; (struct template-metafunction (proc-id)
  ;;    #:property prop:template-metafunction (struct-field-index proc-id)) 
  (define-values (struct:metafunction metafunction metafunction? metafunction-ref _mf-set!)
    (make-struct-type 'syntax-metafunction #f 1 0 #f
                      (list (cons prop:metafunction 0))
                      (current-inspector))))