
(module class-help mzscheme
  (require mzlib/class)

  (provide init-accessible)

  ;; like `init-field', but makes a `get-' public method
  ;;  instead of a public field
  (define-syntax (init-accessible stx)
    (syntax-case stx ()
      [(_ id)
       (identifier? #'id)
       #'(init-accessible [id])]
      [(_ [id . val])
       (with-syntax ([get-id (datum->syntax-object
                              #'id
                              (string->symbol (format "get-~a" (syntax-e #'id)))
                              #'id)])
         #'(begin
             (init [(internal-id id) . val])
             (define private-id internal-id)
             (define/public (get-id) private-id)))]
      [(_ binding ...)
       #'(begin (init-accessible binding) ...)])))

