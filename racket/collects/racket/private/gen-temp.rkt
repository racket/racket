;;----------------------------------------------------------------------
;; stateful syntax support

(module gen-temp '#%kernel
  (define-values (intro) #f)
  (define-values (counter) 0)
  (define-values (gen-temp-id)
    ;; Even though we gensym, using an introducer helps the
    ;;  syntax system simplify renamings that can't apply
    ;;  to other identifiers (when the generated identifier
    ;;  is used as a binding id)
    (lambda (pfx)
      (if intro
          (void)
          (set! intro (make-syntax-introducer)))
      (set! counter (add1 counter))
      (intro (datum->syntax #f (string->uninterned-symbol (format "~a~a" pfx counter))))))
  (#%provide gen-temp-id))
