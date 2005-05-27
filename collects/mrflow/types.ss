(module types mzscheme
  (provide (all-defined))
  
  (define-struct type () (make-inspector))
  
  ; (make-type-empty) is the same as (make-type-cst 'bottom) for now. The reason we
  ; *never* use (make-type-cst 'bottom) is because it would trigger the propagation of
  ; bottom everywhere, thus slowing down the analysis. There's two solutions to that:
  ; - not use initialize-label-set-for-value-source when using (make-type-cst 'bottom)
  ; - use a separate (make-type-empty), which is more correct anyway (note that there's
  ;   currently no way to define the type for a primitive that returns the symbol 'bottom
  ;   (or 'number, or 'null, etc...))
  (define-struct (type-empty type) () (make-inspector))

  (define-struct (type-cst type) (type) (make-inspector))
  (define-struct (type-cons type) (car cdr) (make-inspector))
  (define-struct (type-vector type) (element) (make-inspector))
  (define-struct (type-case-lambda type) (rest-arg?s req-args argss exps) (make-inspector))
  (define-struct (type-var type) (name reach handle) (make-inspector))
  (define-struct (type-union type) (elements) (make-inspector))
  (define-struct (type-rec type) (vars types body) (make-inspector))
  (define-struct (type-values type) (type) (make-inspector))
  (define-struct (type-promise type) (value) (make-inspector))

  ; note: we have to keep the type label around, because that's the only thing
  ; that allows us to differentiate structurally equivalent structure that have
  ; the same name (i.e. the only way to have subtyping work in the presence of generative
  ; structures). The reason for type-struct-type is because structure types are first
  ; class values in mzscheme. Also, by keeping the type-label around, we avoid the need
  ; to duplicate the type hierarchy all the way up to the root each time we compute the
  ; type of a structure.
  (define-struct (type-struct-value type) (type-label types) (make-inspector))
  (define-struct (type-struct-type type) (type-label) (make-inspector))

  (define-struct (type-flow-var type) (name) (make-inspector))
  (define-struct (type-scheme type) (flow-vars type^cs type) (make-inspector))
  

  ;;
  ;; Printing
  ;; 

  (require (lib "match.ss")
           (prefix string: (lib "string.ss"))
           "util.ss"
           "labels.ss")

  (define type->list
    (lambda (type)
      (letrec
          ([loop (lambda (type) 
             (match type
               [($ type-empty) '_]
               [($ type-cst type)
                (if (null? type)
                    'null 
                    (string->symbol (string:expr->string type)))]
               [($ type-struct-type label)
                (string->symbol (string-append "#<struct-type:"
                                               (symbol->string (label-struct-type-name label))
                                               ">"))]
               [($ type-cons hd tl)
                (list 'cons (loop hd) (loop tl))]
               [($ type-case-lambda rest-arg?s req-args argss exps)
                (list 'case-lambda
                      (foldr-case-lambda-vector
                       (lambda (rest-arg? req-arg args exp acc)
                         (cons (list args (if rest-arg? '*-> '->) exp) acc))
                       null
                       rest-arg?s req-args argss exps))]
               [($ type-promise value)
                (list 'promise (loop value))]
               [($ type-struct-value label types)
                (list  (string->symbol
                        (string-append "#(struct:"
                                       (symbol->string (if (label-struct-type? label)
                                                           (label-struct-type-name label)
                                                           label))))
                       (map loop types))]
               [($ type-values values-type)
                (cond
                 [(type-empty? values-type)
                  (loop values-type)]
                 [(and (type-cst? values-type) (eq? (type-cst-type values-type) 'top))
                  (loop values-type)]
                 [else
                  (list 'values (loop values-type))])]
               [($ type-vector element)
                (list 'vector (loop element))]
               [($ type-union elements)
                (list 'union (map loop elements))]
               [($ type-rec vars binders body)
                (list 'rec-type
                      (map (lambda (v b)
                             (list (loop  v) (loop b)))
                           vars binders)
                      (loop body))]
               [($ type-var name r h)
                name]
               [(? natural?) (string->symbol (string-append "h:" (number->string type)))]))])
        (loop type))))

  (define handle? natural?)

  ;; Is there a better place for this?
  (define foldr-case-lambda-vector
    (lambda (f init rest-arg?s req-args argss exps)
      (let* ([v-to-l (lambda (x) (if (list? x) (list->vector x) x))]
             [rest-arg?s (v-to-l rest-arg?s)]
             [req-args (v-to-l req-args)]
             [argss (if (list? argss) (lol->vov argss) argss)]
             [exps (v-to-l exps)]
             [len (vector-length rest-arg?s)])
        (let loop ([i 0])
          (if (= i len) init
              (f (vector-ref rest-arg?s i)
                 (vector-ref req-args i)
                 (vector-ref argss i)
                 (vector-ref exps i)
                 (loop (add1 i))))))))

)
