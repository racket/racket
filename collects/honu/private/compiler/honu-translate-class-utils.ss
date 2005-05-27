(module honu-translate-class-utils mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (prefix list: (lib "list.ss"))
           (lib "plt-match.ss"))
  
  (require "../../ast.ss")
  (require "../../tenv.ss")
  (require "../typechecker/honu-type-utils.ss")
  (require "honu-translate-utils.ss")
  (require "honu-translate-expression.ss")
  
  (provide honu-translate-init-slots)
  (define (honu-translate-init-slots slot-names)
    (map (lambda (name)
           (at name `(init ,(at-ctxt name))))
         slot-names))
  
  (provide honu-translate-slotdefns)
  (define (honu-translate-slotdefns tenv outer-defn defns)
    (map (match-lambda
           [(struct honu-init-field (stx name type value))
            (if value
                (at stx `(begin
                           (init ([,(add-init name) ,(at-ctxt name)]
                                  ,(honu-translate-expression tenv outer-defn value)))
                           (define ,(at-ctxt name) ,(add-init name))))
                (at stx `(begin
                           (init ([,(add-init name) ,(at-ctxt name)]))
                           (define ,(at-ctxt name) ,(add-init name)))))]
           [(struct honu-field (stx name type value))
            (at stx `(define ,(at-ctxt name)
                       ,(honu-translate-expression tenv outer-defn value)))]
           [(struct honu-method (stx name type arg-names arg-types body))
            (if (honu-top-type? type)
                (at stx `(define (,(at-ctxt name) ,@arg-names)
                           ,(honu-translate-expression tenv outer-defn body)
                           (void)))
                (at stx `(define (,(at-ctxt name) ,@arg-names)
                           ,(honu-translate-expression tenv outer-defn body))))])
         defns))
  
  (define (add-init sym)
    (at sym
        (string->symbol
         (string-append "init-" (symbol->string (printable-key sym))))))
  
  (define-struct pexp (new-name new-type old-name old-type method?))
  
  (provide honu-translate-exports)
  (define (honu-translate-exports tenv defn prior-impls exports)
    (let* ((processed-exports (apply append (map (lambda (e)
                                                   (process-export tenv defn e))
                                                 exports)))
           (filtered-exports (filter-exports processed-exports)))
      (map (lambda (pexp)
             (generate-export tenv prior-impls pexp))
           filtered-exports)))
  
  (define (check-prior-impls tenv prior-impls typ)
    (ormap (lambda (t)
             (<:_P tenv t typ))
           prior-impls))
  
  (define (generate-export tenv prior-impls pexp)
    (let ([new-name (pexp-new-name pexp)]
          [new-type (pexp-new-type pexp)]
          [old-name (pexp-old-name pexp)]
          [old-type (pexp-old-type pexp)]
          [method?  (pexp-method?  pexp)])
      (let ([define-sym (if (check-prior-impls tenv prior-impls new-type)
                            'define/override
                            'define/public)])
        (if method?
            (if old-type
                `(,define-sym (,(honu-translate-dynamic-method-name tenv new-name new-type) . args)
                   (super ,(honu-translate-dynamic-method-name tenv old-name old-type) . args))
                `(,define-sym (,(honu-translate-dynamic-method-name tenv new-name new-type) . args)
                   (apply ,(at-ctxt old-name) args)))
            (if old-type
                `(begin
                   (,define-sym (,(honu-translate-dynamic-field-getter tenv new-name new-type))
                     (super ,(honu-translate-dynamic-field-getter tenv old-name old-type)))
                   (,define-sym (,(honu-translate-dynamic-field-setter tenv new-name new-type) val)
                     (super ,(honu-translate-dynamic-field-setter tenv old-name old-type) val)))
                `(begin
                   (,define-sym (,(honu-translate-dynamic-field-getter tenv new-name new-type))
                     ,(at-ctxt old-name))
                   (,define-sym (,(honu-translate-dynamic-field-setter tenv new-name new-type) val)
                     (set! ,(at-ctxt old-name) val)
                     (void))))))))
  
  (define (process-export tenv defn e)
    (map (lambda (old new)
           (process-names tenv defn (honu-export-type e) old new))
         (honu-export-old-names e)
         (honu-export-new-names e)))

  (define (process-names tenv defn typ old new)
    (let ((slotdefns (cond
                       [(honu-class? defn)
                        (honu-class-defns defn)]
                       [(honu-mixin? defn)
                        (append (honu-mixin-defns-before defn)
                                (honu-mixin-defns-after defn))])))
      (cond
        [(find (lambda (s)
                 (tenv-key=? old s))
               (get-local-fields slotdefns))
         (make-pexp new (find-type-for-name tenv new typ)
                    old #f
                    #f)]
        [(find (lambda (s)
                 (tenv-key=? old s))
               (get-local-methods slotdefns))
         (make-pexp new (find-type-for-name tenv new typ)
                    old #f
                    #t)]
        [(and (honu-mixin? defn)
              (find (lambda (s)
                      (tenv-key=? old s))
                    (get-field-names-for-type tenv (honu-mixin-arg-type defn))))
         (make-pexp new (find-type-for-name tenv new typ)
                    old (find-type-for-name tenv old (honu-mixin-arg-type defn))
                    #f)]
        [(and (honu-mixin? defn)
              (find (lambda (s)
                      (tenv-key=? old s))
                    (get-method-names-for-type tenv (honu-mixin-arg-type defn))))
         (make-pexp new (find-type-for-name tenv new typ)
                    old (find-type-for-name tenv old (honu-mixin-arg-type defn))
                    #t)]
        [else (error (format "Shouldn't reach here!~n~nDefn~n~a~n~nTyp:~n~a~n~nOld:~n~a~n~nNew:~n~a~n~n"
                             defn 
                             (printable-key (honu-iface-type-name typ))
                             (printable-key old)
                             (printable-key new)))])))
  
  (define (pexp<? a b)
    (or (string<? (symbol->string (printable-key (honu-iface-type-name (pexp-new-type a))))
                  (symbol->string (printable-key (honu-iface-type-name (pexp-new-type b)))))
        (and (tenv-key=? (honu-iface-type-name (pexp-new-type a))
                         (honu-iface-type-name (pexp-new-type b)))
             (string<? (symbol->string (printable-key (pexp-new-name a)))
                       (symbol->string (printable-key (pexp-new-name b)))))))
  
  (define (pexp=? a b)
    (and (tenv-key=? (honu-iface-type-name (pexp-new-type a))
                     (honu-iface-type-name (pexp-new-type b)))
         (tenv-key=? (pexp-new-name a)
                     (pexp-new-name b))))
    
  (define (filter-exports pexps)
    (let ((sorted-exports (list:quicksort pexps pexp<?)))
      (let loop ((pexps sorted-exports)
                 (acc   '()))
        (cond
          [(null? pexps)       (reverse acc)]
          [(null? (cdr pexps)) (reverse (cons (car pexps) acc))]
          [(pexp=? (car pexps)
                   (cadr pexps)) (loop (cdr pexps) acc)]
          [else (loop (cdr pexps) (cons (car pexps) acc))]))))
  
  (define (get-local-fields slotdefns)
    (filter-map (lambda (d)
                  (cond
                    [(honu-init-field? d) (honu-init-field-name d)]
                    [(honu-field? d) (honu-field-name d)]
                    [else #f]))
                slotdefns))
  
  (define (get-local-methods slotdefns)
    (filter-map (lambda (d)
                  (cond 
                    [(honu-method? d) (honu-method-name d)]
                    [else #f]))
                slotdefns))
  )
