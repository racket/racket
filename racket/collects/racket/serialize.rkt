
(module serialize racket/base
  (require "private/serialize.rkt"
           (for-syntax racket/base
                       racket/struct-info)
           racket/runtime-path)

  (provide (all-from-out "private/serialize.rkt")
           serializable-struct
           serializable-struct/versions
           define-serializable-struct
           define-serializable-struct/versions)

  (define-syntax (define-serializable-struct/versions/derived stx)
    (syntax-case stx ()
      ;; First check `id/sup':
      [(_ orig-stx make-prefix? id/sup . _)
       (not (or (identifier? #'id/sup)
                (syntax-case #'id/sup ()
                  [(id sup) (and (identifier? #'id) 
                                 (identifier? #'sup)
                                 (let ([v (syntax-local-value #'sup (lambda () #f))])
                                   (struct-info? v)))]
                  [_ #f])))
       ;; Not valid, so let `define-struct/derived' complain:
       #'(define-struct/derived orig-stx id/sup ())]
      ;; Check version:
      [(_ orig-stx make-prefix? id/sup vers . _)
       (not (exact-nonnegative-integer? (syntax-e #'vers)))
       (raise-syntax-error #f "expected a nonnegative exact integer for a version" #'orig-stx #'vers)]
      ;; Main case:
      [(_ orig-stx make-prefix? id/sup vers (field ...) ([other-vers make-proc-expr cycle-make-proc-expr] ...) 
          prop ...)
       (let* ([id (if (identifier? #'id/sup)
                      #'id/sup
                      (car (syntax-e #'id/sup)))]
              [super-info (if (identifier? #'id/sup)
                              #f
                              (extract-struct-info (syntax-local-value (cadr (syntax->list #'id/sup)))))]
              [fields (syntax->list #'(field ...))]
              [given-maker (let loop ([props (syntax->list #'(prop ...))])
                             (cond
                              [(null? props) #f]
                              [(null? (cdr props)) #f]
                              [(or (eq? (syntax-e (car props)) '#:constructor-name)
                                   (eq? (syntax-e (car props)) '#:extra-constructor-name))
                               (and (identifier? (cadr props))
                                    (cadr props))]
                              [else (loop (cdr props))]))]
              [maker (or given-maker
                         (if (syntax-e #'make-prefix?)
                             (datum->syntax id
                                            (string->symbol
                                             (format "make-~a" (syntax-e id)))
                                            id)
                             id))]
              [getters (map (lambda (field)
                              (datum->syntax
                               id
                               (string->symbol
                                (format "~a-~a"
                                        (syntax-e id)
                                        (syntax-e
                                         (if (identifier? field)
                                             field
                                             (syntax-case field ()
                                               [(id . _)
                                                (if (identifier? #'id)
                                                    #'id
                                                    #'bad)]
                                               [_ #'bad])))))))
                            fields)]
              [mutable? (ormap (lambda (x)
                                 (eq? '#:mutable (syntax-e x)))
                               (syntax->list #'(prop ...)))]
              [setters (map (lambda (field)
                              (let-values ([(field-id mut?)
                                            (if (identifier? field)
                                                (values field #f)
                                                (syntax-case field ()
                                                  [(id prop ...)
                                                   (values (if (identifier? #'id)
                                                               #'id
                                                               #'bad)
                                                           (ormap (lambda (x)
                                                                    (eq? '#:mutable (syntax-e x)))
                                                                  (syntax->list #'(prop ...))))]
                                                  [_ (values #'bad #f)]))])
                                (and (or mutable? mut?)
                                     (datum->syntax
                                      id
                                      (string->symbol
                                       (format "set-~a-~a!"
                                               (syntax-e id)
                                               (syntax-e field-id)))))))
                            fields)]
              [make-deserialize-id (lambda (vers)
                                     (datum->syntax id
                                                    (string->symbol
                                                     (format "deserialize-info:~a-v~a"
                                                             (syntax-e id)
                                                             (syntax-e vers)))
                                                    id))]
              [deserialize-id (make-deserialize-id #'vers)]
              [other-deserialize-ids (map make-deserialize-id
                                          (syntax->list #'(other-vers ...)))])
         (when super-info
           (unless (andmap values (list-ref super-info 3))
             (raise-syntax-error
              #f
              "not all fields are known for parent struct type"
              #'orig-stx
              (syntax-case #'id/sup ()
                [(_ sup) #'sup]))))
         #`(begin
             ;; =============== struct with serialize property ================
             (define-struct/derived orig-stx
               id/sup
               (field ...)
               prop ...
               #,@(if (or given-maker
                          (syntax-e #'make-prefix?))
                      null
                      (list #'#:constructor-name id))
               #:property prop:serializable
               (make-serialize-info
                ;; The struct-to-vector function: --------------------
                (lambda (v)
                  (vector
                   #,@(if super-info
                          (reverse
                           (map (lambda (sel)
                                  #`(#,sel v))
                                (list-ref super-info 3)))
                          null)
                   #,@(map (lambda (getter)
                             #`(#,getter v))
                           getters)))
                ;; The serializer id: --------------------
                (quote-syntax #,deserialize-id)
                ;; Can handle cycles? --------------------
                ;;  Yes, as long as we have mutators for the
                ;;  superclass.
                #,(and (andmap values setters)
                       (or (not super-info)
                           (andmap values (list-ref super-info 4))))
                ;; Directory for last-ditch resolution --------------------
                (or (current-load-relative-directory) 
                    (current-directory))))
             ;; =============== deserialize info ================
             (define #,deserialize-id 
               (make-deserialize-info
                ;; The maker: --------------------
                #,maker
                ;; The shell function: --------------------
                ;;  Returns an shell object plus
                ;;  a function to update the shell (used for
                ;;  building cycles): 
                (let ([super-sets
                       (list #,@(if super-info
                                    (list-ref super-info 4)
                                    null))])
                  (lambda ()
                    (let ([s0
                           (#,maker
                            #,@(append
                                (if super-info
                                    (map (lambda (x) #f)
                                         (list-ref super-info 3))
                                    null)
                                (map (lambda (g)
                                       #f)
                                     getters)))])
                      (values
                       s0
                       (lambda (s)
                         #,@(if super-info
                                (map (lambda (set get)
                                       #`(#,set s0 (#,get s)))
                                     (list-ref super-info 4)
                                     (list-ref super-info 3))
                                null)
                         #,@(map (lambda (getter setter)
                                   #`(#,setter s0 (#,getter s)))
                                 getters
                                 setters)
                         (void))))))))
             #,@(map (lambda (other-deserialize-id proc-expr cycle-proc-expr)
                       #`(define #,other-deserialize-id
                           (make-deserialize-info #,proc-expr #,cycle-proc-expr)))
                     other-deserialize-ids
                     (syntax->list #'(make-proc-expr ...))
                     (syntax->list #'(cycle-make-proc-expr ...)))
             ;; =============== provide ===============
             ;; If we're in a module context, then provide through
             ;; a submodule:
             (#,@(if (eq? 'top-level (syntax-local-context))
                     #'(begin)
                     #'(module+ deserialize-info))
              #,@(map (lambda (deserialize-id)
                        (if (eq? 'top-level (syntax-local-context))
                            ;; Top level; in case deserializer-id-stx is macro-introduced,
                            ;;  explicitly use namespace-set-variable-value!
                            #`(namespace-set-variable-value! '#,deserialize-id
                                                             #,deserialize-id)
                            ;; In a module; provide:
                            #`(provide #,deserialize-id)))
                      (cons deserialize-id
                            other-deserialize-ids)))
             ;; Make sure submodule is pulled along for run time:
             #,@(if (eq? 'top-level (syntax-local-context))
                    null
                    #'((runtime-require (submod "." deserialize-info))))))]
      ;; -- More error cases ---
      ;; Check fields
      [(_ orig-stx id/sup vers fields . _rest)
       ;; fields isn't a sequence:
       #`(define-struct/derived orig-stx fields)]
      ;; vers-spec bad?
      [(_ orig-stx id/sup vers fields vers-spec prop ...)
       ;; Improve this:
       (raise-syntax-error 
        #f
        "expected a parenthesized sequence of version mappings"
        #'orig-stx
        #'vers-spec)]
      ;; Last-ditch error:
      [(_ orig-stx . _)
       (raise-syntax-error #f "bad syntax" #'orig-stx)]))

  (define-syntax (define-serializable-struct/versions stx)
    (syntax-case stx ()
      [(_ . rest)
       #`(define-serializable-struct/versions/derived #,stx #t . rest)]))

  (define-syntax (serializable-struct/versions stx)
    (syntax-case stx ()
      [(_ id super-id . rest)
       (and (identifier? #'id)
            (identifier? #'super-id))
       #`(define-serializable-struct/versions/derived #,stx #f (id super-id) . rest)]
      [(_ id vers (field ...) . rest)
       (and (identifier? #'id)
            (number? (syntax-e #'vers)))
       #`(define-serializable-struct/versions/derived #,stx #f id vers (field ...) . rest)]))
  
  (define-syntax (define-serializable-struct stx)
    (syntax-case stx ()
      [(_ id/sup (field ...) prop ...)
       #`(define-serializable-struct/versions/derived #,stx #t
           id/sup 0 (field ...) () prop ...)]
      [(_ . rest)
       #`(define-struct/derived #,stx . rest)]))

  (define-syntax (serializable-struct stx)
    (syntax-case stx ()
      [(_ id super-id (field ...) prop ...)
       (and (identifier? #'id)
            (identifier? #'super-id))
       #`(define-serializable-struct/versions/derived #,stx #f
           (id super-id) 0 (field ...) () prop ...)]
      [(_ id (field ...) prop ...)
       (and (identifier? #'id)
            (identifier? #'super-id))
       #`(define-serializable-struct/versions/derived #,stx #f
           id 0 (field ...) () prop ...)]))

)
