(module kernel '#%kernel

  (#%provide (all-from '#%kernel))

  (#%declare #:cross-phase-persistent)


  (module reader '#%kernel
    (#%module-begin

     (#%require '#%paramz)

     (#%provide (rename lang:read read)
                (rename lang:read-syntax read-syntax)

                get-info
                get-interaction-info)

     (#%declare #:cross-phase-persistent)

     (define-values (the-lang) (quote '#%kernel))


     ;
     ;
     ;                                ;     ;
     ;   ;;;;;;                       ;     ;
     ;   ;    ;;                      ;
     ;   ;     ;  ;;;;     ;;;    ;;; ;   ;;;    ; ;;;    ;;; ;
     ;   ;     ;  ;  ;;   ;   ;   ;  ;;     ;    ;;   ;   ;  ;;
     ;   ;    ;; ;    ;       ;  ;    ;     ;    ;    ;  ;    ;
     ;   ;;;;;   ;;;;;;   ;;;;;  ;    ;     ;    ;    ;  ;    ;
     ;   ;    ;  ;       ;    ;  ;    ;     ;    ;    ;  ;    ;
     ;   ;     ; ;       ;    ;  ;    ;     ;    ;    ;  ;    ;
     ;   ;     ;  ;      ;   ;;   ;  ;;     ;    ;    ;   ;  ;;
     ;   ;      ; ;;;;;   ;;; ;   ;;; ;  ;;;;;;; ;    ;   ;;; ;
     ;                                                        ;
     ;                                                    ;  ;;
     ;                                                     ;;;
     ;


     (define-values (lang:read)
       (lambda (in modpath line col pos)
         (read-and-maybe-wrap #f  ; stx?
                              #f  ; src
                              in
                              modpath
                              line
                              col
                              pos)))

     (define-values (lang:read-syntax)
       (lambda (src in modpath line col pos)
         (read-and-maybe-wrap #t  ; stx?
                              src
                              in
                              modpath
                              line
                              col
                              pos)))

     (define-values (read-and-maybe-wrap)
       (lambda (stx? src port modpath line col pos)

         (define-values (to-stx/maybe)
           (lambda (v loc props)
             (if stx?
                 (datum->syntax #f v loc props)
                 v)))

         (define-values (read-loop)
           (lambda (accum)
             (let-values ([(v) (if stx?
                                   (read-syntax src port)
                                   (read port))])
               (if (eof-object? v)
                   (reverse accum)
                   (read-loop (cons v accum))))))

         (define-values (all-body-forms)
           (letrec-values ([(old-paramz) (continuation-mark-set-first #f parameterization-key)]
                           [(new-paramz) (extend-parameterization old-paramz read-accept-lang #f)])
             (with-continuation-mark
                 parameterization-key new-paramz
               (read-loop null))))

         (define-values (all-loc)
           (vector src line col pos
                   (let-values ([(l c p) (port-next-location port)])
                     (if p
                         (if pos
                             (max 0 (- p pos))
                             #f)
                         #f))))

         (define-values (modpath-loc)
           (vector src line col pos
                   (if pos (max 0
                                (- (if (syntax-position modpath)
                                       (syntax-position modpath)
                                       (add1 pos))
                                   pos)) #f)))

         (let-values ([(body) (maybe-wrap-module-begin all-body-forms)]
                      [(name) (port->module-name port)])
           (to-stx/maybe (list (to-stx/maybe 'module   modpath-loc #f)
                               (to-stx/maybe name      modpath-loc #f)
                               (to-stx/maybe the-lang  modpath     modpath)
                               body)
                      all-loc
                      #f))))

     (define-values (maybe-wrap-module-begin)
       (lambda (body)
         (let-values ([(exprs) (if (syntax? body) (syntax->list body) body)]
                      [(stx?-e) (lambda (v) (if (syntax? v) (syntax-e v) v))])
           (if (if (pair? exprs)
                   (if (null? (cdr exprs))
                       (let-values ([(only-body-form) (stx?-e (car exprs))])
                         (if (pair? only-body-form)
                             (let-values ([(its-head) (stx?-e (car only-body-form))])
                               (eq? its-head '#%module-begin))
                             #f))
                       #f)
                   #f)
               (car exprs)
               (cons '#%module-begin body)))))


     ;
     ;
     ;                           ;                                  ;     ;;;
     ;   ;;;;;;            ;     ;               ;     ;   ;        ;       ;
     ;   ;    ;;           ;     ;               ;     ;   ;                ;
     ;   ;     ;   ;;;   ;;;;;;  ; ;;;           ;     ; ;;;;;;   ;;;       ;     ;;;;
     ;   ;     ;  ;   ;    ;     ;;   ;          ;     ;   ;        ;       ;    ;    ;
     ;   ;    ;;      ;    ;     ;    ;          ;     ;   ;        ;       ;    ;
     ;   ;;;;;;   ;;;;;    ;     ;    ;          ;     ;   ;        ;       ;    ;;;
     ;   ;       ;    ;    ;     ;    ;          ;     ;   ;        ;       ;       ;;;
     ;   ;       ;    ;    ;     ;    ;          ;     ;   ;        ;       ;         ;
     ;   ;       ;   ;;    ;     ;    ;          ;;   ;;   ;        ;       ;    ;    ;
     ;   ;        ;;; ;     ;;;  ;    ;           ;;;;;     ;;;  ;;;;;;;     ;;;  ;;;;
     ;
     ;
     ;
     ;


     (define-values (port->module-name)
       (lambda (port)
         (define-values (p-name) (object-name port))
         (if (path? p-name)
             (let-values ([(base name dir?) (split-path p-name)])
               (string->symbol
                (path->string (strip-extension name))))
             'anonymous-module)))

     ; Simplified version of the function in racket/private/path.rkt
     (define-values (check-extension-call)
       (lambda (s sfx who sep)
         (let-values ([(base name dir?) (split-path s)])
           (if (not base)
               (raise-arguments-error* who 'racket/primitive "cannot add an extension to a root path"
                                       "path" s)
               #t)
           (if (if (eq? name 'same) #t (eq? name 'up))
               (raise-arguments-error* who 'racket/primitive
                                       "cannot add an extension to path that ends with a dot element"
                                       "path" s)
               #t)
           (values base name))))

     ; Simplified version of the function in racket/private/path.rkt
     (define-values (strip-extension)
       (lambda (s)
         (let-values ([(base name) (check-extension-call s #"" 'path-replace-extension #"")])
           (define-values (bs) (path-element->bytes name))
           (define-values (finish)
             (lambda (i i2)
               (bytes->path-element
                (subbytes bs 0 i)
                (if (path-for-some-system? s)
                    (path-convention-type s)
                    (system-path-convention-type)))))
           (let-values ([(new-name) (letrec-values ([(loop)
                                                     (lambda (i)
                                                       (if (zero? i)
                                                           (finish (bytes-length bs) (bytes-length bs))
                                                           (let-values ([(i) (sub1 i)])
                                                             (if (if (not (zero? i))
                                                                     (eq? (char->integer #\.) (bytes-ref bs i))
                                                                     #f)
                                                                 (finish i (add1 i))
                                                                 (loop i)))))])
                                      (loop (bytes-length bs)))])
             (if (path-for-some-system? base)
                 (build-path base new-name)
                 new-name)))))


     ;
     ;
     ;                                              ;               ;;;
     ;   ;                                          ;              ;
     ;   ;                                                         ;
     ;   ;         ;;;   ; ;;;    ;;; ;           ;;;    ; ;;;   ;;;;;;   ;;;;
     ;   ;        ;   ;  ;;   ;   ;  ;;             ;    ;;   ;    ;     ;;  ;;
     ;   ;            ;  ;    ;  ;    ;             ;    ;    ;    ;     ;    ;
     ;   ;        ;;;;;  ;    ;  ;    ;             ;    ;    ;    ;     ;    ;
     ;   ;       ;    ;  ;    ;  ;    ;             ;    ;    ;    ;     ;    ;
     ;   ;       ;    ;  ;    ;  ;    ;             ;    ;    ;    ;     ;    ;
     ;   ;       ;   ;;  ;    ;   ;  ;;             ;    ;    ;    ;     ;;  ;;
     ;   ;;;;;;;  ;;; ;  ;    ;   ;;; ;          ;;;;;;; ;    ;    ;      ;;;;
     ;                                ;
     ;                            ;  ;;
     ;                             ;;;
     ;




     (define-values (get-info)
       (lambda (in modpath line col pos)
         the-language-info))

     (define-values (get-interaction-info)
       (lambda (data)
         the-language-info))

     (define-values (the-language-info)
       (lambda (what defval)
         (if (eq? what 'module-language) the-lang defval))))))
