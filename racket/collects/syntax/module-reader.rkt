(module module-reader racket/private/base
  (require syntax/readerr
           (for-syntax racket/private/base))

  (provide (rename-out [provide-module-reader #%module-begin]
                       [wrap wrap-read-all])
           make-meta-reader
           (except-out (all-from-out racket/private/base) #%module-begin))

  (define ar? procedure-arity-includes?)

  (define-syntax (provide-module-reader stx)
    (define (err str [sub #f])
      (raise-syntax-error 'syntax/module-reader str sub))
    (define-syntax-rule (keywords body [kwd var default] ... [checks ...])
      (begin
        (define var #f) ...
        (set! body
              (let loop ([body body])
                (if (not (and (pair? body)
                              (pair? (cdr body))
                              (keyword? (syntax-e (car body)))))
                  (datum->syntax stx body stx)
                  (let* ([k (car body)] [k* (syntax-e k)] [v (cadr body)])
                    (case k*
                      [(kwd) (if var
                               (err (format "got two ~s keywords" k*) k)
                               (begin (set! var v) (loop (cddr body))))]
                      ...
                      [else (err "got an unknown keyword" (car body))])))))
        checks ...
        (unless var (set! var default)) ...))
    (define <lang-id> (datum->syntax stx 'language-module stx))
    (define <data-id> (datum->syntax stx 'language-data   stx))
    (define (construct-reader lang body)
      (keywords body
        [#:language            ~lang                lang]
        [#:read                ~read                #'read]
        [#:read-syntax         ~read-syntax         #'read-syntax]
        [#:wrapper1            ~wrapper1            #'#f]
        [#:wrapper2            ~wrapper2            #'#f]
        [#:whole-body-readers? ~whole-body-readers? #'#f]
        [#:info                ~info                #'#f]
        [#:language-info       ~module-get-info     #'#f]
        [(when (equal? (and lang #t) (and ~lang #t))
           (err (string-append
                 "must specify either a module language, or #:language"
                 (if (and lang ~lang) ", not both" ""))))
         (unless (equal? (and ~read #t) (and ~read-syntax #t))
           (err "must specify either both #:read and #:read-syntax, or none"))
         (when (and ~whole-body-readers? (not (and ~read ~read-syntax)))
           (err "got a #:whole-body-readers? without #:read and #:read-syntax"))])
      ;; FIXME: some generated code is constant and should be lifted out of the
      ;; template:
      (quasisyntax/loc stx
        (#%module-begin
         #,@body
         (#%provide (rename lang:read read)
                    (rename lang:read-syntax read-syntax)
                    get-info)
         (define (lang:read in modpath line col pos)
           (wrap-internal/wrapper #f #f in modpath line col pos))
         (define (lang:read-syntax src in modpath line col pos)
           (wrap-internal/wrapper #t src in modpath line col pos))
         (define (wrap-internal/wrapper stx? src in modpath line col pos)
           (let* ([props (read-properties in modpath line col pos)]
                  [lang (car  props)] [#,<lang-id> lang] ;\ visible in
                  [data (cadr props)] [#,<data-id> data] ;/ user-code
                  [read (if stx?
                          (let ([rd #,~read-syntax])
                            (lambda (in) (rd src in)))
                          #,~read)]
                  [w1 #,~wrapper1]
                  [w2 #,~wrapper2]
                  [whole? #,~whole-body-readers?]
                  [rd (lambda (in)
                        (wrap-internal (if (and (not stx?) (syntax? lang))
                                         (syntax->datum lang)
                                         lang)
                                       in read whole? w1 stx?
                                       modpath src line col pos))]
                  [r (cond [(not w2) (rd in)]
                           [(ar? w2 3) (w2 in rd stx?)]
                           [else (w2 in rd)])])
             (if stx?
                 (let ([prop #,(if (syntax-e ~module-get-info)
                                   ~module-get-info
                                   #'#f)])
                   (if prop 
                       (syntax-property r 'module-language prop)
                       r))
                 r)))
         (define read-properties (lang->read-properties #,~lang))
         (define (get-info in modpath line col pos)
           (get-info-getter (read-properties in modpath line col pos)))
         (define (get-info-getter props)
           (define lang (car  props))
           (define data (cadr props))
           (define (default-info what defval)
             (case what
               [(module-language) lang]
               ;; ... more?
               [else defval]))
           (define info
             (let* ([#,<lang-id> lang] ;\ visible in
                    [#,<data-id> data] ;/ user-code
                    [info #,~info])
               (if (or (not info) (and (procedure? info) (ar? info 3)))
                 info
                 (raise-type-error 'syntax/module-reader
                                   "info procedure of 3 arguments" info))))
           (define (language-info what defval)
             (if info
               (let ([r (info what defval default-info)])
                 (if (eq? r default-info) (default-info what defval) r))
               (default-info what defval)))
           language-info))))
    (syntax-case stx ()
      [(_ lang body ...)
       (not (keyword? (syntax-e #'lang)))
       (construct-reader #''lang (syntax->list #'(body ...)))]
      [(_ body ...) (construct-reader #f (syntax->list #'(body ...)))]))

  ;; turns the language specification (either a language or some flavor of a
  ;; function that returns a language and some properties) into a function that
  ;; returns (list <lang> <props>)
  (define (lang->read-properties lang)
    (define lang*
      (cond [(not (procedure? lang)) (list lang #f)]
            [(ar? lang 5) lang]
            [(ar? lang 1) (lambda (in . _) (lang in))]
            [(ar? lang 0) (lambda _ (lang))]
            [else (raise-type-error
                   'syntax/module-reader
                   "language+reader procedure of 5, 1, or 0 arguments"
                   lang)]))
    (define (read-properties in modpath line col pos)
      (if (not (procedure? lang*))
        lang*
        (parameterize ([current-input-port in])
          (call-with-values
              (lambda () (lang* in modpath line col pos))
              (lambda xs
                (case (length xs)
                  [(2) xs] [(1) (list (car xs) #f)]
                  [else (error 'syntax/module-reader
                               "wrong number of results from ~a, ~a ~e"
                               "the #:language function"
                               "expected 1 or 2 values, got"
                               (length xs))]))))))
    read-properties)

  ;; Since there are users that wrap with `#%module-begin' in their reader
  ;;   or wrapper1 functions, we need to avoid double-wrapping.  Having to do
  ;;   this for #lang readers should be considered deprecated, and hopefully
  ;;   one day we'll move to just doing it unilaterally (making this code throw
  ;;   an error in that case before that's done).
  ;; This function takes "body" as a sequence of expressions (can be syntaxes
  ;;   and/or sexprs) and returns a new body as a *single* expression that is
  ;;   wrapped in a `#%module-begin' -- using the input if it was a single
  ;;   pre-wrapped expression.
  (define (wrap-module-begin body)
    (let ([exprs (if (syntax? body) (syntax->list body) body)])
      (if (and (pair? exprs) (null? (cdr exprs))
               (let* ([x (car exprs)]
                      [x (if (syntax? x) (syntax-e x) x)]
                      [x (and (pair? x) (car x))]
                      [x (if (syntax? x) (syntax-e x) x)])
                 (eq? x '#%module-begin)))
        (car exprs)
        (cons '#%module-begin body))))

  (define (wrap-internal lang port read whole? wrapper stx?
                         modpath src line col pos)
    (let* ([lang (if stx? (datum->syntax #f lang modpath modpath) lang)]
           [body (lambda ()
                   (if whole?
                       (read port)
                       (parameterize ([read-accept-lang #f])
                         (let loop ([a null])
                           (let ([v (read port)])
                             (if (eof-object? v)
                                 (reverse a)
                                 (loop (cons v a))))))))]
           [body (cond [(not wrapper)   (body)]
                       [(ar? wrapper 2) (wrapper body stx?)]
                       [else            (wrapper body)])]
           [body (wrap-module-begin body)]
           [all-loc (vector src line col pos
                            (let-values ([(l c p) (port-next-location port)])
                              (and p pos (max 0 (- p pos)))))]
           [p-name (object-name port)]
           [name (if (path? p-name)
                   (let-values ([(base name dir?) (split-path p-name)])
                     (string->symbol
                      (path->string (path-replace-suffix name #""))))
                   'anonymous-module)]
           [tag-src (lambda (v)
                      (if stx?
                        (datum->syntax
                         #f v (vector src line col pos
                                      (and pos (max 0
                                                    (- (or (syntax-position modpath)
                                                           (add1 pos))
                                                       pos)))))
                        v))]
           [r `(,(tag-src 'module) ,(tag-src name) ,lang ,body)])
      (if stx? (datum->syntax #f r all-loc) r)))

  (define (wrap lang port read modpath src line col pos)
    (wrap-internal lang port read #f #f #f modpath src line col pos))

  (define (make-meta-reader
           self-sym module-path-desc spec->module-path
           convert-read
           convert-read-syntax
           convert-get-info
           #:read-spec
           [read-spec
            (lambda (in)
              (let ([spec (regexp-try-match #px"^[ \t]+(.*?)(?=\\s|$)" in)]) ;; if this changes, the regexp in planet's lang/reader.rkt must also change
                (and spec (let ([s (cadr spec)])
                            (if (equal? s "") #f s)))))])
    (define (get in export-sym src line col pos spec-as-stx? mk-fail-thunk)
      (define (bad str eof?)
        ((if eof? raise-read-eof-error raise-read-error)
         (let ([msg (format "bad ~a following ~a" module-path-desc self-sym)])
           (if str (format "~a: ~a" msg str) msg))
         src line col pos
         (let-values ([(line col pos2) (port-next-location in)])
           (and pos pos2 (- pos2 pos)))))
      (let*-values ([(spec-line spec-col spec-pos) (port-next-location in)]
                    [(spec) (read-spec in)]
                    [(spec-end-line spec-end-col spec-end-pos) (port-next-location in)])
        (if (not spec)
            (bad #f (eof-object? (peek-byte in)))
            (let ([parsed-spec (spec->module-path spec)])
              (if parsed-spec
                  (let loop ([specs (if (vector? parsed-spec)
                                        (vector->list parsed-spec)
                                        (list parsed-spec))])
                    (define parsed-spec (car specs))
                    (define guarded-spec ((current-reader-guard) parsed-spec))
                    (if (or (null? (cdr specs))
                            (module-declared? guarded-spec #t))
                        (values
                         (dynamic-require guarded-spec export-sym
                                          (mk-fail-thunk spec))
                         (if spec-as-stx?
                             (datum->syntax #f
                                            guarded-spec
                                            (vector src spec-line spec-col spec-pos
                                                    (max 0 (- spec-end-pos spec-pos))))
                             guarded-spec))
                        (loop (cdr specs))))
                  (bad spec #f))))))

    (define (-get-info inp mod line col pos)
      (let-values ([(r next-mod)
                    (get inp 'get-info (object-name inp) line col pos #f
                         (lambda (spec)
                           (lambda ()
                             (lambda (inp mod line col pos)
                               (lambda (tag defval) defval)))))])
        (convert-get-info (r inp next-mod line col pos))))

    (define (read-fn in read-sym args src mod line col pos convert)
      (let-values ([(r next-mod)
                    (get in read-sym src #|mod|# line col pos
                         (eq? read-sym 'read-syntax)
                         (lambda (spec)
                           (lambda ()
                             (error read-sym "cannot find reader for `#lang ~a ~s'"
                                    self-sym
                                    spec))))])
        (let ([r (convert r)])
          (if (and (procedure? r)
                   (procedure-arity-includes? r (+ 5 (length args))))
            (apply r (append args (list in next-mod line col pos)))
            (apply r (append args (list in)))))))

    (define (-read inp mod line col pos)
      (read-fn inp 'read null (object-name inp) mod line col pos
               convert-read))

    (define (-read-syntax src inp mod line col pos)
      (read-fn inp 'read-syntax (list src) src mod line col pos
               convert-read-syntax))

    (values -read -read-syntax -get-info)))
