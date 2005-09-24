(module top mzscheme

  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "contract.ss")
           (lib "boundmap.ss" "syntax")
           "parsers/parse.ss"
           "parsers/post-parsing.ss"
           "private/typechecker/type-utils.ss"
           "private/typechecker/typechecker.ss"
           "private/compiler/translate.ss"
           "private/compiler/translate-utils.ss"
           "tenv-utils.ss"
           "parameters.ss"
           "honu-context.ss"
           "ast.ss"
           "tenv.ss"
           "utils.ss"
           )

  (define/c top:current-tenv parameter? (make-parameter (empty-tenv)))
  (define/c top:current-lenv parameter? (make-parameter (get-builtin-lenv)))

  (define/c (top:reset-env) (-> void?)
    (top:current-tenv (empty-tenv))
    (top:current-lenv (get-builtin-lenv)))

  (define-syntax (with-env stx)
    (syntax-case stx ()
      [(_ BODY ...)
       #`(parameterize ([current-type-environment (top:current-tenv)]
                        [current-lexical-environment (top:current-lenv)])
           BODY ...)]))

  (define-syntax (with-context stx)
    (syntax-case stx ()
      [(_ BODY ...)
       #`(parameterize ([current-compile-context honu-compile-context])
           BODY ...)]))

  (define/c (top:parse-file file) (path-string? . -> . (listof honu:defn?))
    (with-env
     (post-parse-program
      (add-defns-to-tenv
       (parse-port (open-input-file file) file)))))

  (define/c (top:check-defns defns) ((listof honu:defn?) . -> . (listof honu:defn?))
    (with-env (typecheck defns)))

  (define/c (top:translate-defns defns) ((listof honu:defn?) . -> . (syntax/c any/c))
    (with-env
     (with-context
       (let-values
           ([(annotations syntax) (translate defns)])
         (namespace-syntax-introduce (datum->syntax-object #f (cons 'begin syntax) #f))))))

  (define (lenv-names)
    (let* ([lenv (top:current-lenv)]
           [orig (get-builtin-lenv)]
           [ids '()])
      (bound-identifier-mapping-for-each
       lenv
       (lambda (id entry)
         (if (bound-identifier-mapping-get orig id (lambda () #f))
             (void)
             (set! ids (cons (syntax-e id) ids)))))
      (reverse! ids)))

  (define (tenv:entry-mangled-name id entry)
    (cond [(tenv:type? entry) (syntax-e (translate-iface-name (make-iface-type id id)))]
          [(tenv:class? entry) (syntax-e (translate-class-name id))]
          [(tenv:mixin? entry) (syntax-e (translate-mixin-name id))]))
  
  (define (tenv-names)
    (let* ([tenv (top:current-tenv)])
      (bound-identifier-mapping-map tenv tenv:entry-mangled-name)))

  (define test<%> (interface ()))
  
  (define (run-test-class-from-name name)
    (let ([def (eval name)])
      (if (and (class? def) (implementation? def test<%>))
          (printf "WILL test ~s [~s]~n" def name)
          (printf "WONT test ~s [~s]~n" def name))))
  
  (define/c (top:run-program file) (path-string? . -> . (values (listof symbol?) (listof symbol?)))
    (top:reset-env)
    (eval-syntax (top:translate-defns (top:check-defns (top:parse-file file))))
    (values (tenv-names) (lenv-names)))

  (define/c (top:run-programs files)
    ((listof path-string?) . -> . (values (listof (listof symbol?)) (listof (listof symbol?))))
    (map-values top:run-program files))
  
  )
