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

  (define/c current-tenv parameter? (make-parameter (empty-tenv)))
  (define/c current-lenv parameter? (make-parameter (get-builtin-lenv)))

  (define/c (reset-env) (-> void?)
    (current-tenv (empty-tenv))
    (current-lenv (get-builtin-lenv)))

  (define-syntax (with-env stx)
    (syntax-case stx ()
      [(_ BODY ...)
       #`(parameterize ([current-type-environment (current-tenv)]
                        [current-lexical-environment (current-lenv)])
           BODY ...)]))

  (define-syntax (with-context stx)
    (syntax-case stx ()
      [(_ BODY ...)
       #`(parameterize ([current-compile-context honu-compile-context])
           BODY ...)]))

  (define/c (ast-from-file file) (path-string? . -> . (listof honu:defn?))
    (with-env
     (post-parse-program
      (add-defns-to-tenv
       (parse-port (open-input-file file) file)))))

  (define/c (check-defns defns) ((listof honu:defn?) . -> . (listof honu:defn?))
    (with-env (typecheck defns)))

  (define/c (translate-defns defns) ((listof honu:defn?) . -> . (syntax/c any/c))
    (with-env
     (with-context
       (let-values
           ([(annotations syntax) (translate defns)])
         (namespace-syntax-introduce (datum->syntax-object #f (cons 'begin syntax) #f))))))

  (define (lenv-names)
    (let* ([lenv (current-lenv)]
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
    (let* ([tenv (current-tenv)])
      (bound-identifier-mapping-map tenv tenv:entry-mangled-name)))

  (define/p (eval-after-program file stx) (path-string? syntax? . -> . any)
    (reset-env)
    (let* ([ast (ast-from-file file)]
           [ast (check-defns ast)]
           [defs (translate-defns ast)])
      (eval
       #`(begin #,defs #,stx))))

  (define/c (run-program file) (path-string? . -> . (values (listof symbol?) (listof symbol?)))
    (reset-env)
    (eval-syntax (translate-defns (check-defns (ast-from-file file))))
    (values (tenv-names) (lenv-names)))

  (define/c (run-programs files)
    ((listof path-string?) . -> . (values (listof (listof symbol?)) (listof (listof symbol?))))
    (map-values run-program files))

  (define (program-syntax file)
    (let* ([port (open-input-file file)])
      #`(begin
          #,@(let read-loop
                 ([sexps (list)]
                  [input (read-syntax file port)])
               (if (eof-object? input)
                   (reverse sexps)
                   (read-loop (cons input sexps) (read-syntax file port)))))))
  
  (define/c (test-file file) (path-string? . -> . any)
    (with-handlers
        ([exn:fail? (lambda (exn) `(error ,(exn-message exn)))])
      (let* ([honu-path (if (path? file) file (string->path file))]
             [test-path (path-replace-suffix honu-path "-test.ss")])
        (unless (file-exists? honu-path)
          (error 'test-file "~s not found" (path->string honu-path)))
        (unless (file-exists? test-path)
          (error 'test-file "~s not found" (path->string test-path)))
        (let* ([stx (program-syntax test-path)])
          (eval-after-program
           honu-path
           #`(begin
               (require (lib "test-tools.ss" "honu"))
               #,stx))))))
  
  )
