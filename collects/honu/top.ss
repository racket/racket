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
           "private/tools/general.ss"
           )

  (provide/contract
   [current-tenv parameter?]
   [current-lenv parameter?]
   )
  
  (define current-tenv (make-parameter (empty-tenv)))
  (define current-lenv (make-parameter (get-builtin-lenv)))

  (provide/contract
   [reset-env (-> void?)]
   )
  
  (define (reset-env)
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

  (provide/contract
   [ast-from-file (path-string? . -> . (listof ast:defn?))]
   [check-defns ((listof ast:defn?) . -> . (listof ast:defn?))]
   [translate-defns ((listof ast:defn?) . -> . (syntax/c any/c))]
   )
  
  (define (ast-from-file file)
    (with-env
     (post-parse-program
      (add-defns-to-tenv
       (parse-port (open-input-file file) file)))))

  (define (check-defns defns)
    (with-env (typecheck defns)))

  (define (translate-defns defns)
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

  (provide/contract
   [eval-after-program (path-string? syntax? . -> . any)]
   [run-program  (path-string? . -> . (values (listof symbol?) (listof symbol?)))]
   [run-programs ((listof path-string?) . -> .
                  (values (listof (listof symbol?)) (listof (listof symbol?))))]
   )
  
  (define (eval-after-program file stx)
    (reset-env)
    (let* ([ast (ast-from-file file)]
           [ast (check-defns ast)]
           [defs (translate-defns ast)])
      (eval
       #`(begin #,defs #,stx))))

  (define (run-program file)
    (reset-env)
    (eval-syntax (translate-defns (check-defns (ast-from-file file))))
    (values (tenv-names) (lenv-names)))

  (define (run-programs files)
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

  (provide/contract
   [test-file (path-string? . -> . any)]
   )
  
  (define (test-file file)
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
             #,stx)))))
  
  )
