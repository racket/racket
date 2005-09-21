(module top mzscheme

  (require (lib "contract.ss")
           (prefix honu: "parsers/parse.ss")
           (prefix honu: "parsers/post-parsing.ss")
           (prefix honu: "private/typechecker/typechecker.ss")
           (prefix honu: "private/compiler/translate.ss")
           (prefix honu: "tenv.ss")
           (prefix honu: "tenv-utils.ss")
           (prefix honu: "parameters.ss")
           (prefix honu: "honu-context.ss")
           "ast.ss"
           )

  (require-for-template (lib "contract.ss"))
  
  (define-syntax (define/provide stx)
    (syntax-case stx ()
        [(_ (NAME ARG ...) BODY ...)
         #`(begin
             (define (NAME ARG ...) BODY ...)
             (provide NAME))]
        [(_ NAME BODY ...)
         #`(begin
             (define NAME BODY ...)
             (provide NAME))]
        ))
  
  (define-syntax (def/pro/con stx)
    (syntax-case stx ()
        [(_ (NAME ARG ...) CONTRACT BODY ...)
         #`(begin
             (define (NAME ARG ...) BODY ...)
             (provide/contract [NAME CONTRACT]))]
        [(_ NAME CONTRACT BODY ...)
         #`(begin
             (define NAME BODY ...)
             (provide/contract [NAME CONTRACT]))]
        ))

  (define type-syntax/c any/c)

  (def/pro/con current-top-tenv parameter? (make-parameter (honu:empty-tenv)))
  (def/pro/con current-top-lenv parameter? (make-parameter (honu:get-builtin-lenv)))

  (def/pro/con (reset-env) (-> void?)
    (current-top-tenv (honu:empty-tenv))
    (current-top-lenv (honu:get-builtin-lenv)))
  
  (define-syntax (with-env stx)
    (syntax-case stx ()
      [(_ BODY ...)
       #`(parameterize ([honu:current-type-environment (current-top-tenv)]
                        [honu:current-lexical-environment (current-top-lenv)])
           BODY ...)]))
  
  (def/pro/con (parse-file file) (path-string? . -> . (listof honu:defn?))
    (with-env
     (honu:post-parse-program
      (honu:add-defns-to-tenv
       (honu:parse-port (open-input-file file) file)))))

  (def/pro/con (check-defns program) ((listof honu:defn?) . -> . (listof honu:defn?))
    (with-env (honu:typecheck program)))

  (def/pro/con (translate-defns program)
    ((listof honu:defn?) . -> . (syntax/c any/c))
    (with-env
     (parameterize ([honu:current-compile-context honu:honu-compile-context])
       (let-values
           ([(annotations syntax) (honu:translate program)])
         (namespace-syntax-introduce (datum->syntax-object #f (cons 'begin syntax) #f))))))

  (def/pro/con (run-program file) (path-string? . -> . void?)
    (reset-env)
    (eval-syntax (translate-defns (check-defns (parse-file file)))))
  
  )