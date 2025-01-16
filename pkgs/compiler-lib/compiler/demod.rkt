#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     compiler/cm-accomplice
                     syntax/modcode))

(provide (rename-out
          [module-begin #%module-begin]))

(module+ module-begin
  (provide (for-syntax demod-module-begin)))
 
(module reader syntax/module-reader
  compiler/demod)

(define-syntax (module-begin stx)
  (demod-module-begin stx))

(begin-for-syntax
  (define-syntax-class mc-tag
    (pattern #:module)
    (pattern #:dir)
    (pattern #:collect)))

(define-for-syntax (demod-module-begin stx)
  (syntax-parse stx
    [(_ mod-path
        (~alt (~optional (~and disable #:no-demod))
              (~optional (~seq (~and mode (~or #:exe #:dynamic #:static))))
              (~optional (~seq #:include ((~seq include-tag:mc-tag include-path) ...)))
              (~optional (~seq #:exclude ((~seq exclude-tag:mc-tag exclude-path) ...))
                         #:defaults ([(exclude-tag 1) '()]
                                     [(exclude-path 1) '()]))
              (~optional (~seq #:submodule-include (include-submod ...)))
              (~optional (~seq #:submodule-exclude (exclude-submod ...))
                         #:defaults ([(exclude-submod 1) '()]))
              (~optional (~and prune-definitions #:prune-definitions))
              (~optional (~seq #:dump dump-demod))
              (~optional (~seq #:dump-mi dump-mi-demod)))
        ...)
     (unless (module-path? (syntax->datum #'mod-path))
       (raise-syntax-error #f "not a module path" stx #'mod-path))
     (define (convert-inex tags paths)
       (for/list ([tag (in-list (syntax->datum tags))]
                  [path (in-list (syntax->list paths))])
         (case tag
           [(#:module)
            (unless (module-path? (syntax->datum path))
              (raise-syntax-error #f "not a module path" stx path))
            (define name
              (resolved-module-path-name
               (module-path-index-resolve
                (module-path-index-join (syntax->datum path) #f))))
            (unless (path? name)
              (raise-syntax-error #f "not a file module path" stx path))
            (list 'module name)]
           [(#:dir)
            (unless (path-string? (syntax-e path))
              (raise-syntax-error #f "not a path" stx path))
            (list 'dir (path->complete-path (syntax-e path)
                                            (or (current-load-relative-directory)
                                                (current-directory))))]
           [(#:collect)
            (unless (and (string? (syntax-e path))
                         (module-path? `(lib "x.rkt" ,(syntax-e path))))
              (raise-syntax-error #f "not a collection path" stx path))
            (list 'collect (syntax-e path))])))
     (define includes
       (and (attribute include-tag)
            (convert-inex #'(include-tag ...)
                          #'(include-path ...))))
     (define excludes
       (convert-inex #'(exclude-tag ...)
                     #'(exclude-path ...)))
     (define src-module (resolved-module-path-name
                         (module-path-index-resolve
                          (module-path-index-join (syntax->datum #'mod-path) #f)
                          ;; loading maybe trigger compilation and register
                          ;; a cm dependency:
                          #t)))
     (define (convert-submods submods)
       (for/list ([submod-stx (in-list (syntax->list submods))])
         (define submod (syntax->datum submod-stx))
         (cond
           [(symbol? submod) (list submod)]
           [(and (pair? submod) (list? submod) (andmap symbol? submod)) submod]
           [else (raise-syntax-error #f "not a submodule specification" stx submod-stx)])))
     (define include-submodules (if (attribute include-submod)
                                    (convert-submods #'(include-submod ...))
                                    (if (and (attribute mode)
                                             (eq? (syntax-e #'mode) '#:exe))
                                        '((main) (configure-runtime))
                                        #f)))
     (define exclude-submodules (convert-submods #'(exclude-submod ...)))
     (cond
       [(or (attribute disable)
            (not (syntax-local-compiling-module?)))
        (define compiled (get-module-code src-module))
        #`(#%module-begin
           (require mod-path)
           (provide (all-from-out mod-path))
           #,@(let loop ([compiled compiled] [supers '()])
                (for/list ([submod (in-list (append (module-compiled-submodules compiled #t)
                                                    (module-compiled-submodules compiled #f)))])
                  (define name (list-ref (module-compiled-name submod) (add1 (length supers))))
                  #`(module #,name racket/base
                      (require (submod mod-path #,@supers #,name))
                      (provide (all-from-out (submod mod-path #,@supers #,name)))
                      #,@(loop submod (append supers (list name)))))))]
       [else
        (define (get sym)
          (dynamic-require 'compiler/demodularizer/main sym))
        (define demodularize (get 'demodularize))
        (define syntax-object-preservation-enabled (get 'syntax-object-preservation-enabled))
        (register-external-module (collection-file-path "main.rkt" "compiler/demodularizer"))
        (define bundle
          (demodularize src-module
                        #:keep-syntax? (or (not (attribute mode))
                                           (not (eq? (syntax-e #'mode) '#:exe)))
                        #:external-singetons? (or (not (attribute mode))
                                                  (eq? (syntax-e #'mode) '#:dynamic))
                        #:work-directory (build-path (or (current-load-relative-directory)
                                                         (current-directory))
                                                     "compiled/demod")
                        #:includes includes
                        #:excludes excludes
                        #:include-submodules include-submodules
                        #:exclude-submodules exclude-submodules
                        #:prune-definitions? (and (attribute prune-definitions) #t)
                        #:dump-output-file (and (attribute dump-demod) (syntax->datum #'dump-demod))
                        #:return-bundle? #t))
        (register-external-module src-module)
        (when (attribute dump-mi-demod)
          (with-output-to-file (syntax->datum #'dump-mi-demod) #:exists 'truncate (lambda () (write bundle))))
        (datum->syntax #f bundle)])]))
