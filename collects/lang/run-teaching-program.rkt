#lang scheme/base

(require "stepper-language-interface.rkt"
         "debugger-language-interface.rkt"
         stepper/private/shared
         scheme/class
         scheme/contract
         test-engine/scheme-tests)

(provide/contract
 [expand-teaching-program (->* (input-port?
                                (-> any/c input-port? any/c)
                                any/c
                                (listof any/c)
                                (or/c false/c (object-contract [display-results/void (-> (listof any/c) any)])))
                               (symbol? boolean?)
                               any)])

;; this function expands a port providing a program and a bunch of 
;; arguments describing the user environment, and returns a thunk
;; that returns the top-level expressions that make up the expanded
;; program, one on each call.
(define (expand-teaching-program port reader language-module teachpacks rep [module-name '#%htdp] [enable-testing? #t])
  (let ([state 'init]
        ;; state : 'init => 'require => 'done-or-exn
        
        ;; in state 'done-or-exn, if this is an exn, we raise it
        ;; otherwise, we just return eof
        [saved-exn #f])
    
    (lambda ()
      (case state
        [(init)
         (set! state 'require)
         (with-handlers ([exn:fail?
                          (λ (x)
                            (set! saved-exn x)
                            (expand
                             (datum->syntax
                              #f
                              `(,#'module ,module-name ,language-module 
                                          ,@(map (λ (x) 
                                                   `(require ,x))
                                                 teachpacks)))))])
           (let ([body-exps 
                  (let loop ()
                    (let ([result (reader (object-name port) port)])
                      (if (eof-object? result)
                          null
                          (cons result (loop)))))])
             (for-each
              (λ (tp)
                (with-handlers ((exn:fail? (λ (x) (error 'teachpack (missing-tp-message tp)))))
                  (unless (file-exists? (build-path (apply collection-path (cddr tp))
                                                    (cadr tp)))
                    (error "fail"))))
              teachpacks)
             (rewrite-module
              (expand
               (datum->syntax
                #f
                `(,#'module ,module-name ,language-module 
                            ,@(map (λ (x) `(require ,x)) teachpacks)
                            ,@(if enable-testing?
                                  (if (null? body-exps)
                                      '() 
                                      ;; this definition pulls the test~object binding from the user's namespace
                                      ;; over to the one that is used in the REPL when module->namepsace
                                      ;; grabs a hold of this module to make a namespace for the REPL
                                      `(,(syntax-property
                                          #'(define test~object (namespace-variable-value 'test~object))
                                          'test-call #t)))
                                  '())
                            ,@body-exps)))
              rep)))]
        [(require)
         (set! state 'done-or-exn)
         (stepper-syntax-property
          #`(let ([done-already? #f])
              (dynamic-wind
               void
               (lambda () 
                 (dynamic-require ''#,module-name #f))  ;; work around a bug in dynamic-require
               (lambda () 
                 (unless done-already?
                   (set! done-already? #t)
                   #,(if enable-testing? 
                         #'(test) 
                         #'(begin))
                   (current-namespace (module->namespace ''#,module-name))))))
          'stepper-skip-completely
          #t)]
        [(done-or-exn)
         (cond
           [saved-exn
            (raise saved-exn)]
           [else
            eof])]))))

(define (missing-tp-message x)
  (let* ([m (regexp-match #rx"/([^/]*)$" (cadr x))]
         [name (if m
                   (cadr m)
                   (cadr x))])
    (format "the teachpack '~a' was not found" name)))

;; rewrite-module : settings syntax (is-a?/c interactions-text<%>) -> syntax
;; rewrites the module to print out results of non-definitions
(define (rewrite-module stx rep)
  (syntax-case stx (module #%plain-module-begin)
    [(module name lang (#%plain-module-begin bodies ...))
     (with-syntax ([(rewritten-bodies ...) 
                    (rewrite-bodies (syntax->list (syntax (bodies ...))) rep)])
       #`(module name lang
           (#%plain-module-begin 
            rewritten-bodies ...)))]
    [else
     (raise-syntax-error 'htdp-languages "internal error .1")]))

;; rewrite-bodies : (listof syntax) (is-a?/c interactions-text<%>) -> syntax
(define (rewrite-bodies bodies rep)
  (let loop ([bodies bodies])
    (cond
      [(null? bodies) null]
      [else
       (let ([body (car bodies)])
         (syntax-case body (#%require define-values define-syntaxes define-values-for-syntax #%provide)
           [(define-values (new-vars ...) e)
            (cons body (loop (cdr bodies)))]
           [(define-syntaxes (new-vars ...) e)
            (cons body (loop (cdr bodies)))]
           [(define-values-for-syntax (new-vars ...) e)
            (cons body (loop (cdr bodies)))]
           [(#%require specs ...)
            (cons body (loop (cdr bodies)))]
           [(#%provide specs ...)
            (loop (cdr bodies))]
           [else 
            (cons body (loop (cdr bodies)))]))])))
