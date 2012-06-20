#lang scheme/base

(require "stepper-language-interface.rkt"
         "debugger-language-interface.rkt"
         stepper/private/syntax-property
         scheme/class
         scheme/contract
         test-engine/scheme-tests
         (only-in racket/list split-at)
         (only-in racket/sequence sequence->list))

(provide/contract
 [expand-teaching-program (->* (input-port?
                                (-> any/c input-port? any/c)
                                any/c
                                (listof any/c))
                               (symbol? boolean?)
                               any)])

;; this function expands a port providing a program and a bunch of 
;; arguments describing the user environment, and returns a thunk
;; that returns the top-level expressions that make up the expanded
;; program, one on each call.

;; the expanded program generally contains two expressions: a module, and
;; a require. The module includes a 'require' for each teachpack that
;; the user has added. Also, any 'provide' expressions are stripped out.

(define (expand-teaching-program port reader language-module teachpacks [module-name '#%htdp] [enable-testing? #t])
  
  (define state 'init)
  ;; state : 'init => 'require => 'done-or-exn

  ;; in state 'done-or-exn, if this is an exn, we raise it
  ;; otherwise, we just return eof
  (define saved-exn #f)

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
         (define body-exps (suck-all-exps port reader))           
         (define teachpack-requires (teachpacks->requires teachpacks))        
         (rewrite-module
          (expand
           (datum->syntax
            #f
            `(,#'module ,module-name ,language-module 
                        ,@teachpack-requires
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
                        ,@body-exps)))))]
      [(require)
       (set! state 'done-or-exn)
       (stepper-skip
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
                 (current-namespace (module->namespace ''#,module-name)))))))]
      [(done-or-exn)
       (cond
         [saved-exn (raise saved-exn)]
         [else      eof])])))

;; take all of the body expressions from the port
(define (suck-all-exps port reader)
  (define (port-reader p) (parameterize ([read-accept-lang #f])
                            (reader (object-name port) p)))
  (sequence->list (in-port port-reader port)))

;; check that the teachpacks exist, return 
;; syntax objects that require them (tagged
;; with stepper-skip-completely)
(define (teachpacks->requires teachpacks)
  (for/list ([tp (in-list teachpacks)])
    (unless (file-exists? (build-path (apply collection-path (cddr tp))
                                      (cadr tp)))
      (error 'teachpack (missing-tp-message tp)))
    (stepper-skip
     (datum->syntax #f `(require ,tp)))))

(define (missing-tp-message x)
  (let* ([m (regexp-match #rx"/([^/]*)$" (cadr x))]
         [name (if m
                   (cadr m)
                   (cadr x))])
    (format "the teachpack '~a' was not found" name)))


;; rewrite-module : syntax -> syntax
;; rewrites the module to remove provide's (for now...)
(define (rewrite-module stx)
  (syntax-case stx (module #%plain-module-begin)
    [(module name lang (#%plain-module-begin bodies ...))
     (with-syntax ([(rewritten-bodies ...) 
                    (filter not-provide?
                            (syntax->list (syntax (bodies ...))))])
       #`(module name lang
           (#%plain-module-begin 
            rewritten-bodies ...)))]
    [else
     (raise-syntax-error 'htdp-languages "internal error .1")]))


;; not-provide? : syntax -> boolean
;; return #t for expressions that are not 'provide's
(define (not-provide? stx)
  (syntax-case stx (#%provide)
    [(#%provide specs ...) #f]
    [else                  #t]))

;; stepper-skip : syntax -> syntax
;; tag the expression with stepper-skip-completely
(define (stepper-skip stx)
  (stepper-syntax-property stx 'stepper-skip-completely #t))