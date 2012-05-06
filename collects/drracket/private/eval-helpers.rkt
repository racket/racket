#lang racket/base
(require racket/class
         racket/draw
         racket/list
         compiler/cm
         setup/dirs
         planet/config
         (prefix-in *** '#%foreign) ;; just to make sure it is here
         )

(provide set-basic-parameters/no-gui
         set-module-language-parameters
         (struct-out prefab-module-settings)
         transform-module
         get-init-dir)

;; get-init-dir : (or/c path? #f) -> path?
;; returns the initial directory for a program
;; that is saved in 'path/f' (with #f indicating
;; an unsaved file)
(define (get-init-dir path/f)
  (cond
    [path/f
     (let-values ([(base name dir?) (split-path path/f)])
       base)]
    [else
     (find-system-path 'home-dir)]))

(struct prefab-module-settings
  (command-line-args
   collection-paths
   compilation-on?
   full-trace?
   annotations)
  #:prefab)

(define orig-namespace (current-namespace))

(define (set-basic-parameters/no-gui)
  (let ([cust (current-custodian)])
    (define (drracket-plain-exit-handler arg)
      (custodian-shutdown-all cust))
    (exit-handler drracket-plain-exit-handler))
  (current-thread-group (make-thread-group))
  (current-command-line-arguments #())
  (current-pseudo-random-generator (make-pseudo-random-generator))
  (current-evt-pseudo-random-generator (make-pseudo-random-generator))
  (read-curly-brace-as-paren #t)
  (read-square-bracket-as-paren #t)
  (error-print-width 250)
  (current-ps-setup (make-object ps-setup%))
  (current-namespace (make-base-empty-namespace))
  ;; is this wise?
  #;(namespace-attach-module orig-namespace ''#%foreign))
  

(define (set-module-language-parameters settings module-language-parallel-lock-client
                                        #:use-use-current-security-guard? [use-current-security-guard? #f])
  (current-command-line-arguments (prefab-module-settings-command-line-args settings))
  (let* ([default (current-library-collection-paths)]
         [cpaths (append-map (λ (x) (if (symbol? x) default (list x)))
                             (prefab-module-settings-collection-paths settings))])
    (when (null? cpaths)
      (eprintf "WARNING: your collection paths are empty!\n"))
    (current-library-collection-paths cpaths))
  
  (compile-context-preservation-enabled (prefab-module-settings-full-trace? settings))
  
  (when (prefab-module-settings-compilation-on? settings)
    (case (prefab-module-settings-annotations settings)
      [(none)
       (use-compiled-file-paths
        (cons (build-path "compiled" "drracket")
              (use-compiled-file-paths)))]
      [(debug)
       (use-compiled-file-paths
        (cons (build-path "compiled" "drracket" "errortrace")
              (use-compiled-file-paths)))])
    (parallel-lock-client module-language-parallel-lock-client)
    (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler 
                                #t
                                #:security-guard (and use-current-security-guard?
                                                      (current-security-guard))))
    (let* ([cd (find-collects-dir)]
           [no-dirs (if cd 
                        (list (CACHE-DIR) cd)
                        (list (CACHE-DIR)))])
      (manager-skip-file-handler
       (λ (p) (file-stamp-in-paths p no-dirs))))))

(define (transform-module filename stx raise-hopeless-syntax-error)
  (define-values (mod name lang body)
    (syntax-case stx ()
      [(module name lang . body)
       (eq? 'module (syntax-e #'module))
       (values #'module #'name #'lang #'body)]
      [_ (raise-hopeless-syntax-error
          (string-append "only a module expression is allowed, either\n"
                         "    #lang <language-name>\n or\n"
                         "    (module <name> <language> ...)\n")
          stx)]))
  (define name* (syntax-e name))
  (unless (symbol? name*)
    (raise-hopeless-syntax-error "bad syntax in name position of module"
                                 stx name))
  (let* (;; rewrite the module to use the racket/base version of `module'
         [mod  (datum->syntax #'here 'module mod)]
         [expr (datum->syntax stx `(,mod ,name ,lang . ,body) stx stx)])
    (values name lang expr)))
