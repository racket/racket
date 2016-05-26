(module path-list '#%kernel
  (#%require "qq-and-or.rkt"
             "cond.rkt"
             "define-et-al.rkt"
             "path.rkt"
             "path-list.rkt")
  
  (#%provide find-executable-path)

  (define-values (find-executable-path)
    (case-lambda 
      [(program libpath reverse?)
       (unless (path-string? program) 
         (raise-argument-error 'find-executable-path "path-string?" program))
       (unless (or (not libpath) (and (path-string? libpath) 
                                      (relative-path? libpath)))
         (raise-argument-error 'find-executable-path "(or/c #f (and/c path-string? relative-path?))" libpath))
       (letrec ([found-exec
                 (lambda (exec-name)
                   (if libpath
                       (let-values ([(base name isdir?) (split-path exec-name)])
                         (let ([next
                                (lambda ()
                                  (let ([resolved (resolve-path exec-name)])
                                    (cond
                                     [(equal? resolved exec-name) #f]
                                     [(relative-path? resolved)
                                      (found-exec (build-path base resolved))]
                                     [else (found-exec resolved)])))])
                           (or (and reverse? (next))
                               (if (path? base)
                                   (let ([lib (build-path base libpath)])
                                     (and (or (directory-exists? lib) 
                                              (file-exists? lib))
                                          lib))
                                   #f)
                               (and (not reverse?) (next)))))
                       exec-name))])
         (if (and (relative-path? program)
                  (let-values ([(base name dir?) (split-path program)])
                    (eq? base 'relative)))
             (let ([paths-str (environment-variables-ref (current-environment-variables)
                                                         #"PATH")]
                   [win-add (lambda (s) (if (eq? (system-type) 'windows) 
                                       (cons (bytes->path #".") s) 
                                       s))])
               (let loop ([paths (win-add 
                                  (if paths-str
                                      (path-list-string->path-list (bytes->string/locale paths-str #\?)
                                                                   null)
                                      null))])
                 (if (null? paths)
                     #f
                     (let* ([base (path->complete-path (car paths))]
                            [name (build-path base program)])
                       (if (file-exists? name)
                           (found-exec name)
                           (loop (cdr paths)))))))
             (let ([p (path->complete-path program)])
               (and (file-exists? p) (found-exec p)))))]
      [(program libpath) (find-executable-path program libpath #f)]
      [(program) (find-executable-path program #f #f)])))
