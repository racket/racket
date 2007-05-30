(module namespace mzscheme
  (require (lib "kw.ss")
           (lib "list.ss"))
  
  (define default-to-be-copied-module-specs
    '(mzscheme
      (lib "mred.ss" "mred")))
  
  (define/kw (make-make-servlet-namespace
              #:key
              [to-be-copied-module-specs empty])    
    ;; get the names of those modules.
    (define (get-name spec)
      (if (symbol? spec)
          spec
          (with-handlers ([exn? (lambda _ #f)])
            ((current-module-name-resolver) spec #f #f))))
    (define to-be-copied-module-names
      (map get-name 
           (append default-to-be-copied-module-specs
                   to-be-copied-module-specs)))
    (lambda/kw (#:key
                [additional-specs empty])
      (define server-namespace (current-namespace))
      (define new-namespace (make-namespace))
      (define additional-names (map get-name additional-specs))
      (parameterize ([current-namespace new-namespace])
        (for-each (lambda (name)
                    (with-handlers ([exn? void])
                      (when name
                        (namespace-attach-module server-namespace name))))
                  (append to-be-copied-module-names
                          additional-names))
        new-namespace)))
  
  (provide
   make-make-servlet-namespace))