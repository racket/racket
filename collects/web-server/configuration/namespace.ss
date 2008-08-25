#lang scheme/base
(require mzlib/list
         scheme/contract)

(define default-to-be-copied-module-specs '(mzscheme mred))

(define (make-make-servlet-namespace
         #:to-be-copied-module-specs [to-be-copied-module-specs empty])    
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
  (lambda (#:additional-specs [additional-specs empty])
    (define server-namespace (current-namespace))
    (define new-namespace (make-base-empty-namespace))
    (define additional-names (map get-name additional-specs))
    (parameterize ([current-namespace new-namespace])
      (namespace-require 'scheme/base)
      (for-each (lambda (name)
                  (with-handlers ([exn? void])
                    (when name
                      (namespace-attach-module server-namespace name))))
                (append to-be-copied-module-names
                        additional-names))
      new-namespace)))

(define make-servlet-namespace/c
  (->* ()
       (#:additional-specs (listof module-path?))
       namespace?))

(provide/contract
 [make-servlet-namespace/c contract?]
 [make-make-servlet-namespace 
  (->* ()
       (#:to-be-copied-module-specs (listof module-path?))
       make-servlet-namespace/c)])
