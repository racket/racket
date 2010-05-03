
(for-each (lambda (f)
	    (when (regexp-match "^flat-[0-9]+[.]ss$" (path->string f))
              (let ([ns (current-namespace)])
                (parameterize ([current-namespace (make-base-namespace)]
                               [exit-handler void])
                  (namespace-attach-module ns 'scheme)
                  (namespace-require 'scheme)
                  (eval
                   `(begin
                      (define quiet-load ,(path->string f))
                      (load-relative "quiet.ss")))))))
	  (directory-list))

