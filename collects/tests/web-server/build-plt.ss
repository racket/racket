(module build-plt mzscheme
  (require (lib "pack.ss" "setup")
           (lib "date.ss")
           "suite.ss")
  
  (define web-root (build-path "collects" "web-server" "default-web-root"))
  
  (define exclude
    (list (build-path web-root "servlets" "tests")
          (build-path web-root "htdocs" "secret")
          (build-path web-root "passwords")
          (build-path web-root "log")))
  
  (let ([why-broken (broken? 8180)])
    (when why-broken
      (error 'build-plt "The web server is broken~n~a" why-broken)))
  
  (current-directory (build-path (collection-path "web-server") 'up 'up))
  (pack "/home/ptg/.www/packages/web-server.plt"
        (format "Web Server: ~a" (date->string (seconds->date (current-seconds))))
        (list (build-path "collects" "web-server"))
        '(("web-server"))
        (lambda (p)
          (and (not (member p exclude))
               (std-filter p)))))
