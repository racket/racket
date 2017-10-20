#lang racket
(require net/url
         tests/eli-tester)

(require (prefix-in ss: "http-proxy/https-non-server.rkt")
         (prefix-in ps: "http-proxy/proxy-server.rkt"))

(provide tests)
(module+ main (test do (tests)))
(define (tests)

  (define envar-stash (environment-variables-copy (current-environment-variables)))
  
  (define (test-proxy-server-for #:current-proxy-servers [current-proxy-servers-val #f]
                                 #:current-no-proxy-servers [current-no-proxy-servers-val #f]
                                 schema
                                 (host #f)
                                 #:plt-http-proxy (plt-http-proxy #f)
                                 #:http-proxy (http-proxy #f)
                                 #:plt-https-proxy (plt-https-proxy #f)
                                 #:https-proxy (https-proxy #f)
                                 #:plt-git-proxy (plt-git-proxy #f)
                                 #:git-proxy (git-proxy #f)
                                 #:plt-no-proxy (plt-no-proxy #f)
                                 #:no-proxy (no-proxy #f))
    (parameterize ([current-environment-variables envar-stash]
                   [current-namespace (make-base-namespace)])
      (define (put! name val)
        (environment-variables-set! envar-stash
                                    (string->bytes/locale name)
                                    (and val (string->bytes/locale val))))
      (for ((var.val (in-list `(("plt_http_proxy"  . ,plt-http-proxy)
                                ("plt_https_proxy" . ,plt-https-proxy)
                                ("plt_git_proxy"   . ,plt-git-proxy)
                                ("http_proxy"      . ,http-proxy)
                                ("https_proxy"     . ,https-proxy)
                                ("git_proxy"       . ,git-proxy)

                                ("PLT_HTTP_PROXY"  . ,plt-http-proxy)
                                ("PLT_HTTPS_PROXY" . ,plt-https-proxy)
                                ("PLT_GIT_PROXY"   . ,plt-git-proxy)
                                ("HTTP_PROXY"      . ,http-proxy)
                                ("HTTPS_PROXY"     . ,https-proxy)
                                ("GIT_PROXY"       . ,git-proxy)

                                ("plt_no_proxy"    . ,plt-no-proxy)
                                ("no_proxy"        . ,no-proxy)))))
           (put! (car var.val) (cdr var.val)))

      (eval '(require net/url))
      (eval `(parameterize (,@(if current-proxy-servers-val
                                  `([current-proxy-servers (quote ,current-proxy-servers-val)])
                                  null)
                            ,@(if current-no-proxy-servers-val
                                  `([current-no-proxy-servers (quote ,current-no-proxy-servers-val)])
                                  null))
              (proxy-server-for ,schema ,host)))))
  
  (test
   ;; Test the current-proxy-servers parameter can be set
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (current-proxy-servers))
   => '(("http" "proxy.com" 3128))

   ;; we have at least http, https, git
   (member "http" proxiable-url-schemes)
   (member "https" proxiable-url-schemes)
   (member "git" proxiable-url-schemes)

   ;; by default, there are no proxy servers
   (test-proxy-server-for "http") => #f
   (test-proxy-server-for "https") => #f
   (test-proxy-server-for "git") => #f

   ;; current-no-proxy-servers converts incoming strings to anchored regexps
   (parameterize ([current-no-proxy-servers (list "test.racket-lang.org"
                                              #rx".*\\.racket-lang\\.org")])
     (current-no-proxy-servers))
   => '(#rx"^test\\.racket-lang\\.org$"
        #rx".*\\.racket-lang\\.org")

   ;; ------------------------------------------------------------------
   ;; HTTP: Test Proxy Servers (loading from environment and proxy-server-for)
   
   ;; proxy servers set in current-proxy-servers are not overridden by environment
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          #:plt-http-proxy "http://proxy.net:1234"
                          #:http-proxy "http://proxy.net:1234"
                          "http" "test.racket-lang.org")
   => '("http" "proxy.com" 3128)

   ;; plt_http_proxy is is prioritised over http_proxy
   (test-proxy-server-for #:plt-http-proxy "http://proxy.net:3128"
                          #:http-proxy "http://proxy.net:3228"
                          "http" "test.racket-lang.org")
   => '("http" "proxy.net" 3128)

   ;; otherwise fall back to http_proxy
   (test-proxy-server-for #:http-proxy "http://proxy.net:3228"
                          "http" "test.racket-lang.org")
   => '("http" "proxy.net" 3228)

   ;; ------------------------------------------------------------------
   ;; HTTPS: Test Proxy Servers (loading from environment and proxy-server-for)
   
   ;; proxy servers set in current-proxy-servers are not overridden by environment
   (test-proxy-server-for #:current-proxy-servers '(("https" "proxy.com" 3128))
                          #:plt-https-proxy "http://proxy.net:1234"
                          #:https-proxy "http://proxy.net:1234"
                          "https" "test.racket-lang.org")
   => '("https" "proxy.com" 3128)

   ;; plt_https_proxy is is prioritised over https_proxy
   (test-proxy-server-for #:plt-https-proxy "http://proxy.net:3128"
                          #:https-proxy "http://proxy.net:3228"
                          "https" "test.racket-lang.org")
   => '("https" "proxy.net" 3128)

   ;; otherwise fall back to https_proxy
   (test-proxy-server-for #:https-proxy "http://proxy.net:3228"
                          "https" "test.racket-lang.org")
   => '("https" "proxy.net" 3228)

   ;; ------------------------------------------------------------------
   ;; GIT: Test Proxy Servers (loading from environment and proxy-server-for)
   
   ;; proxy servers set in current-proxy-servers are not overridden by environment
   (test-proxy-server-for #:current-proxy-servers '(("git" "proxy.com" 3128))
                          #:plt-git-proxy "http://proxy.net:1234"
                          #:git-proxy "http://proxy.net:1234"
                          "git" "test.racket-lang.org")
   => '("git" "proxy.com" 3128)

   ;; plt_git_proxy is is prioritised over git_proxy
   (test-proxy-server-for #:plt-git-proxy "http://proxy.net:3128"
                          #:git-proxy "http://proxy.net:3228"
                          "git" "test.racket-lang.org")
   => '("git" "proxy.net" 3128)

   ;; otherwise fall back to git_proxy
   (test-proxy-server-for #:git-proxy "http://proxy.net:3228"
                          "git" "test.racket-lang.org")
   => '("git" "proxy.net" 3228)

   ;; ---------------------------------------------------------------------
   ;; Test NO Proxy Servers (loading from environment and proxy-server-for)
   ;; no proxy servers accumulate (they don't override), so test each one
   ;; being inserted in turn

   ;; prove that we need a proxy if not otherwise told...
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          "http" "test.racket-lang.org")
   => '("http" "proxy.com" 3128)
   
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          #:current-no-proxy-servers '("test.racket-lang.org")
                          "http" "test.racket-lang.org")
   => #f
   
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          #:plt-no-proxy "test.racket-lang.org"
                          "http" "test.racket-lang.org")
   => #f
   
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          #:no-proxy "test.racket-lang.org"
                          "http" "test.racket-lang.org")
   => #f

   ;; Pattern matching
   ;; prove that we need a proxy if not otherwise told...
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          #:current-no-proxy-servers '(#rx".racket-lang.org")
                          "http" "test.racket-lang.org")
   => #f
   
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          #:plt-no-proxy ".racket-lang.org"
                          "http" "test.racket-lang.org")
   => #f
   
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          #:no-proxy ".racket-lang.org"
                          "http" "test.racket-lang.org")
   => #f

   ;; Failed matches
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          #:plt-no-proxy ".racket-lang.org"
                          "http" "test.bracket-lang.org")
   => '("http" "proxy.com" 3128)
   
   (test-proxy-server-for #:current-proxy-servers '(("http" "proxy.com" 3128))
                          #:no-proxy ".racket-lang.org"
                          "http" "test.bracket-lang.org")
   => '("http" "proxy.com" 3128)

   ;; Look at this... the no-proxes has a regexp which starts with a '.', a regexp
   ;; any char... that will match the 'b' in bracket
   (test-proxy-server-for    #:current-proxy-servers '(("http" "proxy.com" 3128))
                             #:current-no-proxy-servers '(#rx".racket-lang.org")
                             "http" "test.bracket-lang.org")
   => #f
  )

  (define-values (ss:port ss:server-thread ss:shutdown-server)
    (ss:server))

  (define-values (ps:port ps:server-thread ps:shutdown-server)
    (ps:server))

  (test (parameterize ([current-proxy-servers `(("https" "localhost" ,ps:port))])
          (port->string
           (get-pure-port
            (string->url
             (format "https://localhost:~a/woo/yay"
                     ss:port)))))
        => "\"/woo/yay\" (but at least it's secure)")

  (ps:shutdown-server)
  (ss:shutdown-server))

(module+ test (require (submod ".." main))) ; for raco test & drdr
