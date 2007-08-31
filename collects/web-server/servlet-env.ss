(module servlet-env mzscheme
  (require (prefix net: (lib "sendurl.ss" "net"))
           (lib "list.ss"))
  (require "web-server.ss"
           "configuration/configuration-table.ss"
           "private/util.ss"
           "managers/timeouts.ss"
           "private/servlet.ss"
           "configuration/namespace.ss"
           "private/cache-table.ss"           
           (prefix servlets: "dispatchers/dispatch-servlets.ss"))
  (require "servlet.ss")
  (provide (rename on-web:syntax on-web)
           send-url
           (all-from "servlet.ss"))
  
  (define send-url (make-parameter net:send-url))
  
  ; XXX Change to setup temporary file and special dispatcher  
  (define-syntax (on-web:syntax stx)
    (syntax-case stx ()
      [(on-web:syntax servlet-expr)
       (syntax
        (on-web:syntax 8000 servlet-expr))]
      [(on-web:syntax port servlet-expr)
       (with-syntax ([initial-request (datum->syntax-object (syntax servlet-expr) 'initial-request)])
         (syntax
          (on-web (lambda (initial-request) servlet-expr)
                  port
                  "servlets/standalone.ss")))]))
  
  (define (on-web servlet-expr the-port the-path)
    (let*-values
        ([(standalone-url)
          (format "http://localhost:~a/~a" the-port the-path)]
         [(final-value)
          (void)]
         [(final-conn)
          (void)]
         [(sema)
          (make-semaphore 0)]
         [(make-servlet-namespace) (make-make-servlet-namespace)]
         [(new-servlet)
          (lambda (initial-request)
            (let ([v (servlet-expr initial-request)])
              (set! final-value v)
              (semaphore-post sema)
              (if (response? v)
                  v
                  `(html (head (title "Servlet has ended."))
                         (body (p "This servlet has ended, please return to the interaction window."))))))]
         [(the-scripts) (make-cache-table)]
         [(clear-cache! servlet-dispatch)
          (servlets:make (box the-scripts)
                         #:make-servlet-namespace make-servlet-namespace
                         #:url->path
                         (lambda _
                           (values (build-path (directory-part default-configuration-table-path)
                                         "default-web-root" "."
                                         the-path)
                                   empty)))]
         [(shutdown-server)
          (serve #:dispatch servlet-dispatch
                 #:port the-port)])
      (cache-table-lookup! the-scripts
                           (string->symbol
                            (path->string
                             (build-path (directory-part default-configuration-table-path)
                                         "default-web-root" "."
                                         the-path)))
                           (lambda ()
                             (make-servlet (make-custodian)
                                           (make-servlet-namespace)
                                           (create-timeout-manager
                                            (lambda (request)
                                              `(html (head "Return to the interaction window.")
                                                     (body (p "Return to the interaction window."))))
                                            30 30)
                                           new-servlet)))
      ((send-url) standalone-url #t)
      ; Wait for final call
      (semaphore-wait sema)
      ; XXX: Find a way to wait for final HTML to be sent
      ; Shutdown the server
      (shutdown-server)
      final-value)))