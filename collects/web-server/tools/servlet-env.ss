(module servlet-env mzscheme
  (require (lib "sendurl.ss" "net")
           (lib "unit.ss"))
  (require "../configuration.ss"
           "../web-server.ss"
           "../sig.ss"
           "../private/util.ss"
           "../response-structs.ss"
           "../managers/timeouts.ss"
           "../private/servlet.ss"
           "../private/cache-table.ss")
  (require "../servlet.ss")
  (provide (rename on-web:syntax on-web)
           (all-from "../servlet.ss"))
  
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
    (let* ([standalone-url
            (format "http://localhost:~a/~a" the-port the-path)]
           [final-value
            (void)]
           [final-conn
            (void)]
           [sema
            (make-semaphore 0)]
           [new-servlet
            (lambda (initial-request)
              (let ([v (servlet-expr initial-request)])
                (set! final-value v)
                (semaphore-post sema)
                (if (response? v)
                    v
                    `(html (head (title "Servlet has ended."))
                           (body (p "This servlet has ended, please return to the interaction window."))))))]
           [shutdown-server
            (serve (build-standalone-servlet-configuration the-port the-path new-servlet))])
      (send-url standalone-url #t)
      ; Wait for final call
      (semaphore-wait sema)
      ; XXX: Find a way to wait for final HTML to be sent
      ; Shutdown the server
      (shutdown-server)
      final-value))
  
  (define (build-standalone-servlet-configuration the-port the-path the-servlet)
    (let ([basic-configuration@ (load-developer-configuration default-configuration-table-path)]
          [the-scripts (make-cache-table)])
      (define-values/invoke-unit basic-configuration@ 
        (import) 
        (export (prefix i: web-config^)))
      (cache-table-lookup! the-scripts
                           (string->symbol
                            (path->string
                             (build-path (directory-part default-configuration-table-path)
                                         "default-web-root" "."
                                         the-path)))
                           (lambda ()
                             (make-servlet (make-custodian)
                                           (i:make-servlet-namespace)
                                           (create-timeout-manager
                                             (lambda (request)
                                             `(html (head "Return to the interaction window.")
                                                    (body (p "Return to the interaction window."))))
                                             30 30)
                                           the-servlet)))
      (unit
        (import)
        (export web-config^)
        (define port the-port)
        (define max-waiting i:max-waiting)
        (define listen-ip i:listen-ip)
        (define initial-connection-timeout i:initial-connection-timeout)
        (define virtual-hosts i:virtual-hosts)
        (define access i:access)
        (define instances i:instances)
        (define scripts (box the-scripts))
        (define make-servlet-namespace i:make-servlet-namespace)))))