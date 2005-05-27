;; Mike Burns, July 26th, 2004, netgeek@speakeasy.net
;; Test the ability to start and stop the server via the library.
(module test-web-server mzscheme
  (require (lib "web-server.ss" "web-server")
           (lib "configuration.ss" "web-server")
           (lib "test.ss" "schemeunit")
           (lib "etc.ss"))

  (provide test-web-server)

  (define the-configuration
    ;;; TODO: test load-configuration
    (load-configuration (expand-path "configuration-table")))

  (define the-port 8135)
  
  (define the-ip "127.0.0.1")

  (define test-web-server
    (make-test-suite
      "Start and stop the Web server from the library"
      (let ((stop #f))
        (make-test-case
          "Start the Web server with just the configuration"
          (assert-pred procedure? (begin
                                    (set! stop (serve the-configuration))
                                    stop))
          (when stop (stop))))
      (let ((stop #f))
        (make-test-case
          "Start the Web server with the configuration and port"
          (assert-pred procedure? (begin
                                    (set! stop (serve the-configuration
                                                      the-port))
                                    stop))
          (when stop (stop))))
      (let ((stop #f))
        (make-test-case
          "Start the Web server with the configuration, port, and IP address"
          (assert-pred procedure? (begin
                                    (set! stop (serve the-configuration
                                                      the-port the-ip))
                                    stop))
          (when stop (stop))))))

  )
