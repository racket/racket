;; Mike Burns, July 28th, 2004, netgeek@speakeasy.net
;; Test if the Web server can start and stop via the command line.
;; Not sure if this is needed, but it was in Paul's.
(module test-web-server-process mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "process.ss"))

  (provide test-web-server-process)

  (define the-configuration-file "configuration-table")

  (define the-port 8135)

  ;;; TODO resurrect
  (define test-web-server-process
    (make-test-suite
      "Start and stop the Web server via the command line"
        ;; Start it
        (let-values (((mz-subprocess mz-out mz-in mz-err)
                      (subprocess #f #f #f
                                  (find-executable-path "web-server" #f)
                                  "-p" (number->string the-port)
                                  "-f" the-configuration-file)))
          (sleep 5)
          (make-test-case
            "Start the Web server on a port with a configuration file"
            ;; Test it
            (and (assert-eq? 'running (subprocess-status mz-subprocess))
                 (assert-false (char-ready? mz-err)))
            ;; Kill it
            (let ((pid (subprocess-pid mz-subprocess))
                  (kill-path (find-executable-path "kill" #f)))
              (unless (or (zero? pid) (not kill-path))
                (let-values (((kill-subprocess kill-out kill-in kill-err)
                              (subprocess #f #f #f kill-path (number->string pid))))
                  (close-input-port kill-out)
                  (close-output-port kill-in)
                  (close-input-port kill-err)
                  (subprocess-wait kill-subprocess))))))))
  )
