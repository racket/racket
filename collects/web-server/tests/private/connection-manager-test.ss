(module connection-manager-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "connection-manager.ss" "web-server" "private"))
  (provide connection-manager-tests)
  
  (start-connection-manager (current-custodian))
  
  (define connection-manager-tests
    (test-suite
     "Connection Manager"
     
     ; (tests rely on timing and may fail for that reason)
     (test-case
      "Input closed"
      (check-true
       (let ([ib (open-input-bytes #"")]
             [ob (open-output-bytes)])
         (new-connection 1 ib ob (current-custodian) #t)
         (sleep 2)
         (with-handlers ([exn? (lambda _ #t)])
           (read ib) #f))))
     
     (test-case
      "Output closed"
      (check-true
       (let ([ib (open-input-bytes #"")]
             [ob (open-output-bytes)])
         (new-connection 1 ib ob (current-custodian) #t)
         (sleep 2)
         (with-handlers ([exn? (lambda _ #t)])
           (write 1 ob) #f))))
     
     (test-case
      "Early kill"
      (check-true
       (let* ([ib (open-input-bytes #"")]
              [ob (open-output-bytes)]
              [c (new-connection 1 ib ob (current-custodian) #t)])
         (kill-connection! c)
         (and (with-handlers ([exn? (lambda _ #t)])
                (read ib) #f)
              (with-handlers ([exn? (lambda _ #t)])
                (write 1 ob) #f)))))
     
     (test-case
      "Extend timer"
      (check-true
       (let* ([ib (open-input-bytes #"")]
              [ob (open-output-bytes)]
              [c (new-connection 1 ib ob (current-custodian) #t)])
         (adjust-connection-timeout! c 1)
         (sleep 2)
         (and (with-handlers ([exn? (lambda _ #t)])
                (read ib) #f)
              (with-handlers ([exn? (lambda _ #t)])
                (write 1 ob) #f))))))))