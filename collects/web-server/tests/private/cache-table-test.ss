(module cache-table-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "cache-table.ss" "web-server" "private"))
  (provide cache-table-tests)
  
  (define cache-table-tests
    (test-suite
     "Cache Table"
     
     (test-case
      "Can construct cache-table"
      (check-not-false (make-cache-table)))
     
     (test-case
      "make-cache-table returns cache-table?"
      (check-true (cache-table? (make-cache-table))))
     
     (test-case
      "cache-table? does not recognize hash-tables"
      (check-false (cache-table? (make-hash-table))))
     
     (test-case
      "cache-table-lookup: simple"
      (check-true (cache-table-lookup! (make-cache-table) 'foo (lambda () #t))))
     
     (test-case
      "cache-table-lookup: complicated"
      (check-true (let ([ct (make-cache-table)])
                    (cache-table-lookup! ct 'foo (lambda () #t))
                    (cache-table-lookup! ct 'foo (lambda () #f)))))
     
     (test-case
      "cache-table-clear! is effective"
      (check-false (let ([ct (make-cache-table)])
                     (cache-table-lookup! ct 'foo (lambda () #t))
                     (cache-table-clear! ct)
                     (cache-table-lookup! ct 'foo (lambda () #f))))))))