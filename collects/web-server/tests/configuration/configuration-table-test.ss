(module configuration-table-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "file.ss")
           (lib "configuration-table.ss" "web-server" "configuration"))
  (provide configuration-table-tests)
  
  (define configuration-table-tests
    (test-suite
     "Configuration Table"
     
     (test-case
      "Default configuration file may be parsed"
      (check-not-false (read-configuration-table default-configuration-table-path)))
     
     (test-case
      "Default configuration file may be written"
      (check-not-false (write-configuration-table 
                        (read-configuration-table default-configuration-table-path)
                        (make-temporary-file))))
     
     (test-case
      "Default configuration file may be converted to sexpr and back"
      (check-not-false
       (sexpr->configuration-table
        (configuration-table->sexpr
         (read-configuration-table
          default-configuration-table-path))))))))