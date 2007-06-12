(module all-managers-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide all-managers-tests)
  
  (define all-managers-tests
    (test-suite
     "Continuation Managers"
     
     ; XXX test timeout.ss
     ; XXX test none.ss
     ; XXX test lru.ss
     )))