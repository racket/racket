#lang scheme/base
(require (planet schematics/schemeunit:3))
(provide all-managers-tests)

(define all-managers-tests
  (test-suite
   "Continuation Managers"
   
   ; XXX test timeout.ss
   ; XXX test none.ss
   ; XXX test lru.ss
   ))
