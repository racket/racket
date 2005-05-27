(module go-check mzscheme
  (require (lib "unitsig.ss") "checksigs.ss" "runcheck.ss")
  (provide go-check)
  (define (go-check frame the-sync defs@)
    (let ([extra-params@ (unit/sig extra-params^ (import)
                           (define check-frame frame)
                           (define sync? the-sync))])
      (invoke-unit/sig
       (compound-unit/sig
        (import)
        (link
         [CFRAME : extra-params^ (extra-params@)]
         [DEFS : defs^ (defs@)]
         [RUNCHECK : empty^ (runcheck@ (CFRAME) (DEFS))])
        (export))))))
