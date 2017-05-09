#lang racket/base
(require '#%unsafe
         '#%flfxnum
         '#%extfl
         "../private/kw.rkt")

(provide (except-out (all-from-out '#%unsafe)
                     unsafe-undefined
                     check-not-unsafe-undefined
                     check-not-unsafe-undefined/assign
                     prop:chaperone-unsafe-undefined
                     chaperone-struct-unsafe-undefined
                     unsafe-chaperone-procedure
                     unsafe-impersonate-procedure
                     unsafe-start-atomic unsafe-end-atomic
                     unsafe-start-breakable-atomic unsafe-end-breakable-atomic
                     unsafe-in-atomic?)
         (rename-out [new:unsafe-impersonate-procedure unsafe-impersonate-procedure]
                     [new:unsafe-chaperone-procedure unsafe-chaperone-procedure])
         (prefix-out unsafe-
                     (combine-out flsin flcos fltan
                                  flasin flacos flatan
                                  fltruncate flround flfloor flceiling
                                  flexp fllog flexpt

                                  extflsin extflcos extfltan
                                  extflasin extflacos extflatan
                                  extfltruncate extflround extflfloor extflceiling
                                  extflexp extfllog extflexpt)))
