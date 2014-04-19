#lang racket/base
(require '#%unsafe
         '#%flfxnum
         '#%extfl)

(provide (except-out (all-from-out '#%unsafe)
                     unsafe-undefined
                     check-not-unsafe-undefined
                     check-not-unsafe-undefined/assign
                     prop:chaperone-unsafe-undefined
                     chaperone-struct-unsafe-undefined)
         (prefix-out unsafe-
                     (combine-out flsin flcos fltan
                                  flasin flacos flatan
                                  fltruncate flround flfloor flceiling
                                  flexp fllog flexpt

                                  extflsin extflcos extfltan
                                  extflasin extflacos extflatan
                                  extfltruncate extflround extflfloor extflceiling
                                  extflexp extfllog extflexpt)))
