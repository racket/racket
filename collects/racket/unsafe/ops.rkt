#lang racket/base
(require '#%unsafe
         '#%flfxnum)

(provide (all-from-out '#%unsafe)
         (prefix-out unsafe-
                     (combine-out flsin flcos fltan
                                  flasin flacos flatan
                                  fltruncate flround flfloor flceiling
                                  flexp fllog flexpt)))
