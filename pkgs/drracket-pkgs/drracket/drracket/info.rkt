#lang info

;(define tools      '("sprof.rkt"))
;(define tool-names '("Sampling Profiler"))

(define drracket-tools      '("syncheck.rkt"))
(define drracket-tool-names '("Check Syntax"))

(define gracket-launcher-names     '("DrRacket"))
(define gracket-launcher-libraries '("drracket.rkt"))

(define release-note-files (list (list "DrRacket" "HISTORY.txt")))

(define copy-man-pages '("drracket.1"))

(define framework:color-schemes 
  (list (hash 
         'name 'modern-color-scheme
         'colors
         '((framework:syntax-color:scheme:string #(211 72 255))
           (framework:syntax-color:scheme:constant #(211 72 255))
           (framework:syntax-color:scheme:comment #(194 158 31))
           (framework:syntax-color:scheme:parenthesis #(0 150 255))))))
