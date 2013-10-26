#lang info

;(define tools      '("sprof.rkt"))
;(define tool-names '("Sampling Profiler"))

(define drracket-tools      '("syncheck.rkt"))
(define drracket-tool-names '("Check Syntax"))

(define gracket-launcher-names     '("DrRacket"))
(define gracket-launcher-libraries '("drracket.rkt"))

(define release-note-files (list (list "DrRacket" "HISTORY.txt")))

(define copy-man-pages '("drracket.1"))

;; color-blind friendly palette from Paul Tol:
;; http://www.sron.nl/~pault/
(define dark-blue '#(#x33 #x22 #x88))
(define light-blue #(#x88 #xCC #xEE))
(define medium-green #(#x44 #xAA #x99))
(define dark-green #(#x11 #x77 #x33))
(define olive-green #(#x99 #x99 #x33))
(define tan #(#xDD #xCC #x77))
(define orangy-pink #(#xCC #x66 #x77))
(define dark-red #(#x88 #x22 #x55))
(define purple #(#xAA #x44 #x99))


(define error-color orangy-pink)
(define tol-black-on-white-colors
  `((framework:paren-match-color ,tan)
    (framework:syntax-color:scheme:comment ,olive-green)
    (framework:syntax-color:scheme:constant ,dark-green)
    (framework:syntax-color:scheme:error ,error-color)
    (framework:syntax-color:scheme:hash-colon-keyword ,dark-green)
    (framework:syntax-color:scheme:keyword ,dark-blue)
    (framework:syntax-color:scheme:string ,dark-green)
    (framework:syntax-color:scheme:other ,dark-blue)
    (framework:syntax-color:scheme:parenthesis ,light-blue)
    (framework:syntax-color:scheme:symbol ,dark-blue)
    (drracket:read-eval-print-loop:error-color ,error-color)
    (drracket:read-eval-print-loop:out-color ,dark-green)
    (drracket:read-eval-print-loop:value-color ,dark-blue) 
    (drracket:check-syntax:free-variable ,error-color)
    (drracket:check-syntax:set!d ,error-color)
    (drracket:check-syntax:unused-require ,error-color)))

(define tol-wob-constants purple)
(define tol-white-on-black-colors
  `((framework:paren-match-color ,dark-blue)
    (framework:syntax-color:scheme:comment ,olive-green)
    (framework:syntax-color:scheme:constant ,tol-wob-constants)
    (framework:syntax-color:scheme:error ,error-color)
    (framework:syntax-color:scheme:hash-colon-keyword ,tol-wob-constants)
    (framework:syntax-color:scheme:keyword ,light-blue)
    (framework:syntax-color:scheme:string ,tol-wob-constants)
    (framework:syntax-color:scheme:other ,light-blue)
    (framework:syntax-color:scheme:parenthesis ,dark-green)
    (framework:syntax-color:scheme:symbol ,light-blue)
    (drracket:read-eval-print-loop:error-color ,error-color)
    (drracket:read-eval-print-loop:out-color ,dark-green)
    (drracket:read-eval-print-loop:value-color ,light-blue) 
    (drracket:check-syntax:free-variable ,error-color)
    (drracket:check-syntax:set!d ,error-color)
    (drracket:check-syntax:unused-require ,error-color)))

(define framework:color-schemes 
  (list 
   (hash 
    'name 'modern-color-scheme
    'colors
    '((framework:syntax-color:scheme:string #(211 72 255))
      (framework:syntax-color:scheme:constant #(211 72 255))
      (framework:syntax-color:scheme:comment #(194 158 31))
      (framework:syntax-color:scheme:parenthesis #(0 150 255))))
   (hash
    'name "Tol's Color-blind-safe"
    'colors tol-black-on-white-colors)
   (hash
    'name "Tol's White on Black"
    'white-on-black-base? #t
    'colors tol-white-on-black-colors)))

