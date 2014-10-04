#lang info

;(define tools      '("sprof.rkt"))
;(define tool-names '("Sampling Profiler"))

(define drracket-tools      '("syncheck.rkt"))
(define drracket-tool-names '("Check Syntax"))

(define gracket-launcher-names     '("DrRacket"))
(define gracket-launcher-libraries '("drracket.rkt"))

(define release-note-files (list (list "DrRacket" "HISTORY.txt")))

(define copy-man-pages '("drracket.1"))

(define binary-keep-files '("private/launcher-mred-bootstrap.rkt"
                            "private/launcher-mz-bootstrap.rkt"))

;; color-blind friendly palette from Paul Tol:
;; http://www.sron.nl/~pault/
;; using his Scheme for Marking Text colors
(define light-blue #(#x90 #xb3 #xff))
(define light-cyan #(#xcc #xee #xff))
(define light-green #(#xcc #xdd #xaa))
(define light-tan #(#xee #xee #xbb))
(define light-pink #(#xff #xcc #xcc))
(define light-grey #(#xdd #xdd #xdd))
(define dark-blue #(#x22 #x22 #x77))
(define dark-plum #(#x77 #x22 #x77))
(define dark-green #(#x22 #x77 #x22))
(define dark-olive #(#x66 #x66 #x33))
(define dark-red #(#x88 #x11 #x11))
(define sea-green #(#x00 #xb2 #xb2))
(define burnt-orange #(181 102 51))

(define tol-bow-error-color dark-red)
(define tol-bow-constants dark-green)
(define tol-black-on-white-colors
  `((framework:paren-match-color ,light-tan)
    (framework:syntax-color:scheme:comment ,dark-plum)
    (framework:syntax-color:scheme:constant ,tol-bow-constants)
    (framework:syntax-color:scheme:error ,tol-bow-error-color)
    (framework:syntax-color:scheme:hash-colon-keyword ,tol-bow-constants)
    (framework:syntax-color:scheme:keyword ,dark-blue)
    (framework:syntax-color:scheme:string ,tol-bow-constants)
    (framework:syntax-color:scheme:text ,tol-bow-constants)
    (framework:syntax-color:scheme:other ,dark-blue)
    (framework:syntax-color:scheme:parenthesis ,sea-green)
    (framework:syntax-color:scheme:symbol ,dark-blue)
    (drracket:read-eval-print-loop:error-color ,tol-bow-error-color)
    (drracket:read-eval-print-loop:out-color ,tol-bow-constants)
    (drracket:read-eval-print-loop:value-color ,dark-blue) 
    (drracket:check-syntax:free-variable ,tol-bow-error-color)
    (drracket:check-syntax:set!d ,tol-bow-error-color)
    (drracket:check-syntax:unused-require ,tol-bow-error-color)))

(define tol-wob-constants light-blue)
(define tol-wob-error light-pink)
(define tol-white-on-black-colors
  `((framework:paren-match-color ,dark-blue)
    (framework:syntax-color:scheme:comment ,light-green)
    (framework:syntax-color:scheme:constant ,tol-wob-constants)
    (framework:syntax-color:scheme:error ,tol-wob-error)
    (framework:syntax-color:scheme:hash-colon-keyword ,tol-wob-constants)
    (framework:syntax-color:scheme:keyword ,light-blue)
    (framework:syntax-color:scheme:string ,tol-wob-constants)
    (framework:syntax-color:scheme:text ,tol-wob-constants)
    (framework:syntax-color:scheme:other ,light-blue)
    (framework:syntax-color:scheme:parenthesis ,dark-plum)
    (framework:syntax-color:scheme:symbol ,light-blue)
    (drracket:read-eval-print-loop:error-color ,tol-wob-error)
    (drracket:read-eval-print-loop:out-color ,dark-green)
    (drracket:read-eval-print-loop:value-color ,light-blue) 
    (drracket:check-syntax:free-variable ,tol-wob-error)
    (drracket:check-syntax:set!d ,tol-wob-error)
    (drracket:check-syntax:unused-require ,tol-wob-error)))

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
