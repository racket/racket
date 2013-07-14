#lang racket/base
(require syntax/module-reader
         "../resolver.rkt")

(provide (rename-out [planet-read read]
                     [planet-read-syntax read-syntax]
                     [planet-get-info get-info]))

(define (str->spec str)
  (let ([str (bytes->string/latin-1 str)])
    (if (module-path? `(planet ,(string->symbol str)))
        (vector `(submod (planet ,(string->symbol str)) reader)
                `(planet ,(string->symbol (string-append str "/lang/reader"))))
        #f)))

(define-values (planet-read planet-read-syntax real-planet-get-info)
  (make-meta-reader
   'planet
   "planet path"
   str->spec
   values
   values
   values))

(define op (current-output-port))

(define (planet-get-info inport module-path line col pos)
  (parameterize ([install? #f]
                 [download? #f])
    (real-planet-get-info inport module-path line col pos)))

