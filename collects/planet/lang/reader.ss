#lang scheme/base

(require "../parsereq.ss"
         syntax/readerr)

(provide (rename-out [planet-read read]
                     [planet-read-syntax read-syntax]))

(define (planet-read-fn in read-sym args src mod line col pos)
  (let ([spec (regexp-try-match #px"^(.*?)(\\s|$)" in)]
        [bad (lambda (str eof?)
               ((if eof?
                    raise-read-eof-error 
                    raise-read-error)
                (format "bad planet path following language-loder syntax~a~a"
                        (if str ": " "")
                        (or str ""))
                src line col pos
                (let-values ([(line col pos2) (port-next-location in)])
                  (and pos pos2 (- pos2 pos)))))])
    (if (or (not spec)
            (equal? (cadr spec) ""))
        (bad #f (eof-object? (peek-byte in)))
        (let ([parsed-spec
               (let ([str (bytes->string/latin-1 (cadr spec))])
                 (if (module-path? `(planet ,(string->symbol str)))
                     `(planet ,(string->symbol (string-append str "/lang/reader")))
                     #f))])
          (if parsed-spec
              (let ([r (dynamic-require parsed-spec read-sym)])
                (if (and (procedure? r)
                         (procedure-arity-includes? r (+ 5 (length args))))
                    (apply r (append args
                                     (list in mod line col pos)))
                    (apply r (append args (list in)))))
              (bad (cadr spec) #f))))))

(define (planet-read inp mod line col pos)
  (planet-read-fn inp 'read null (object-name inp) mod line col pos))

(define (planet-read-syntax src inp mod line col pos)
  (planet-read-fn inp 'read-syntax (list src) src mod line col pos))
