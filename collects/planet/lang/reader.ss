#lang scheme/base

(require "../parsereq.ss"
         syntax/readerr)


(provide (rename-out [planet-read read]
                     [planet-read-syntax read-syntax])
         get-info)

(define (planet-get in lang-mod export-sym src line col pos mk-fail-thunk)
  (let ([spec (regexp-try-match #px"^[ \t]+(.*?)(\\s|$)" in)]
        [bad (lambda (str eof?)
               ((if eof?
                    raise-read-eof-error 
                    raise-read-error)
                (format "bad planet path following language-loader syntax~a~a"
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
                     `(planet ,(string->symbol (string-append str lang-mod)))
                     #f))])
          (if parsed-spec
              (begin
                ((current-reader-guard) parsed-spec)
                (dynamic-require parsed-spec export-sym (mk-fail-thunk spec)))
              (bad (cadr spec) #f))))))

(define (get-info in mod line col pos)
  (planet-get in "/lang/langinfo" 'get-info (object-name in) line col pos
              (lambda (spec) (lambda () (lambda (tag) #f)))))

(define (planet-read-fn in read-sym args src mod line col pos)
  (let ([r (planet-get in "/lang/reader" read-sym src #|mod|# line col pos
                       (lambda (spec) 
                         (lambda ()
                           (error 'planet "cannot find reader for `#lang planet ~a'" spec))))])
    (if (and (procedure? r)
             (procedure-arity-includes? r (+ 5 (length args))))
        (apply r (append args
                         (list in mod line col pos)))
        (apply r (append args (list in))))))

(define (planet-read inp mod line col pos)
  (planet-read-fn inp 'read null (object-name inp) mod line col pos))

(define (planet-read-syntax src inp mod line col pos)
  (planet-read-fn inp 'read-syntax (list src) src mod line col pos))
