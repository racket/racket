#lang scheme/base

;; FIXME: This code was largely cut-and-pasted from the planet reader.

(require syntax/readerr
         (only-in scribble/reader make-at-readtable))

(provide (rename-out [at-read read]
                     [at-read-syntax read-syntax])
         get-info)

(define (at-get in export-sym src line col pos mk-fail-thunk)
  (define (bad str eof?)
    ((if eof? raise-read-eof-error raise-read-error)
     (let ([msg "bad language path following at-exp"])
       (if str (string-append msg ": " str) msg))
     src line col pos
     (let-values ([(line col pos2) (port-next-location in)])
       (and pos pos2 (- pos2 pos)))))
  (define spec (regexp-try-match #px"^[ \t]+(.*?)(?=\\s|$)" in))
  (if (or (not spec) (equal? (cadr spec) ""))
    (bad #f (eof-object? (peek-byte in)))
    (let ([parsed-spec
           (let ([s (string->symbol
                     (string-append (bytes->string/latin-1 (cadr spec))
                                    "/lang/reader"))])
             (and (module-path? s) s))])
      (if parsed-spec
        (begin ((current-reader-guard) parsed-spec)
               (dynamic-require parsed-spec export-sym (mk-fail-thunk spec)))
        (bad (cadr spec) #f)))))

(define (get-info inp mod line col pos)
  (at-get inp 'get-info (object-name inp) line col pos
          (lambda (spec) (lambda () (lambda (tag) #f)))))

(define at-readtable (make-at-readtable))

(define (at-read-fn in read-sym args src mod line col pos)
  (let ([r (at-get in read-sym src #|mod|# line col pos
                   (lambda (spec)
                     (lambda ()
                       (error 'at-exp "cannot find reader for `#lang at ~a'"
                              spec))))])
    (parameterize ([current-readtable at-readtable])
      (if (and (procedure? r) (procedure-arity-includes? r (+ 5 (length args))))
        (apply r (append args (list in mod line col pos)))
        (apply r (append args (list in)))))))

(define (at-read inp mod line col pos)
  (at-read-fn inp 'read null (object-name inp) mod line col pos))

(define (at-read-syntax src inp mod line col pos)
  (at-read-fn inp 'read-syntax (list src) src mod line col pos))

