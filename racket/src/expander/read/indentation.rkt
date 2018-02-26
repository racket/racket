#lang racket/base
(require "config.rkt"
         "closer.rkt")

(provide make-indentation
         track-indentation!
         indentation-possible-cause
         indentation-unexpected-closer-message)

(struct indentation
  (closer ; expected close paren, bracket, etc.
   [suspicious-closer #:mutable] ; expected closer when suspicious line found
   [multiline? #:mutable] ; set to #f if the match attempt spans a line
   start-line ; opener's line
   [last-line #:mutable] ; current line, already checked the identation
   [suspicious-line #:mutable] ; non-#f => first suspicious line since opener
   [max-indent #:mutable] ; max indentation encountered since opener, not counting brackets by a more neseted opener
   [suspicious-quote #:mutable])) ; non-#f => first suspicious quote whose closer is on a different line


(define (make-indentation closer in config)
  (define-values (line col pos) (port-next-location in))
  (indentation closer
               #f    ; suspicious-closer
               #f    ; multiline?
               line  ; start-line
               line  ; last-line
               #f    ; suspicious-line
               (and col (add1 col)) ; max-indent
               #f))  ; suspicious-quote

(define (track-indentation! config line col)
  (define indts (read-config-indentations config))
  (define indt (and (pair? indts) (car indts)))
  (when (and indt
             line
             (indentation-last-line indt)
             ;; Already checked this line?
             (line . > . (indentation-last-line indt)))
    (set-indentation-last-line! indt line)
    (set-indentation-multiline?! indt #t)
    ;; At least as indented as before?
    (cond
     [(col . >= . (indentation-max-indent indt))
      (set-indentation-max-indent! indt col)]
     [else
      (unless (indentation-suspicious-line indt)
	;; Not as indented, and no suspicious line found already.
	;; Suspect that the closer should have appeared earlier.
        (set-indentation-suspicious-closer! indt (indentation-closer indt))
        (set-indentation-suspicious-line! indt line))])))

(define (indentation-possible-cause config)
  (define indt (car (read-config-indentations config)))
  (cond
   [(indentation-suspicious-line indt)
    (format "\n  possible cause: indentation suggests a missing ~a before line ~a"
            (closer-name (indentation-suspicious-closer indt) config)
            (indentation-suspicious-line indt))]
   [else ""]))

(define (indentation-unexpected-closer-message ec c config)
  (define indts (read-config-indentations config))
  (cond
   [(null? indts)
    (format "unexpected `~a`" c)]
   [else
    (define indt (car indts))
    (string-append
     ;; Base message:
     (cond
      [(char=? ec (indentation-closer indt))
       ;; If this closer is the expected on, why did we get an error?
       (format "unexpected `~a`" c)]
      [else
       ;; If we're expecting this closer later, then it's not so much
       ;; "unexpected" as we expected something else...
       (define missing
         (or (for/or ([indt (in-list (cdr indts))])
               (and (char=? ec (indentation-closer indt))
                    "missing"))
             "expected"))
       (define opener-str
         (opener-name (closer->opener (indentation-closer indt)) config))
       (format "~a ~a to close ~a, found instead `~a`"
               missing
               (closer-name (indentation-closer indt) config)
               (cond
                [(indentation-multiline? indt)
                 (format "~a on line ~a"
                         opener-str
                         (indentation-start-line indt))]
                [else
                 (format "preceding ~a" opener-str)])
               c)])
     ;; Possibly add a cause based on indentation:
     (indentation-possible-cause config))]))
