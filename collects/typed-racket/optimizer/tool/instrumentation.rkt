#lang racket/base

(require racket/class racket/gui/base racket/string racket/match racket/list
         unstable/syntax unstable/logging
         "structs.rkt" "sandbox.rkt")

(provide generate-logs)

(define (generate-logs this)
  (define file-predicate (make-file-predicate this))
  (define input          (open-input-text-editor this))
  (port-count-lines! input)
  (define (right-file? l) ; does the log-entry refer to the file we're in?
    (define stx (log-entry-stx l))
    (define path
      (let ([dir  (syntax-source-directory stx)]
            [file (syntax-source-file-name stx)])
        (if (and dir file)
            (build-path dir file)
            #f)))
    (file-predicate path))
  (define TR-log   '())
  (define mzc-log  '())
  (define info-log '()) ; for hidden costs
  (with-intercepted-logging
      (lambda (l)
        ;; From mzc, create a log-entry from the info.
        (define entry (mzc-opt-log-message->log-entry (vector-ref l 1)))
        (when (and entry (right-file? entry))
          (set! mzc-log (cons entry mzc-log))))
    (lambda ()
      (with-intercepted-logging
          (lambda (l)
            ;; From TR, use the log-entry struct provided.
            (define entry (vector-ref l 2))
            (when (right-file? entry)
              (if (info-log-entry? entry)
                  (set! info-log (cons entry info-log))
                  (set! TR-log   (cons entry TR-log)))))
        (lambda ()
          (run-inside-optimization-coach-sandbox
           this
           (lambda ()
             (void (compile (read-syntax (send this get-port-name) input))))))
        'debug 'TR-optimizer))
    'debug 'optimizer)
  ;; The raw TR logs may contain duplicates from the optimizer traversing
  ;; the same piece of code multiple times.
  ;; Duplicates are not significant (unlike for inlining logs) and we can
  ;; prune them.
  (values (remove-duplicates TR-log) mzc-log (remove-duplicates info-log)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Inlining pre-processing

(provide success-key failure-key out-of-fuel-key)

;;; Low-level log parsing. Goes from strings to log-entry structs.

(define success-key     'inlining)
(define failure-key     'no-inlining)
(define out-of-fuel-key 'out-of-fuel)

;; Inliner logs contain path literals, which are not readable.
;; Use a custom reader to parse the logs.

;; At this point, the #< has already been seen.
;; For now, returns a string. Maybe return a path eventually.
(define (read-path port)
  (let ([s (open-output-string)])
    (unless (string=? (read-string 5 port) "path:")
      (error "OC path reader: bad path syntax"))
    (let loop ([c (read-char port)])
      ;; parse until the closing >
      (cond [(eof-object? c)
             (error "OC path reader: bad path syntax")]
            [(not (equal? c #\>))
             (write-char c s)
             (loop (read-char port))]
            [else
             ;; we saw the closing broket, we're done
             (values (get-output-string s))]))))
(define path-readtable
  (make-readtable
   (current-readtable)
   #\<
   'dispatch-macro
   (case-lambda
     [(char port) ; read
      (read-path port)]
     [(char port src line col pos) ; read-syntax
      (error "OC path reader: read-syntax is not supported")])))
(define (read/path s)
  (parameterize ([current-readtable  path-readtable]
                 [current-input-port (open-input-string s)])
    (read)))

;; String (message from the mzc optimizer) -> log-entry
(define (mzc-opt-log-message->log-entry l)
  (define evt (parse-inlining-event l))
  (cond [evt
         (define forged-stx (inlining-event->forged-stx evt))
         (define kind
           (match (inlining-event-kind evt)
             [(== success-key)                             success-key]
             [(or (== failure-key)     (== 'non-copyable)) failure-key]
             [(or (== out-of-fuel-key) (== 'too-large))    out-of-fuel-key]
             [_ (error "Unknown log message type" l)]))
         (inliner-log-entry kind kind
                            forged-stx forged-stx
                            (syntax-position forged-stx)
                            evt)]
        [else #f]))

;; _Where_ this happens (in which function, can't get more precise info).
;; Note: sadly, this part still needs to be parsed by a regexp. Inliner logging
;;   doesn't have control over the format for that part. Since it may include
;;   unquoted paths, which can include spaces, can't really use the reader
;;   approach. Backslashes are doubled before we get here, to handle Windows
;;   paths.
(define where-regexp
  (string-append
   ;; maybe full path info: path, line, col, name
   ;; path allows `:' as the second character (and first, but not a problem)
   ;; to support absolute windows paths (e.g. C:\...)
   "( in: (([^ :]?[^ ]?[^:]+):([^ :]+):([^ :]+): )?([^ ]+))?"
   ;; maybe module info, useless to us (at least for now)
   "( in module: [^ ]+)?"))
(define (parse-where l)
  (match (regexp-match where-regexp l)
    [`(,all
       ,where ,where-loc ,where-path ,where-line ,where-col ,where-name
       ,maybe-module-info)
     (values (and where-name (string->symbol where-name))
             (if where-loc
                 (list where-path
                       (string->number where-line)
                       (string->number where-col))
                 #f))])) ; no source location

(define (parse-inlining-event l)
  (define (ill-formed)
    (log-debug (format "OC log parser: ill-formed mzc log entry: ~a" l))
    #f)
  ;; Inlining log entry strings consist of two parts.
  ;; The first is `read'-able, given the custom reader above that can
  ;; read path literals.
  ;; The second part needs to be parsed with a regexp (see above).
  ;; The two are separated by "#<separator>", which shouldn't clash with
  ;; program identifiers.
  (cond [(regexp-match #rx"#<separator>" l)
         (match-define `(,readable-part ,parsable-part)
           (regexp-split #rx"#<separator>" l))
         (match (read/path (format "(~a)" readable-part))
           [`(optimizer: ,kind ,what
                         size: ,size threshold: ,threshold)
            (define-values (what-name  what-loc)
              (match what
                [`#(,what-name ,what-path ,what-line ,what-col
                               ,what-pos ,what-span ,gen?)
                 (values
                  what-name
                  (list what-path what-line what-col what-pos what-span))]
                [only-name
                 (values only-name #f)]))
            (define-values (where-name where-loc)
              (parse-where parsable-part))
            (inlining-event kind
                            what-name  what-loc
                            where-name where-loc
                            size threshold)]
           ;; can't parse, or log entry not about inlining (e.g. div by 0 detected)
           [_ (ill-formed)])]
        [else (ill-formed)]))


(define (inlining-event->forged-stx evt)
  (match evt
    [(inlining-event kind name loc where-name where-loc size threshold)
     (datum->syntax #'here name loc)]))



(module+ test
  (require rackunit)

  ;; log parsing tests

  ;; Windows path
  (check-equal?
   (parse-inlining-event "optimizer: out-of-fuel #(.../private/map.rkt:22:14 #<path:C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt> 22 14 620 335 #t) size: 55 threshold: 8#<separator> in: C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt:23:0: prova2 in module: 'anonymous-module")
   (inlining-event
    'out-of-fuel '.../private/map.rkt:22:14
    (list "C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt" 22 14 620 335)
    'prova2
    (list "C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt" 23 0)
    55 8))

  (check-equal?
   (parse-inlining-event "optimizer: out-of-fuel #(sqr #<path:/home/stamourv/src/plt/collects/racket/math.rkt> 35 2 838 93 #f) size: 21 threshold: 6#<separator> in: /home/stamourv/src/examples/example-shapes.rkt:41:0: inC in module: 'example-shapes")
   (inlining-event
    'out-of-fuel 'sqr
    (list "/home/stamourv/src/plt/collects/racket/math.rkt" 35 2 838 93)
    'inC (list "/home/stamourv/src/examples/example-shapes.rkt" 41 0)
    21 6))

  (check-equal?
   (parse-inlining-event "optimizer: inlining #(inC #<path:/home/stamourv/src/examples/example-shapes.rkt> 41 0 993 165 #f) size: 41 threshold: 128#<separator> in: /home/stamourv/src/examples/example-shapes.rkt:27:0: in in module: 'example-shapes")
   (inlining-event
    'inlining 'inC
    (list "/home/stamourv/src/examples/example-shapes.rkt" 41 0 993 165)
    'in (list "/home/stamourv/src/examples/example-shapes.rkt" 27 0)
    41 128))

  (check-equal?
   (parse-inlining-event "optimizer: out-of-fuel #(sqr #<path:/Applications/Racket v5.3/collects/racket/math.rkt> 35 2 838 93 #f) size: 21 threshold: 6#<separator> in: /Users/user/Desktop/Optimization Coach/example-shapes.rkt:41:0: inC in module: 'anonymous-module")
   (inlining-event
    'out-of-fuel 'sqr
    (list "/Applications/Racket v5.3/collects/racket/math.rkt" 35 2 838 93)
    'inC (list "/Users/user/Desktop/Optimization Coach/example-shapes.rkt" 41 0)
    21 6))

  (check-equal?
   (parse-inlining-event
    "optimizer: inlining #(f unsaved-editor590 2 0 20 14 #f) size: 0 threshold: 64#<separator> in: unsaved-editor590:3:0: g in module: 'anonymous-module")
   (inlining-event
    'inlining 'f (list 'unsaved-editor590 2 0 20 14)
    'g (list "unsaved-editor590" 3 0)
    0 64))

  (check-equal?
   (parse-inlining-event
    "optimizer: inlining #(g unsaved-editor590 3 0 35 16 #f) size: 0 threshold: 64#<separator> in module: 'anonymous-module")
   (inlining-event
    'inlining 'g (list 'unsaved-editor590 3 0 35 16)
    #f #f 0 64))
  )
