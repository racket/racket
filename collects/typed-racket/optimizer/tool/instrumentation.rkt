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
        (when (right-file? entry)
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
  (values (reverse (remove-duplicates TR-log))
          (reverse mzc-log)
          (reverse (remove-duplicates info-log))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Inlining pre-processing

(provide success-regexp failure-regexp out-of-fuel-regexp)

;;; Low-level log parsing. Goes from strings to log-entry structs.


(define success-regexp       "inlining: ")
(define failure-regexp       "no inlining: ")
(define out-of-fuel-regexp   "no inlining, out of fuel: ")
(define any-inlining-event-regexp
  (format "^optimizer: (~a)" (string-join (list success-regexp
                                                failure-regexp
                                                out-of-fuel-regexp)
                                          "|")))


;; String (message from the mzc optimizer) -> log-entry
(define (mzc-opt-log-message->log-entry l)
  (define evt (parse-inlining-event l))
  (define forged-stx (inlining-event->forged-stx evt))
  (define kind
    (match (inlining-event-kind evt)
      [(and k (== success-regexp))     success-regexp]
      [(and k (== failure-regexp))     failure-regexp]
      [(and k (== out-of-fuel-regexp)) out-of-fuel-regexp]
      [_ (error "Unknown log message type" l)]))
  (inliner-log-entry kind kind
                     forged-stx forged-stx
                     (syntax-position forged-stx)
                     'mzc
                     evt))

(define inlining-event-regexp
  ;; Last bit is `generated?'. We don't care about that.
  ;; The middle elements of the vector are numbers of #f.
  (string-append
   ;; Attempt at making this thing readable.
   any-inlining-event-regexp
   "involving: "
   ;; _What_ gets inlined (or not).
   (string-append ; either a vector with name and source info, or just name
    "("
    "#\\(([^ ]+) "
    "(" "#<path:(.+)>" "|" "([^ ]+)" ")"
    " ([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+) [^ ]+\\)"
    "|"
    "([^ ]+)" ; just name, we won't be able to do much with it
    ")")
   ;; _Where_ this happens (in which function, can't get more precise info).
   (string-append
    ;; maybe full path info: path, line, col, name
    ;; path allows `:' as the second character (and first, but not a problem)
    ;; to support absolute windows paths (e.g. C:\...)
    "( in: (([^ :]?[^ ]?[^:]+):([^ :]+):([^ :]+): )?([^ ]+))?"
    ;; maybe module info, useless to us (at least for now)
    "( in module: [^ ]+)?")
   " size: ([^ ]+) threshold: ([^ ]+)"
   "$"))

(define (parse-inlining-event l)
  (match (regexp-match inlining-event-regexp l)
    [`(,all ,kind
            ,what ,name ,path ,file-path ,unsaved-path ,line ,col ,pos ,span
                  ,only-name
            ,where ,where-loc ,where-path ,where-line ,where-col ,where-name
            ,maybe-module-info
            ,size ,threshold)
     (inlining-event kind
                     (string->symbol (or name only-name))
                     (if only-name
                        #f ; no source location
                        (list (or file-path unsaved-path)
                              (string->number line)
                              (string->number col)
                              (string->number pos)
                              (string->number span)))
                     where-name
                     (if where-loc
                         (list where-path
                               (string->number where-line)
                               (string->number where-col))
                         #f) ; no source location
                     (string->number size)
                     (string->number threshold))]
    [_ (error "ill-formed inlining log entry" l)]))


(define (inlining-event->forged-stx evt)
  (match evt
    [(inlining-event kind name loc where-name where-loc size threshold)
     (datum->syntax #'here name loc)]))



(module+ test
  (require rackunit)

  ;; log parsing tests

  (define (parse l) (regexp-match inlining-event-regexp l))

  ;; Windows path
  (check-equal? (parse "optimizer: no inlining, out of fuel: involving: #(.../private/map.rkt:22:14 #<path:C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt> 22 14 620 335 #t) in: C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt:23:0: prova2 in module: 'anonymous-module size: 55 threshold: 8")
                '("optimizer: no inlining, out of fuel: involving: #(.../private/map.rkt:22:14 #<path:C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt> 22 14 620 335 #t) in: C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt:23:0: prova2 in module: 'anonymous-module size: 55 threshold: 8"
                  "no inlining, out of fuel: "
                  "#(.../private/map.rkt:22:14 #<path:C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt> 22 14 620 335 #t)"
                  ".../private/map.rkt:22:14"
                  "#<path:C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt>"
                  "C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt"
                  #f
                  "22"
                  "14"
                  "620"
                  "335"
                  #f
                  " in: C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt:23:0: prova2"
                  "C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt:23:0: "
                  "C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt"
                  "23"
                  "0"
                  "prova2"
                  " in module: 'anonymous-module"
                  "55"
                  "8"))

  (check-equal? (parse "optimizer: no inlining, out of fuel: involving: #(sqr #<path:/home/stamourv/src/plt/collects/racket/math.rkt> 35 2 838 93 #f) in: /home/stamourv/src/examples/example-shapes.rkt:41:0: inC in module: 'example-shapes size: 21 threshold: 6")
                '("optimizer: no inlining, out of fuel: involving: #(sqr #<path:/home/stamourv/src/plt/collects/racket/math.rkt> 35 2 838 93 #f) in: /home/stamourv/src/examples/example-shapes.rkt:41:0: inC in module: 'example-shapes size: 21 threshold: 6"
                  "no inlining, out of fuel: "
                  "#(sqr #<path:/home/stamourv/src/plt/collects/racket/math.rkt> 35 2 838 93 #f)"
                  "sqr"
                  "#<path:/home/stamourv/src/plt/collects/racket/math.rkt>"
                  "/home/stamourv/src/plt/collects/racket/math.rkt"
                  #f
                  "35"
                  "2"
                  "838"
                  "93"
                  #f
                  " in: /home/stamourv/src/examples/example-shapes.rkt:41:0: inC"
                  "/home/stamourv/src/examples/example-shapes.rkt:41:0: "
                  "/home/stamourv/src/examples/example-shapes.rkt"
                  "41"
                  "0"
                  "inC"
                  " in module: 'example-shapes"
                  "21"
                  "6"))

  (check-equal? (parse "optimizer: inlining: involving: #(inC #<path:/home/stamourv/src/examples/example-shapes.rkt> 41 0 993 165 #f) in: /home/stamourv/src/examples/example-shapes.rkt:27:0: in in module: 'example-shapes size: 41 threshold: 128")
                '("optimizer: inlining: involving: #(inC #<path:/home/stamourv/src/examples/example-shapes.rkt> 41 0 993 165 #f) in: /home/stamourv/src/examples/example-shapes.rkt:27:0: in in module: 'example-shapes size: 41 threshold: 128"
                  "inlining: "
                  "#(inC #<path:/home/stamourv/src/examples/example-shapes.rkt> 41 0 993 165 #f)"
                  "inC"
                  "#<path:/home/stamourv/src/examples/example-shapes.rkt>"
                  "/home/stamourv/src/examples/example-shapes.rkt"
                  #f
                  "41"
                  "0"
                  "993"
                  "165"
                  #f
                  " in: /home/stamourv/src/examples/example-shapes.rkt:27:0: in"
                  "/home/stamourv/src/examples/example-shapes.rkt:27:0: "
                  "/home/stamourv/src/examples/example-shapes.rkt"
                  "27"
                  "0"
                  "in"
                  " in module: 'example-shapes"
                  "41"
                  "128"))
  (check-equal? (parse "optimizer: no inlining, out of fuel: involving: #(sqr #<path:/Applications/Racket v5.3/collects/racket/math.rkt> 35 2 838 93 #f) in: /Users/user/Desktop/Optimization Coach/example-shapes.rkt:41:0: inC in module: 'anonymous-module size: 21 threshold: 6")
                '("optimizer: no inlining, out of fuel: involving: #(sqr #<path:/Applications/Racket v5.3/collects/racket/math.rkt> 35 2 838 93 #f) in: /Users/user/Desktop/Optimization Coach/example-shapes.rkt:41:0: inC in module: 'anonymous-module size: 21 threshold: 6"
                  "no inlining, out of fuel: "
                  "#(sqr #<path:/Applications/Racket v5.3/collects/racket/math.rkt> 35 2 838 93 #f)"
                  "sqr"
                  "#<path:/Applications/Racket v5.3/collects/racket/math.rkt>"
                  "/Applications/Racket v5.3/collects/racket/math.rkt"
                  #f
                  "35"
                  "2"
                  "838"
                  "93"
                  #f
                  " in: /Users/user/Desktop/Optimization Coach/example-shapes.rkt:41:0: inC"
                  "/Users/user/Desktop/Optimization Coach/example-shapes.rkt:41:0: "
                  "/Users/user/Desktop/Optimization Coach/example-shapes.rkt"
                  "41"
                  "0"
                  "inC"
                  " in module: 'anonymous-module"
                  "21"
                  "6")))
