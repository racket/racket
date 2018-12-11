#lang racket/base
(require "../common/struct-star.rkt"
         "../common/parameter-like.rkt"
         "readtable-parameter.rkt")

(provide (struct*-out read-config)
         (struct-out read-config-state)
         current-read-config
         make-read-config
         read-config-update
         port+config->srcloc
         reading-at
         disable-wrapping
         keep-comment
         discard-comment
         next-readtable)

(struct* read-config (readtable
                      next-readtable ; readtable to use for recursive reads
                      for-syntax?   ; impose restrictions on graphs, fxvectors, etc?
                      source
                      * wrap          ; wrapper applied to each datum, intended for syntax objects
                      read-compiled   ; for `#~`: input-port -> any/c
                      call-with-root-namespace ; around extension callback
                      dynamic-require ; for reader extensions: module-path sym -> any
                      module-declared? ; for `#lang`: module-path -> any/c
                      coerce        ; coerce for syntax or not: any boolean -> any
                      coerce-key    ; coerce unwrapped key for hash
                      * line
                      * col
                      * pos
                      * indentations  ; stack of `indentation` records
                      * keep-comment? ; make main dispatch return on comment
                      parameter-override ; mash of parameter -> value
                      parameter-cache   ; hash of parameter -> value
                      st)) ; other shared mutable state

(struct read-config-state ([accum-str #:mutable] ; string-buffer cache
                           [graph #:mutable]))   ; #f or hash of number -> value

(define-parameter-like current-read-config #f) ; for `read/recursive`

(define (make-read-config
         #:source [source #f]
         #:for-syntax? [for-syntax? #f]
         #:readtable [readtable (current-readtable)]
         #:next-readtable [next-readtable readtable]
         #:wrap [wrap #f #;(lambda (s-exp srcloc) s-exp)]
         #:read-compiled [read-compiled #f]
         #:call-with-root-namespace [call-with-root-namespace #f]
         #:dynamic-require [dynamic-require #f]
         #:module-declared? [module-declared? #f]
         #:coerce [coerce #f]
         #:coerce-key [coerce-key #f]
         #:keep-comment? [keep-comment? #f])
  (read-config readtable
               next-readtable
               for-syntax?
               source
               wrap
               (or read-compiled
                   (lambda (in)
                     (error 'read "no `read-compiled` provided")))
               (or call-with-root-namespace
                   (lambda (thunk)
                     (error 'read "no `call-with-root-namespace` provided")))
               (or dynamic-require
                   (lambda (mod-path sym failure-k)
                     (error 'read "no `dynamic-require` provided")))
               (or module-declared?
                   (lambda (mod-path)
                     (error 'read "no `module-declare?` provided")))
               (or coerce
                   (lambda (for-syntax? v srcloc) v))
               (or coerce-key
                   (lambda (for-syntax? v) v))
               #f ; line
               #f ; col
               #f ; pos
               null ; indentations
               keep-comment?
               #hasheq()     ; parameter-override
               (make-hasheq) ; parameter-cache
               (read-config-state #f    ; accum-str
                                  #f))) ; graph

(define (read-config-update config
                            #:for-syntax? for-syntax?
                            #:wrap wrap
                            #:readtable readtable
                            #:next-readtable [next-readtable (read-config-readtable config)]
                            #:reset-graph? local-graph?
                            #:keep-comment? keep-comment?)
  (struct*-copy read-config config
                [for-syntax? for-syntax?]
                [wrap wrap]
                [readtable readtable]
                [next-readtable next-readtable]
                [keep-comment? keep-comment?]
                [st (if local-graph?
                        (read-config-state #f #f)
                        (read-config-st config))]))

(define (port+config->srcloc in config
                             #:end-pos [given-end-pos #f])
  (define end-pos
    (or given-end-pos
        (let-values ([(end-line end-col end-pos) (port-next-location in)])
          end-pos)))
  (srcloc (or (read-config-source config)
              (object-name in)
              "UNKNOWN")
          (read-config-line config)
          (read-config-col config)
          (read-config-pos config)
          (and (read-config-pos config) end-pos (max 0 (- end-pos (read-config-pos config))))))

(define (reading-at config line col pos)
  (struct*-copy read-config config
                [line line]
                [col col]
                [pos pos]))

(define (disable-wrapping config)
  (struct*-copy read-config config
                [wrap #f]))

(define (keep-comment config)
  (struct*-copy read-config config
                [keep-comment? #t]))

(define (discard-comment config)
  (cond
   [(not (read-config-keep-comment? config))
    config]
   [else
    (struct*-copy read-config config
                  [keep-comment? #f])]))

(define (next-readtable config)
  (cond
   [(eq? (read-config-readtable config)
         (read-config-next-readtable config))
    config]
   [else
    (struct*-copy read-config config
                  [readtable (read-config-next-readtable config)])]))
