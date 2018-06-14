#lang racket/base
(require "../common/set.rkt"
         "../run/status.rkt"
         "../boot/runtime-primitive.rkt"
         "link.rkt"
         "linklet-info.rkt"
         "linklet.rkt"
         "variable.rkt")

(provide check-and-record-report!)

;; Check for bootstrap obstacles, and prepare to report if the
;; obstacles persist
(define (check-and-record-report! #:compiled-modules compiled-modules
                                  #:linklets linklets
                                  #:linklets-in-order linklets-in-order
                                  #:needed needed
                                  #:instance-knot-ties instance-knot-ties)

  (log-status "Traversed ~s modules" (hash-count compiled-modules))
  (log-status "Got ~s relevant linklets" (hash-count linklets))
  (log-status "Need ~s of those linklets" (hash-count needed))

  (define code-bytes
    (let ([o (open-output-bytes)])
      (for ([li (in-list (unbox linklets-in-order))])
        (write (linklet-info-linklet (hash-ref linklets li)) o))
      (get-output-bytes o)))
  
  (define source-mode? (linklets-are-source-mode? linklets))

  (log-status "Code is ~s bytes~a"
              (bytes-length code-bytes)
              (if source-mode? " as source" ""))
  (unless source-mode?
    (log-status "Reading all code...")
    (time (let ([i (open-input-bytes code-bytes)])
            (parameterize ([read-accept-compiled #t])
              (let loop ()
                (unless (eof-object? (read i))
                  (loop)))))))

  ;; Check whether any needed linklet needs an instance of a
  ;; pre-defined instance that is not part of the runtime system:
  (define complained? #f)
  (define check-later-vars (make-hash)) ; variable -> (listof complain-proc)
  (for ([lnk (in-list (unbox linklets-in-order))])
    (define needed-reason (hash-ref needed lnk #f))
    (when needed-reason
      (define li (hash-ref linklets lnk))
      (define complained-this? #f)
      (for ([in-lnk (in-list (linklet-info-imports li))]
            [in-vars (in-list (linklet-info-in-variables li))])
        (define p (link-name in-lnk))
        (when (and (symbol? p)
                   (not (member p runtime-instances))
                   (not (eq? p '#%linklet))
                   (not (hash-ref instance-knot-ties p #f))
                   (hash-ref needed in-lnk #t))
          ;; Delay the complaint until we know whether the name is
          ;; actually used after flattening and pruning
          (define (complain really-used-var?)
            (unless complained?
              (log-status "~a\n ~a\n ~a\n ~a\n ~a"
                          "Unfortunately, some linklets depend on pre-defined host instances"
                          "that are not part of the runtime system; at least one the following"
                          "references is a problem, but not necessarily all of them, because"
                          "some references may be detected as unused by the flattener (but"
                          "we've lost track of the connection):")
              (set! complained? #t))
            (unless complained-this?
              (log-status "  - ~a at ~s" (link-name lnk) (link-phase lnk))
              (log-status "~a" (lines (format "   needs ~s:" p)
                                      (for/list ([in-var (in-list in-vars)]
                                                 #:when (really-used-var? (variable in-lnk in-var)))
                                        in-var)))
              (log-status "   needed by ~s" needed-reason)
              (set! complained-this? #t)))
          (for ([in-var (in-list in-vars)])
            (hash-update! check-later-vars (variable in-lnk in-var) (lambda (l) (cons complain l)) null))))))
  check-later-vars)
