#lang racket/base
(require "../run/status.rkt"
         "../boot/runtime-primitive.rkt"
         "link.rkt"
         "linklet-info.rkt"
         "linklet.rkt")

(provide check-and-report!)

;; Check for bootstrap obstacles and report the results
(define (check-and-report! #:compiled-modules compiled-modules
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
  (define needed-vars null)
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
          (unless complained?
            (log-status "~a\n ~a"
                        "Unfortunately, some linklets depend on pre-defined host instances"
                        "that are not part of the runtime system:")
            (set! complained? #t))
          (unless complained-this?
            (log-status " - ~a at ~s" (link-name lnk) (link-phase lnk))
            (set! complained-this? #t))
          (log-status "~a" (lines (format "   needs ~s:" p) in-vars))
          (set! needed-vars (append in-vars needed-vars))))
      (when complained-this?
        (log-status "   needed by ~s" needed-reason))))
  (when complained?
    (log-status "~a\n ~a"
                "If these dependencies are not removed by subsequent flattening"
                "and simplification, extraction cannot succeed."))
  (and complained? needed-vars))
