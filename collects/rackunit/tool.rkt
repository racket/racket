#lang racket/base
(require racket/class
         racket/gui/base
         framework
         drscheme/tool
         racket/unit
         (prefix-in drlink: "private/gui/drracket-link.rkt"))

(provide tool@)

;; CONSTANTS

(define BACKTRACE-NO-MESSAGE "No message.")
(define LINK-MODULE-SPEC 'rackunit/private/gui/drracket-link)

(define-namespace-anchor drracket-ns-anchor)

;; ----

;; close/eventspace : (a* -> b) -> (a* -> b)
;; Returns a procedure that executes the procedure in the 
;; eventspace current when close/eventspace was executed.
;; Effectively, "close" the procedure in the current eventspace.
(define (close-eventspace f)
  (let ([es (current-eventspace)])
    (lambda args
      (parameterize [(current-eventspace es)]
        (apply f args)))))

(define (close-eventspace/async f)
  (let ([es (current-eventspace)])
    (lambda args
      (parameterize ((current-eventspace es))
        (queue-callback (lambda () (apply f args)))))))

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)

    ;; show-backtrace : exn -> void
    (define show-backtrace
      (close-eventspace/async
       (lambda (msg bt)
         (drscheme:debug:show-backtrace-window
          (or msg BACKTRACE-NO-MESSAGE)
          bt))))

    (define (list->srcloc x)
      (make-srcloc (list-ref x 0)
                   (list-ref x 1)
                   (list-ref x 2)
                   (list-ref x 3)
                   (list-ref x 4)))

    (define (get-errortrace-backtrace exn)
      exn)

    ;; show-source : value number number -> void
    (define show-source
      (close-eventspace/async
       (lambda (src pos span)
         (drscheme:debug:open-and-highlight-in-file
          (list (make-srcloc src #f #f pos span))))))

    ;; Send them off to the drscheme-ui module.
    ;; We'll still have to attach our instantiation of drscheme-link
    ;; to the user namespace.
    (set-box! drlink:link
              (vector get-errortrace-backtrace
                      show-backtrace
                      show-source))

    (define drracket-ns (namespace-anchor->namespace drracket-ns-anchor))

    (define interactions-text-mixin
      (mixin ((class->interface drscheme:rep:text%)) ()
        (inherit get-user-namespace)
        (super-new)

        (define/private (setup-helper-module)
          (namespace-attach-module drracket-ns
                                   LINK-MODULE-SPEC
                                   (get-user-namespace)))

        (define/override (reset-console)
          (super reset-console)
          (setup-helper-module))))

    (drscheme:get/extend:extend-interactions-text interactions-text-mixin)

    (define (phase1) (void))
    (define (phase2) (void))

    ))
