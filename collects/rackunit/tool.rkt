#lang racket/base
(require racket/class
         racket/gui/base
         drscheme/tool
         racket/unit)

(provide tool@)

;; CONSTANTS

(define BACKTRACE-NO-MESSAGE "No message.")
(define LINK-MODULE-SPEC 'rackunit/private/gui/drracket-link)

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

    (define interactions-text-mixin
      (mixin ((class->interface drscheme:rep:text%)) ()
        (inherit get-user-namespace)
        (super-new)

        (define/private (setup-helper-module)
          (let ([link (parameterize ((current-namespace (get-user-namespace)))
                        (dynamic-require LINK-MODULE-SPEC 'link))])
            (set-box! link (vector get-errortrace-backtrace
                                   show-backtrace
                                   show-source))))

        (define/override (reset-console)
          (super reset-console)
          (setup-helper-module))))

    (drscheme:get/extend:extend-interactions-text interactions-text-mixin)

    (define (phase1) (void))
    (define (phase2) (void))

    ))
