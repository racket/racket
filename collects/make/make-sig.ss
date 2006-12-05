
(module make-sig (lib "a-signature.ss")
     make/proc
     make-print-checking
     make-print-dep-no-line
     make-print-reasons
     make-notify-handler
     (struct exn:fail:make (target orig-exn)))

