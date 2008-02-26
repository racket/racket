#lang scheme

(provide (struct-out exn:fail:r6rs)
         (struct-out exn:fail:contract:r6rs))

(define-struct (exn:fail:r6rs exn:fail) (who irritants))
(define-struct (exn:fail:contract:r6rs exn:fail:contract) (who irritants))

