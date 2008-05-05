#lang scheme

(provide (struct-out exn:fail:r6rs)
         (struct-out exn:fail:contract:r6rs)
         (struct-out exn:fail:contract:non-continuable)
         (struct-out exn:fail:filesystem:exists-not))

(define-struct (exn:fail:r6rs exn:fail) (message who irritants))
(define-struct (exn:fail:contract:r6rs exn:fail:contract) (message who irritants))
(define-struct (exn:fail:contract:non-continuable exn:fail:contract) ())

(define-struct (exn:fail:filesystem:exists-not exn:fail:filesystem) (filename))
