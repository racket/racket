#lang typed-racket/minimal

(providing (libs (except racket/base #%module-begin #%top-interaction
                         with-handlers default-continuation-prompt-tag
                         define λ lambda define-struct for for*))
	   (basics #%module-begin #%top-interaction))

(require typed-racket/base-env/extra-procs
         (except-in typed-racket/base-env/prims
           require-typed-struct-legacy
           require/typed-legacy)
         typed-racket/base-env/base-types
         typed-racket/base-env/base-types-extra
	 (for-syntax typed-racket/base-env/base-types-extra))
(provide (rename-out [define-type-alias define-type])
         (all-from-out typed-racket/base-env/prims)
         (all-from-out typed-racket/base-env/base-types)
         (all-from-out typed-racket/base-env/base-types-extra)
	 assert defined? with-type for for*
         (for-syntax (all-from-out typed-racket/base-env/base-types-extra)))
