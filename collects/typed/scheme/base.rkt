#lang s-exp typed-scheme/minimal
           
(providing (libs (except scheme/base #%module-begin #%top-interaction with-handlers lambda #%app define-struct for for*))
	   (basics #%module-begin #%top-interaction lambda #%app))

(require typed-scheme/base-env/extra-procs
         typed-scheme/base-env/prims
         typed-scheme/base-env/base-types
         typed-scheme/base-env/base-types-extra
	 (for-syntax typed-scheme/base-env/base-types-extra))
(provide (rename-out [with-handlers: with-handlers]
                     [define-type-alias define-type])
         (except-out (all-from-out typed-scheme/base-env/prims)
                     with-handlers: for/annotation for*/annotation)
         (all-from-out typed-scheme/base-env/base-types)
         (all-from-out typed-scheme/base-env/base-types-extra)
	 assert defined? with-type for for*
         (for-syntax (all-from-out typed-scheme/base-env/base-types-extra)))
