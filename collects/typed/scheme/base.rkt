#lang typed-scheme/minimal

(providing (libs (except scheme/base #%module-begin #%top-interaction with-handlers lambda #%app define-struct for for*))
	   (basics #%module-begin #%top-interaction lambda #%app))

(require typed-scheme/base-env/extra-procs
         (rename-in
           (except-in typed-scheme/base-env/prims
             require-typed-struct
             require/typed)
           (require-typed-struct-legacy require-typed-struct)
           (require/typed-legacy require/typed))
         typed-scheme/base-env/base-types
         typed-scheme/base-env/base-types-extra
	 (for-syntax typed-scheme/base-env/base-types-extra))
(provide (rename-out [define-type-alias define-type])
         (all-from-out typed-scheme/base-env/prims)
         (all-from-out typed-scheme/base-env/base-types)
         (all-from-out typed-scheme/base-env/base-types-extra)
	 assert defined? with-type for for*
         (for-syntax (all-from-out typed-scheme/base-env/base-types-extra)))
