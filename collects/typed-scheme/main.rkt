#lang s-exp "minimal.rkt"



(providing (libs (except scheme/base #%module-begin #%top-interaction with-handlers lambda #%app for for*)
                 (except "base-env/prims.rkt")
                 (except "base-env/base-types.rkt")
                 (except "base-env/base-types-extra.rkt"))
	   (basics #%module-begin
		   #%top-interaction
		   lambda
		   #%app))
(require "base-env/extra-procs.rkt"
         (for-syntax "base-env/base-types-extra.rkt"))
(provide (rename-out [with-handlers: with-handlers])
         (for-syntax (all-from-out "base-env/base-types-extra.rkt"))
	 assert defined? with-type for for*)
