#lang s-exp "minimal.ss"
           


(providing (libs (except scheme/base #%module-begin #%top-interaction with-handlers number? lambda #%app)
                 (except "private/prims.ss")
                 (except "private/base-types.ss")
                 (except "private/base-types-extra.ss"))
	   (basics #%module-begin		   		   		   
		   #%top-interaction
		   lambda
		   #%app))
(require "private/base-env.ss" 
	 "private/base-special-env.ss"
	 "private/base-env-numeric.ss"
	 "private/base-env-indexing-old.ss"
	 "private/extra-procs.ss"
         (for-syntax "private/base-types-extra.ss"))
(provide (rename-out [with-handlers: with-handlers] [real? number?])
         (for-syntax (all-from-out "private/base-types-extra.ss"))
	 assert with-type)
