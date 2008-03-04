#lang s-exp "minimal.ss"
           
(providing (libs (except scheme/base require #%module-begin #%top-interaction with-handlers lambda #%app))
	   (basics #%module-begin		   		   		   
		   #%top-interaction
		   with-handlers
		   lambda
		   #%app)
	   (from scheme require))
