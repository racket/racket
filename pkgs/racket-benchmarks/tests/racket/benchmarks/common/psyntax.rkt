#lang r5rs
(#%require scheme/include
           (only scheme/base time current-directory error)
	   (only mzlib/etc this-expression-source-directory))
(current-directory (this-expression-source-directory))
(include "psyntax.sch")

