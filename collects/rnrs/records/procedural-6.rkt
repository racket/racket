#!r6rs

(library (rnrs records procedural (6))
  (export make-record-type-descriptor
	  record-type-descriptor?
	  make-record-constructor-descriptor record-constructor
	  record-predicate
	  record-accessor record-mutator)
  (import (r6rs private records-core)))
