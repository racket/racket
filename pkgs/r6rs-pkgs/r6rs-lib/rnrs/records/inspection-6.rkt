#!r6rs

(library (rnrs records inspection (6))
  (export record-type-name
	  record-type-parent
	  record-type-sealed?
	  record-type-uid
	  record-type-generative?
	  record-type-field-names
	  record-type-opaque?
	  record-field-mutable?
	  record? record-rtd)
  (import (r6rs private records-core)))
