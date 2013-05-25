#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "portstructs"]{Structures as Ports}

@defthing[prop:input-port struct-type-property?]
@defthing[prop:output-port struct-type-property?]

The @racket[prop:input-port] and @racket[prop:output-port] structure type
properties identify structure types whose instances can serve as input
and output ports, respectively.

Each property value can be either of the following:

@itemize[
 
 @item{An input port (for @racket[prop:input-port]) or output port
  (for @racket[prop:output-port]): In this case, using the structure
  as port is equivalent to using the given input or output port.}

 @item{An exact, non-negative integer between @racket[0] (inclusive) and
  the number of non-automatic fields in the structure type (exclusive, not
  counting supertype fields): The integer identifies a field in
  the structure, and the field must be designated as immutable. If the
  field contains an input port (for @racket[prop:input-port]) or
  output port (for @racket[prop:output-port]), the port is used.
  Otherwise, an empty string input port is used for @racket[prop:input-port],
  and a port that discards all data is used for @racket[prop:output-port].}

]

Some procedures, such as @racket[file-position], work on both input
and output ports. When given an instance of a structure type with both
the @racket[prop:input-port] and @racket[prop:output-port] properties,
the instance is used as an input port.
