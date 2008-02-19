#lang scribble/doc
@(require "mz.ss")

@title[#:tag "portstructs"]{Structures as Ports}

@defthing[prop:input-port struct-type-property?]
@defthing[prop:output-port struct-type-property?]

The @scheme[prop:input-port] and @scheme[prop:output-port] structure type
properties identify structure types whose instances can serve as input
and output ports, respectively.

Each property value can be either of the following:

@itemize{
 
 @item{An input port (for @scheme[prop:input-port]) or output port
  (for @scheme[prop:input-port]): In this case, using the structure
  as port is equivalent to using the given one.}

 @item{An exact, non-negative integer between @scheme[0] (inclusive) and
  number of non-automatic fields in the structure type (exclusive, not
  counting supertype fields): The integer identifies a field in
  the structure, and the field must be designated as immutable. If the
  field contains an input port (for @scheme[prop:input-port]) or
  output port (for @scheme[prop:input-port]), the port is used.
  Otherwise, an empty string input port is used for @scheme[prop:input-port],
  and a port that discards all data is used for @scheme[prop:output-port].}

}

Some procedures, such as @scheme[file-position], work on both input
and output ports. When given an instance of a structure type with both
the @scheme[prop:input-port] and @scheme[prop:output-port] properties,
the instance is used as an input port.
