#lang scribble/doc
@(require "common.ss")

@title[#:tag "documents"]{Documents}

 A browser contains one document at a time.  If 
 hyperlinks are clicked, or the navigation methods
 (navigate, go-forward, go-back) are used, the 
 document changes.

@definterface[mx-document<%> ()]{

@defmethod[(insert-html [html string?]) void?]{

  Inserts the specified HTML string at the
  beginning of the document.}

@defmethod[(append-html [html string?]) void?]{

  Appends the specified HTML string at the end of
  the document.}

@defmethod[(replace-html [html string?]) void?]{

  Replace the current HTML in the document with
  the specified HTML string.}

@defmethod[(objects) (listof com-object?)]{

  Returns a list of COM objects, including ActiveX
  controls, that occur in the document.  The order
  of the objects is the same as in the document.}

@defmethod[(insert-object-from-coclass [coclass string?]
                                       [width exact-integer?]
                                       [height exact-integer?]
                                       [size (one-of/c 'pixels 'percent) 'pixels])
           com-object?]{

  Inserts a COM object with class @scheme[coclass] at the beginning of
  the document.  The optional @scheme[size] argument gives an
  interpretation for the width and height, where @scheme['percent]
  indicates that the width and height are a fixed percentage of the
  document window size.}

@defmethod[(insert-object-from-progid [progid string?]
                                       [width exact-integer?]
                                       [height exact-integer?]
                                       [size (one-of/c 'pixels 'percent) 'pixels])
           com-object?]{

  Like @method[mx-document<%> insert-object-from-coclass], but with a
  ProgID instead of a COM class.}

@defmethod[(append-object-from-coclass [coclass string?]
                                       [width exact-integer?]
                                       [height exact-integer?]
                                       [size (one-of/c 'pixels 'percent) 'pixels])
           com-object?]{

 Like @method[mx-document<%> insert-object-from-coclass], but adds to the
 end of the document.}


@defmethod[(append-object-from-progid [progid string?]
                                       [width exact-integer?]
                                       [height exact-integer?]
                                       [size (one-of/c 'pixels 'percent) 'pixels])
           com-object?]{

 Like @method[mx-document<%> insert-object-from-progid], but adds to the
 end of the document.}

@defmethod[(title) string?]{

  Returns a string indicating the document's title, that is,
  the text that appears within HTML TITLE tags.  If the document 
  has no title, the empty string is returned.}

@defmethod[(find-element [tag string?]
                         [id string?]
                         [index exact-nonnegative-integer? 0])
           (is-a?/c mx-element%)]{

  Returns an object that encapsulates an HTML element, where
  @scheme[tag] names an HTML tag, and @scheme[id] names the @scheme["id"]
  attribute of the HTML element.  The @scheme[index] is a nonnegative
  integer indicating the zero-based index of the element among all
  elements with the same @scheme[tag] and @scheme[id].  The ordering
  of elements is defined by Internet Explorer.  The requested element
  must be within the document's @scheme["body"] tags or the
  @scheme["body"] element itself.}

@defmethod[(find-element-by-id-or-name
                         [id string?]
                         [index exact-nonnegative-integer? 0])
           (is-a?/c mx-element%)]{

  Returns an object that encapsulates an HTML element, where
  @scheme[id] names either the @scheme["id"] or @scheme["name"]
  attribute of the HTML element.  The @scheme[index] is a nonnegative
  integer indicating the zero-based index of the element among all
  elements with the same @scheme["id"] or @scheme["name"].  The ordering
  of elements is defined by Internet Explorer.  The requested element
  must be within the document's @scheme["body"] tags or the
  @scheme["body"] element itself.}

@defmethod[(elements-with-tag [tag string?])
           (listof (is-a?/c mx-element%))]{

  Returns a list of elements with the HTML tag given by @scheme[tag].
  The requested elements must be within the document's @scheme["body"]
  tags or the @scheme["body"] element itself.}

}
