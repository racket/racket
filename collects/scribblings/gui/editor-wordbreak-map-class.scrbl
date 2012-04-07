#lang scribble/doc
@(require "common.rkt")

@defclass/title[editor-wordbreak-map% object% ()]{

An @racket[editor-wordbreak-map%] objects is used with a
 @racket[text%] objects to specify word-breaking criteria for the
 default wordbreaking function.  See also @method[text%
 set-wordbreak-map], @method[text% get-wordbreak-map], @method[text%
 find-wordbreak], and @method[text% set-wordbreak-func].

A global object @racket[the-editor-wordbreak-map] is created
 automatically and used as the default map for all @racket[text%]
 objects.

A wordbreak objects implements a mapping from each character to a list
  of symbols. The following symbols are legal elements of the list:

@itemize[
@item{@indexed-racket['caret]}
@item{@indexed-racket['line]}
@item{@indexed-racket['selection]}
@item{@indexed-racket['user1]}
@item{@indexed-racket['user2]}
]

The presence of a flag in a character's value indicates that the
 character does not break a word when searching for breaks using the
 corresponding reason. For example, if @racket['caret] is present,
 then the character is a non-breaking character for caret-movement
 words. (Each stream of non-breaking characters is a single word.)



@defconstructor[()]{

All ASCII alpha-numeric characters are initialized with
 @racket['(caret line selection)]. All other ASCII non-whitespace
 characters except @litchar{-} are initialized with
 @racket['(line)]. All ASCII whitespace characters and @litchar{-} are
 initialized with @racket[null].

}

@defmethod[(get-map [char char?])
           (listof (or/c 'caret 'line 'selection 'user1 'user2))]{

Gets the mapping value for @racket[char].  See
@racket[editor-wordbreak-map%] for more information.

}

@defmethod[(set-map [char char?]
                    [value (listof (or/c 'caret 'line 'selection 'user1 'user2))])
           void?]{


Sets the mapping value for @racket[char] to @racket[value].  See
@racket[editor-wordbreak-map%] for more information.

}}

