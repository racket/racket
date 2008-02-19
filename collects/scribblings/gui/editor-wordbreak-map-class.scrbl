#lang scribble/doc
@(require "common.ss")

@defclass/title[editor-wordbreak-map% object% ()]{

An @scheme[editor-wordbreak-map%] objects is used with a
 @scheme[text%] objects to specify word-breaking criteria for the
 default wordbreaking function.  See also @method[text%
 set-wordbreak-map], @method[text% get-wordbreak-map], @method[text%
 find-wordbreak], and @method[text% set-wordbreak-func].

A global object @scheme[the-editor-wordbreak-map] is created
 automatically and used as the default map for all @scheme[text%]
 objects.

A wordbreak objects implements a mapping from each character to a list
  of symbols. The following symbols are legal elements of the list:

@itemize{
@item{@indexed-scheme['caret]}
@item{@indexed-scheme['line]}
@item{@indexed-scheme['selection]}
@item{@indexed-scheme['user1]}
@item{@indexed-scheme['user2]}
}

The presence of a flag in a character's value indicates that the
 character does not break a word when searching for breaks using the
 corresponding reason. For example, if @scheme['caret] is present,
 then the character is a non-breaking character for caret-movement
 words. (Each stream of non-breaking characters is a single word.)



@defconstructor/make[()]{

All ASCII alpha-numeric characters are initialized with
 @scheme['(caret line selection)]. All other ASCII non-whitespace
 characters except @litchar{-} are initialized with
 @scheme['(line)]. All ASCII whitespace characters and @litchar{-} are
 initialized with @scheme[null].

}

@defmethod[(get-map [char char?])
           (listof symbol?)]{

Gets the mapping value for @scheme[char].  See
@scheme[editor-wordbreak-map%] for more information.

}

@defmethod[(set-map [char char?]
                    [value (listof symbol?)])
           void?]{


Sets the mapping value for @scheme[char] to @scheme[value].  See
@scheme[editor-wordbreak-map%] for more information.

}}

