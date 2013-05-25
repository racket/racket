#lang scribble/manual
@(require "../utils.rkt"
          (for-label unstable/gui/prefs
                     racket/contract
                     racket/base))

@title[#:tag "gui-prefs"]{Preferences}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/gui/prefs]

@defproc[(pref:get/set [pref symbol?])
         (case-> (-> any/c) (-> any/c void?))]{

Returns a procedure that when applied to zero arguments retrieves the
current value of the preference
(@racketmodname[framework/preferences]) named @racket[pref] and when
applied to one argument updates the preference named @racket[pref].

}
