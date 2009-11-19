#lang scribble/manual
@(require "../utils.ss"
          (for-label unstable/gui/prefs
                     scheme/contract
                     scheme/base))

@title[#:tag "gui-prefs"]{Preferences}

@defmodule[unstable/gui/prefs]

@unstable[@author+email["Ryan Culpepper" "ryanc@plt-scheme.org"]]

@defproc[(pref:get/set [pref symbol?])
         (case-> (-> any/c) (-> any/c void?))]{

Returns a procedure that when applied to zero arguments retrieves the
current value of the preference
(@schememodname[framework/preferences]) named @scheme[pref] and when
applied to one argument updates the preference named @scheme[pref].

}
