#lang scribble/doc
@(require "common.rkt")
@(tools-title "debug")

@defmixin[drracket:debug:profile-unit-frame-mixin
          (drracket:frame:<%> drracket:unit:frame<%>)
          ()]{
}

@defmixin[drracket:debug:profile-interactions-text-mixin
          (drracket:rep:text<%>)
          ()]{
}

@defmixin[drracket:debug:profile-definitions-text-mixin
          (drracket:unit:definitions-text<%> text%)
          ()]{
}

@(tools-include "debug")
