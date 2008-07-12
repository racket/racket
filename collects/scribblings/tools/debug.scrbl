#lang scribble/doc
@(require "common.ss")
@(tools-title "debug")

@defmixin[drscheme:debug:profile-unit-frame-mixin
          (drscheme:frame:<%> drscheme:unit:frame<%>)
          ()]{
}

@defmixin[drscheme:debug:profile-interactions-text-mixin
          (drscheme:rep:text<%>)
          ()]{
}

@defmixin[drscheme:debug:profile-definitions-text-mixin
          (drscheme:unit:definitions-text<%> text%)
          ()]{
}

@(tools-include "debug")
