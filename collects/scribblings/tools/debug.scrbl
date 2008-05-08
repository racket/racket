#lang scribble/doc
@(require "common.ss")
@title{@tt{drscheme:debug}}

@defmixin[drscheme:debug:profile-unit-frame-mixin (drscheme:frame:<%> drscheme:unit:frame<%>) ()]{}

@defmixin[drscheme:debug:profile-interactions-text-mixin (drscheme:rep:text<%>) ()]{}

@defmixin[drscheme:debug:profile-definitions-text-mixin (drscheme:unit:definitions-text<%> text%) ()]{}

@(include-extracted (lib "tool-lib.ss" "drscheme") #rx"^drscheme:debug:")
