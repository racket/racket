#lang scribble/doc
@(require "common.ss")
@title{@tt{drscheme:get/extend}}

@defclass[drscheme:get/extend:base-unit-frame% (drscheme:debug:profile-unit-frame-mixin) ()]{



@defconstructor[()]{
Passes all arguments to @scheme[super-init].
}}


@defclass[drscheme:get/extend:base-tab% () ()]{



@defconstructor[()]{
Passes all arguments to @scheme[super-init].
}}


@defclass[drscheme:get/extend:base-interactions-text% (drscheme:debug:profile-interactions-text-mixin) ()]{



@defconstructor[()]{
Passes all arguments to @scheme[super-init].
}}


@defclass[drscheme:get/extend:base-interactions-canvas% (canvas:delegate-mixin canvas:info-mixin) ()]{



@defconstructor/make[()]{

Calls @scheme[super-new], adding @scheme['hide-hscroll] to the style argument.


}

@defconstructor[()]{
Passes all arguments to @scheme[super-init].
}

@defmethod[#:mode override 
           (on-focus)
           void?]{

When the focus is on, calls
@method[drscheme:unit:frame% make-searchable] with @scheme[this].


}}


@defclass[drscheme:get/extend:base-definitions-text% (drscheme:debug:profile-definitions-text-mixin) ()]{



@defconstructor[()]{
Passes all arguments to @scheme[super-init].
}}


@defclass[drscheme:get/extend:base-definitions-canvas% (canvas:delegate-mixin canvas:info-mixin) ()]{



@defconstructor/make[()]{

Calls @scheme[super-new], adding @scheme['hide-hscroll] to the style argument.


}

@defconstructor[()]{
Passes all arguments to @scheme[super-init].
}

@defmethod[#:mode override 
           (on-focus)
           void?]{

When the focus is on, calls
@method[drscheme:unit:frame% make-searchable] with @scheme[this].

%%                                                                  %%                        drscheme:debug                            %%                                                                  

}}

