#lang scribble/manual
@(require "common.rkt")

@title[#:tag "zuo"]{Zuo and the Racket Build System}

Racket builds with many options, and the build needs to work in a
variety of environments. That variability is difficult to manage
through a traditional makefile. The Racket build is mostly driven
instead with Zuo, which is a tiny, Racket-like scripting language with
facilities inspired by @exec{make} and
@hyperlink["https://shakebuild.com/"]{Shake}. When you build Racket
with @exec{make}, the makefile target ensures that @exec{zuo} is
built, and then it bounces the build request to a @filepath{main.zuo}
script.

Racket makefiles build @exec{zuo} using the @exec{CC_FOR_BUILD}
makefile variable plus @exec{CFLAGS_FOR_BUILD}. The
@exec{CC_FOR_BUILD} variable defaults to using the @exec{CC} makefile
variable plus @exec{-O2}, while @exec{CC} normally defaults to
@exec{cc}. If you need to specify a C compiler or options for building
Zuo, supply @exec{CC=@nonterm{compiler}},
@exec{CC_FOR_BUILD=@nonterm{compiler}}, and/or
@exec{CFLAGS_FOR_BUILD=@nonterm{flags}} to @exec{make}.

In you have @exec{zuo} installed, you can generally substitute
@exec{zuo .} in place of @exec{make} when building Racket components.
You can even use just @exec{zuo} in place of @exec{make} if you're not
providing additional target or variable arguments to @exec{make}, but
otherwise @exec{.} is needed after @exec{zuo} to select the
@exec{main.zuo} script in the current directory. In most cases, it
doesn't matter whether you use @exec{make} or @exec{zuo .}, but if you
move deep enough into the Racket build tree, there are only Zuo
scripts. To install Zuo, you can use the usual @exec{configure && make
&& make install} in @filepath{racket/src/zuo}.

Even when you run @exec{zuo} directly, configuration information is
frequently read from @filepath{Makefile} or @filepath{Mf-config}. The
latter name is used when the makefile exists only for recording a
configuration and does not provide targets. When you run a
@exec{configure} script, configuration choices are recorded in a
generated @filepath{Makefile} or @filepath{Mf-config}.

By convention, a source file @filepath{build.zuo} is analogous to
@filepath{Makefile.in}: it is meant to be instantiated in a build
directory as @filepath{main.zuo}. Instead of copying and updating, as
typically happens to convert @filepath{Makefile.in} to
@filepath{Makefile}, a @filepath{main.zuo} is typically instantiated
as a small module, possibly by copying a @filepath{buildmain.zuo} file
to @filepath{main.zuo}. That @filepath{main.zuo} reaches
@filepath{build.zuo} using a source directory that is recorded in an
accompanying @filepath{Makefile} or @filepath{Mf-config}.
