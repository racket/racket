#lang scribble/manual
@(require "common.rkt")

@title[#:tag "bootstrap"]{Bootstrapping Racket}

Although Racket is implemented in Racket, you do not normally need an
existing Racket installation to build Racket. Distribution archives
include the needed bootstrapping artifacts in a portable form. The
Racket Git repository similarly includes some of those artifacts
checked in directly, and some are in a separate repository that is
downloaded by @exec{make}. Specifically:

@itemlist[

 @item{@filepath{racket/src/cs/schemified} includes macro-expanded,
       schemified versions of layers that are implemented in Racket
       for Racket CS, and these are checked into the Git repository;}
 
 @item{@filepath{racket/src/bc/srcstartup.inc} is the macro-expanded
       expander (as implemented in Racket) for Racket BC, and it is
       checked into the Git repository; and}

 @item{@filepath{racket/src/ChezScheme/boot/pb} contains Chez Scheme
       pb (portable bytecode) boot files, normally downloaded from a
       separate Git repository in a branch that has a single commit
       (i.e., no history of old versions within the branch).}
 
]

If you modify certain pieces of Racket, you will need an existing
build of Racket to bootstrap. That includes the Chez Scheme
implementation (at least for some kinds of modifications), the Racket
macro expander, and in the case of Racket CS, the "thread", "io",
"regexp", and "schemify" layers.

For more information about modifying Chez Scheme, see
@filepath{racket/src/cs/README.txt}. As explained there, you can
create new boot files in @filepath{racket/src/ChezScheme/boot/pb} or
platform-specific boot files using even a relatively old version of
Chez Scheme or Racket.

For information about modifying the macro expander for Racket CS
and/or BC, see @filepath{racket/src/expander/README.txt}. Building the
expander may require a relatively new version of Racket, perhaps even
the very latest version before the change.

Finally, for information about modifying the other layers for Racket
CS, see @filepath{racket/src/cs/README.txt}. Rebuilding these layers
requires a relatively new version of Racket, too.
