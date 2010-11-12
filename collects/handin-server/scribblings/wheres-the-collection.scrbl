#lang scribble/doc
@(require "common.rkt")

@title[#:tag "wheres-the-collection"]{Where is the Collection?}

If you obtained the server and client by installing a @filepath{.plt}
file, then the @filepath{handin-server} and @filepath{handin-client}
directories might be in your Racket addon space.  Start Racket, and
enter @racketblock[(collection-path "handin-server")]
@racketblock[(collection-path "handin-client")] to find out where
these collections are.
