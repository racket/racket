#lang scribble/doc
@(require "common.ss")

@title[#:tag "wheres-the-collection"]{Where is the Collection?}

If you obtained the server and client by installing a @filepath{.plt}
file, then the @filepath{handin-server} and @filepath{handin-client}
directories might be in your Racket addon space.  Start Racket, and
enter @schemeblock[(collection-path "handin-server")]
@schemeblock[(collection-path "handin-client")] to find out where
these collections are.
