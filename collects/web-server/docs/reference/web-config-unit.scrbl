#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:style 'toc]{Web Config Unit}

The @file{web-server} offers a unit-based approach to configuring the server.

@file{web-config-sig.ss} provides the signature
@defthing[web-config^ signature?] signature, which contains the following
identifiers:

@defthing[max-waiting integer?]{
 Passed to @scheme[tcp-accept].
}

@defthing[virtual-hosts (listof (cons/c string? host-table?))]{
 Contains the configuration of individual virtual hosts.
}

@; XXX Remove access
@defthing[access any/c]{Unused.}

@defthing[scripts (box/c (cache-table? path? servlet?))]{
 Contains initially loaded servlets.
}

@defthing[initial-connection-timeout integer?]{
 Specifies the initial timeout given to a connection.
}

@defthing[port (between/c 1 65535)]{
 Specifies the port to serve HTTP on.
}

@defthing[listen-ip string?]{
 Passed to @scheme[tcp-accept].
}

@; XXX Remove instances
@defthing[instances any/c]{Unused.}

@defthing[make-servlet-namespace make-servlet-namespace?]{
 Passed to @scheme[servlets:make].
}

@file{web-config-unit.ss} provides the following:

@; XXX Move to configuration/configuration-table.ss
@defthing[default-configuration-table-path path?]{The default configuration table.}

@; XXX Make port?
@defproc[(configuration-table->web-config\@ [path path?]
                                           [#:port port (or/c false/c (between/c 1 65535)) #f]
                                           [#:listen-ip listen-ip (or/c false/c string?) #f]
                                           [#:make-servlet-namespace make-servlet-namespace make-servlet-namespace? (make-make-servlet-namespace)])
         (unit? web-config^)]{
 Reads the S-expression at @scheme[path] and calls @scheme[configuration-table-sexpr->web-config\@] appropriately.
}

@defproc[(configuration-table-sexpr->web-config\@ [sexpr list?]
                                                 [#:web-server-root web-server-root path? (directory-part default-configuration-table-path)]
                                                 [#:port port (or/c false/c (between/c 1 65535)) #f]
                                                 [#:listen-ip listen-ip (or/c false/c string?) #f]
                                                 [#:make-servlet-namespace make-servlet-namespace make-servlet-namespace? (make-make-servlet-namespace)])
         (unit? web-config^)]{
 Parses @scheme[sexpr] as a configuration-table and constructs a @scheme[web-config^] unit.
}
