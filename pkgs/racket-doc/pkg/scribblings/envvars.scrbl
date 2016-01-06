#lang scribble/manual
@(require "common.rkt")

@title[#:tag "envvars"]{Package Management Environment Variables}

If the @indexed-envvar{PLT_PKG_SSL_NO_VERIFY} environment variable is
set, server certificates are not validated for HTTPS connections. When
accessing Git servers over HTTPS, @envvar{GIT_SSL_NO_VERIFY} must be
set, too, to disable certificate validation.

As noted in the specification of GitHub-repository package sources, if
the @envvar{PLT_USE_GITHUB_API} environment variable is set, GitHub
packages are obtained using the GitHub API protocol instead of using
the Git protocol.
