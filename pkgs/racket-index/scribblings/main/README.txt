How Documentaton Listing and Search Works
=========================================

Propagating Context Across Pages
--------------------------------

There are a few reasons why context from one page of documentation
might need to be carried through to other pages:

 * To select a language family for searches.

 * To find the document that should be considered the root for
   searches.

Local storage would be one way to keep that informaton, but modern
browsers don't propagate local store across `file://` links. In
general, context is put in the query part of a URL, instead, and when
a documentation page is loaded, all URLs via links that have the
"pltdoc" attribute are updated to propagate the query for any key that
does not start "q".

This query propagation is implemented mainly as part of Scribble in
"scribble-common.js"

Installation vs. User Documentation
-----------------------------------

Packages may be installed in user scope.

 * The installation has its own "index.html" to list all documentation
   in installation scope, and it has its own "search/" to search in
   installation-scope packages.

 * The user's addon directory has its own "index.html" to list all
   documentation (in all scopes), and it has its own "search/" to
   search in all packages.

If the user-specific page is ever opened, then it records the location
of that page, and installation-scope pages can go back there for
seaches or "top" links. The redirection is handled by `GotoPLTRoot` at
the JavaScript level, which uses the root as propagated via a
propagated `PLT_Root` value. In the case of a URL other than `file:`,
local storage is used (even though this seems unlikely to happen or be
useful in practice).

The `GotoPLTRoot` JavaScript function is implemented in Scibble as
part of "scribble-common.js", but the uses are injected both by the
HTML renderer and here.

See also "Rendering Documentation Listings" in the `raco setup`
documentation.

Search Configuration
--------------------

When the user configures search through the gear icon on the search
page, the confguration is preserved in local storage (fallback for old
browsers: a cookie). The configuartion will not get propagated to
other pages, but that's fine: a search should return to the same URL.

The search panel's mechanics in general are implemented here in
"private/search.js".

Language Family Configuration
-----------------------------

A language family can be configured with through a `fam` query
parameter (typically a capitalized name), and that's used to order
search results --- as opposed to completely filtering out non-matching
index entries, which is handled by search configuration.

If `famroot` is also specified, then links intended to go to the root
document that lists all documentation will instead go to the named
document (typically a case-folded name).

A `fam` and `famroot` configuration is propagated through the query
like any other propagated information. It's used by a combination of
JavaScript in "scribbe-common.js" and "private/search.js".

Nothing in the documentation build generates uses of `fam` and
`famroot` directly. They are meant to be selected by external links
that jump into documentation with an intended language-family context.
