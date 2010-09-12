This directory is used (by default) as a first root for searching
collections when evaluating user code.  This means that Racket libraries
that appear here will be used instead of ones in the Racket tree or the
user-local collections.  Use it to override collections that are safe
for testing, for example -- avoid using real GUI.  See also the
documentation for `sandbox-override-collection-paths' in "doc.txt".
