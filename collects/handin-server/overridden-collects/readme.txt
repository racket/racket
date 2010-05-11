This directory is used (by default) as a first root for searching
collections when evaluating user code.  This means that Racket libraries
that appear here will be used instead of ones in the Racket tree or the
user-local collections.  Use it to override collections that are safe
for testing, for example -- avoid using real GUI.  See also the
documentation for `sandbox-override-collection-paths' in "doc.txt".

This is currently used with the `teachpack' collection.  Note that
Racket resolves collection directories based on toplevel names only,
which means that if we actually use `teachpack' for the directory
name, then files that are not here will not be searched in the usual
plt tree.  Because of this the collection is called `fake-teachpack',
and checkers should specify requires in this collection if submissions
need a fake teachpack.
