#lang racket
(require "path-utils.rkt"
         "archive.rkt"
         tests/eli-tester)

(define archive
  "../test.archive")

(test
 (create-archive archive (current-directory))
 
 (for ([fp (in-list (directory-list* (current-directory)))]
       #:when (file-exists? fp))
   (test
    (archive-extract-file archive (build-path (current-directory) fp)) => (file->bytes fp)))
 (archive-extract-file archive "test") =error> #rx"not in the archive"
 (archive-extract-file archive (build-path (current-directory) "test")) =error> #rx"not in the archive"
 (archive-extract-file archive (build-path (current-directory) "static")) =error> #rx"not a file"
 
 (archive-extract-file "archive-test.rkt" (build-path (current-directory) "archive-test.rkt")) =error> #rx"not a valid archive"
 
 (directory-list->directory-list* (archive-directory-list archive (current-directory)))
 => (directory-list* (current-directory))
 
 (archive-directory-exists? archive (current-directory)) => #t
 (archive-directory-exists? archive (build-path (current-directory) "static")) => #t
 
 (archive-directory-exists? archive (build-path (current-directory) "unknown")) => #f
 
 (archive-directory-exists? archive (build-path (current-directory) "archive-test.rkt")) => #f
 )

