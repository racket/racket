#lang scheme/base

(provide (rename-out [eof eof-object])
         eof-object?
         call-with-input-file
         call-with-output-file
         input-port?
         output-port?
         current-input-port
         current-output-port
         current-error-port
         with-input-from-file
         with-output-to-file
         open-input-file
         open-output-file
         close-input-port
         close-output-port
         read-char
         peek-char
         read
         write-char
         newline
         display
         write)
