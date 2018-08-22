#lang racket/base
(require racket/cmdline
         racket/file
         compiler/private/mach-o
         compiler/private/elf
         "adjust-compress.rkt")

(command-line
 #:once-each
 [("--compress") "Leave compiled code files as compressed"
  (enable-compress!)]
 #:args (src-file dest-file boot-dir racket.boot)

 (define bstr1 (adjust-compress (file->bytes (build-path boot-dir "petite.boot"))))
 (define bstr2 (adjust-compress (file->bytes (build-path boot-dir "scheme.boot"))))
 (define bstr3 (adjust-compress (file->bytes racket.boot)))

 (with-handlers ([exn? (lambda (x)
                         (when (file-exists? dest-file)
                           (delete-file dest-file))
                         (raise x))])
   (define terminator
     (if (compress-enabled?)
         ;; zero byte stops a gzip-read sequence
         #"\0"
         ;; #!eof encoding stops(!) a fasl-read sequence
         #"\26\4\fl"))
   (define data
     (bytes-append bstr1 terminator
                   bstr2 terminator
                   bstr3 terminator))
   (define pos
     (case (path->string (system-library-subpath #f))
       [("x86_64-darwin" "i386-darwin" "x86_64-macosx" "i386-macosx")
        ;; Mach-O
        (copy-file src-file dest-file #t)
        (add-plt-segment dest-file data #:name #"__RKTBOOT")]
       [else
        ;; ELF?
        (define-values (start-pos end-pos any1 any2)
          (add-racket-section src-file dest-file #".rackboot"
                              (lambda (pos)
                                (values data 'any1 'any2))))
        (define (ensure-executable dest-file)
          (let* ([perms1 (file-or-directory-permissions dest-file 'bits)]
                 [perms2 (bitwise-ior user-read-bit user-write-bit user-execute-bit
                                      perms1)])
            (unless (equal? perms1 perms2)
              (file-or-directory-permissions dest-file perms2))))
        (cond
          [start-pos
           ;; Success as ELF
           (ensure-executable dest-file)
           start-pos]
          [else
           ;; Not ELF; just append to the end
           (copy-file src-file dest-file #t)
           (ensure-executable dest-file)
           (define pos (file-size dest-file))
           (call-with-output-file*
            dest-file
            #:exists 'update
            (lambda (o)
              (file-position o pos)
              (write-bytes data o)))
           pos])]))

   (define-values (i o) (open-input-output-file dest-file #:exists 'update))
   (define m (regexp-match-positions #rx"BooT FilE OffsetS:" i))
   (unless m
     (error 'embed-boot "cannot file boot-file offset tag"))

   (define terminator-len (bytes-length terminator))

   (file-position o (cdar m))
   (void (write-bytes (integer->integer-bytes pos 4 #t #f) o))
   (void (write-bytes (integer->integer-bytes (+ pos (bytes-length bstr1) terminator-len) 4 #t #f) o))
   (void (write-bytes (integer->integer-bytes (+ pos (bytes-length bstr1) (bytes-length bstr2) (* 2 terminator-len)) 4 #t #f) o))))
