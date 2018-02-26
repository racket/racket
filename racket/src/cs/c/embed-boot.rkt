#lang racket/base
(require racket/cmdline
         racket/file
         compiler/private/mach-o
         compiler/private/elf)

(command-line
 #:args (src-file dest-file boot-dir racket.so)

 (define bstr1 (file->bytes (build-path boot-dir "petite.boot")))
 (define bstr2 (file->bytes (build-path boot-dir "scheme.boot")))
 (define bstr3 (file->bytes racket.so))

 (with-handlers ([exn? (lambda (x)
                         (when (file-exists? dest-file)
                           (delete-file dest-file))
                         (raise x))])
   (define data
     (bytes-append bstr1 #"\0"
                   bstr2 #"\0"
                   bstr3 #"\0"))
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
        (cond
          [start-pos
           ;; Success as ELF
           start-pos]
          [else
           ;; Not ELF; just append to the end
           (copy-file src-file dest-file #t)
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
   
   (file-position o (cdar m))
   (void (write-bytes (integer->integer-bytes pos 4 #t #f) o))
   (void (write-bytes (integer->integer-bytes (+ pos (bytes-length bstr1) 1) 4 #t #f) o))
   (void (write-bytes (integer->integer-bytes (+ pos (bytes-length bstr1) (bytes-length bstr2) 2) 4 #t #f) o))))
