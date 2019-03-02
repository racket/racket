#lang racket/base
(require racket/cmdline
         racket/file
         compiler/private/mach-o
         compiler/private/pe-rsrc
         compiler/private/elf
         "adjust-compress.rkt")

(define expect-elf? #f)
(define alt-dests '())
(define target #f)

(command-line
 #:once-each
 [("--compress") "Leave compiled code files as compressed"
  (enable-compress!)]
 [("--expect-elf") "Record offset from ELF section"
  (set! expect-elf? #t)]
 [("--target") machine "Select target machine"
  (set! target machine)]
 #:multi
 [("++exe") src dest "Select an alternative executable"
  (set! alt-dests (cons (cons src dest) alt-dests))]
 #:args (src-file dest-file petite.boot scheme.boot racket.boot)

 (define bstr1 (adjust-compress (file->bytes petite.boot)))
 (define bstr2 (adjust-compress (file->bytes scheme.boot)))
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
     (case (or target (path->string (system-library-subpath #f)))
       [("x86_64-darwin" "i386-darwin" "x86_64-macosx" "i386-macosx")
        ;; Mach-O
        (copy-file src-file dest-file #t)
        (add-plt-segment dest-file data #:name #"__RKTBOOT")]
       [("ta6nt" "ti3nt" "win32\\x86_64" "win32\\i386")
        (copy-file src-file dest-file #t)
        (define-values (pe rsrcs) (call-with-input-file*
                                   dest-file
                                   read-pe+resources))
        (define new-rsrcs (resource-set rsrcs
                                        ;; Racket's "user-defined" type for boot:
                                        259
                                        1
                                        1033 ; U.S. English
                                        data))
	(update-resources dest-file pe new-rsrcs)
	;; Find resource at run time:
	0]
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
           (cond
             [expect-elf?
              ;; Find ".rackboot" at run time:
              0]
             [else
              start-pos])]
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
           (when expect-elf?
             (error 'embed-boot "expected ELF"))
           pos])]))

   (define (write-offsets dest-file)
     (define-values (i o) (open-input-output-file dest-file #:exists 'update))
     (define m (regexp-match-positions #rx"BooT FilE OffsetS:" i))
     (unless m
       (error 'embed-boot "cannot file boot-file offset tag"))

     (define terminator-len (bytes-length terminator))
     
     (file-position o (cdar m))
     (void (write-bytes (integer->integer-bytes pos 4 #t #f) o))
     (void (write-bytes (integer->integer-bytes (+ pos (bytes-length bstr1) terminator-len) 4 #t #f) o))
     (void (write-bytes (integer->integer-bytes (+ pos (bytes-length bstr1) (bytes-length bstr2) (* 2 terminator-len)) 4 #t #f) o)))

   (cond
    [(null? alt-dests)
     (write-offsets dest-file)]
    [else
     (for ([alt (in-list alt-dests)])
	  (copy-file (car alt) (cdr alt) #t)
	  (write-offsets (cdr alt)))])))
     
 
