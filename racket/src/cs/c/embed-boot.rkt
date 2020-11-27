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

 ;; If `src-file` is "", then `dest-file` is used as the src, too

 (define bstr1 (adjust-compress (file->bytes petite.boot)))
 (define bstr2 (adjust-compress (file->bytes scheme.boot)))
 (define bstr3 (adjust-compress (file->bytes racket.boot)))

 (define use-src-file
   (if (equal? src-file "")
       (let ([src-file (path-add-suffix dest-file #"_tmp")])
         (rename-file-or-directory dest-file src-file)
         src-file)
       src-file))
 (define (clean-src)
   (unless (eq? use-src-file src-file)
     (delete-file use-src-file)))

 (with-handlers ([exn? (lambda (x)
                         (clean-src)
                         (when (file-exists? dest-file)
                           (delete-file dest-file))
                         (raise x))])
   (define terminator
     ;; A 127 byte teriminates a fasl-read sequence
     #"\177")
   (define data
     (bytes-append bstr1 terminator
                   bstr2 terminator
                   bstr3 terminator))
   (define pos
     (case (or target (path->string (system-library-subpath #f)))
       [("x86_64-darwin" "i386-darwin" "aarch64-darwin"
         "x86_64-macosx" "i386-macosx" "aarch64-macosx")
        ;; Mach-O
        (copy-file use-src-file dest-file #t)
        (add-plt-segment dest-file data #:name #"__RKTBOOT")
        ;; Find segment at run time:
        0]
       [("ta6nt" "ti3nt" "win32\\x86_64" "win32\\i386")
        (copy-file use-src-file dest-file #t)
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
          (add-racket-section use-src-file dest-file #".rackboot"
                              (lambda (pos)
                                (values data 'any1 'any2))))
        (cond
          [start-pos
           ;; Success as ELF
           (file-or-directory-permissions dest-file (file-or-directory-permissions use-src-file 'bits))
           (cond
             [expect-elf?
              ;; Find ".rackboot" at run time:
              0]
             [else
              start-pos])]
          [else
           ;; Not ELF; just append to the end
           (copy-file use-src-file dest-file #t)
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

     (define big-endian?
       (if target
           (case target
             [("tppc32le") #t]
             [else #f])
           (system-big-endian?)))

     (file-position o (cdar m))
     (void (write-bytes (integer->integer-bytes pos 4 #t big-endian?) o))
     (let ([pos (+ pos (bytes-length bstr1) terminator-len)])
       (void (write-bytes (integer->integer-bytes pos 4 #t big-endian?) o))
       (let ([pos (+ pos (bytes-length bstr2) terminator-len)])
         (void (write-bytes (integer->integer-bytes pos 4 #t big-endian?) o))
         (let ([pos (+ pos (bytes-length bstr3) terminator-len)])
           (void (write-bytes (integer->integer-bytes pos 4 #t big-endian?) o))))))

   (cond
    [(null? alt-dests)
     (write-offsets dest-file)]
    [else
     (for ([alt (in-list alt-dests)])
	  (copy-file (car alt) (cdr alt) #t)
	  (write-offsets (cdr alt)))])

   (clean-src)))
