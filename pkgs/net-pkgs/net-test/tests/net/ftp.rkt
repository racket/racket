#lang at-exp racket/base

(require net/ftp tests/eli-tester
         racket/tcp racket/port racket/file racket/match)

(define (tcp-serve dest text)
  (define listener (tcp-listen 0))
  (define the-port (let-values ([(_1 p _2 _3) (tcp-addresses listener #t)]) p))
  (define (feed in out)
    (if (input-port? in) (copy-port in out) (display in out))
    (flush-output out))
  (define (feeder)
    (define-values [ip op] (tcp-accept listener))
    (for-each thread-wait
              (list (thread (λ () (feed text op) (close-output-port op)))
                    (thread (λ () (feed ip dest) (close-input-port ip))))))
  (values (thread feeder) the-port))

(define (port->splitstr n)
  (let-values ([(q r) (quotient/remainder n 256)]) (format "~a,~a" q r)))
(define (tcp-serve* dest text)
  (define-values [thd port] (tcp-serve dest text))
  (values thd (port->splitstr port)))

(define ((progress-proc output dir) get-count)
  (thread
   (lambda ()
     (let loop ()
       (define-values [count changed-evt] (get-count))
       (fprintf output "~a bytes ~aloaded\n" count dir)
       (sync changed-evt)
       (loop)))))

(provide tests)
(module+ main (tests))
(define (tests)
  (define cop (open-output-string))
  (define-values [pasv1-thd pasv1-port] (tcp-serve* (current-output-port) DIRLIST))
  (define-values [pasv2-thd pasv2-port] (tcp-serve* (current-output-port) TEXT-FILE))
  (define-values [pasv3-thd pasv3-port] (tcp-serve* (open-output-nowhere) TEXT-FILE))
  (define-values [main-thd main-port] (tcp-serve cop (SERVER-OUTPUT pasv1-port pasv2-port pasv3-port)))
  (define server "127.0.0.1")
  (define port main-port)
  (define user "anonymous")
  (define passwd "nonny")
  (define conn #f)
  (define pth "=README-about-.diff-files")
  (define tmp-dir (make-temporary-file "racket-ftp-test-~a" 'directory))
  (test (port->splitstr 18291) => "71,115"
        (ftp-connection? 1) => #f
        (set! conn (ftp-establish-connection server port user passwd))
        (ftp-connection? conn)
        (when (ftp-connection? conn)
          (define output (open-output-bytes))
          (test (ftp-cd conn "gnu")
                (for ([f (in-list (ftp-directory-list conn))])
                  (match-define (list* type ftp-date name ?size) f)
                  (test (ftp-make-file-seconds ftp-date)))
                (ftp-download-file conn tmp-dir pth
                                   #:progress (progress-proc output "down"))
                ;; Note: It would be nice to test the output but there is no
                ;; easy way that I see to wait for the progress thread to
                ;; finish (it stays alive after the transfer), and it's
                ;; probably a bad idea to make a test that expects a specific
                ;; output
                ;; (get-output-bytes output #t) => #"0 bytes downloaded\n"
                ;; (get-output-bytes output #t) => #"744 bytes uploaded\n"
                (ftp-upload-file conn (path->string (build-path tmp-dir pth))
                                 #:progress (progress-proc output "up"))
                ;; (get-output-bytes output #t) => #"744 bytes uploaded\n"
                (ftp-delete-file conn "3dldf/test.file")
                (ftp-make-directory conn "test")
                (ftp-delete-directory conn "test")
                (ftp-rename-file conn "test1" "test2")
                (ftp-close-connection conn)
                (delete-file (build-path tmp-dir pth))
                (delete-directory/files tmp-dir)
                (thread-wait pasv1-thd)
                (thread-wait pasv2-thd)
                (thread-wait pasv3-thd)
                (thread-wait main-thd)
                (get-output-string cop) => EXPECTED-USER-OUTPUT
                ))))

(define S string-append)

(define DIRLIST @S{
  drwxr-xr-x    2 1003     1003         4096 Jan 16  2004 3dldf
  -rw-r--r--    1 1003     65534        1492 Jan 25  2001 =README
  -rw-r--r--    1 1003     65534         745 Mar 20  1997 =README-about-.diff-files
  -rw-r--r--    1 1003     65534        1042 Jan 08  2000 =README-about-.gz-files
  drwxrwxr-x    3 0        1003         4096 Feb 08  2005 GNUinfo
  drwxrwsr-x    3 0        1003         4096 Aug 14  2003 GNUsBulletins
  drwxrwxr-x    2 0        1003         4096 Mar 25 18:42 Licenses
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 MailingListArchives
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 MicrosPorts
  -rw-r--r--    1 1003     65534       29107 May 03  1998 ProgramIndex
  -rw-r--r--    1 1003     65534         257 Jun 12  2000 README.DESCRIPTIONS
  drwxrwxr-x    2 0        1003         4096 Dec 29  2007 a2ps
  drwxrwxr-x    2 0        1003         4096 Feb 12 16:45 acct
  drwxrwxr-x    2 0        1003         4096 Jun 09  2006 adns
  drwxr-xr-x    2 1003     1003         4096 Jul 14  2008 aeneas
  drwxrwxr-x    2 0        1003         4096 Dec 20  2008 anubis
  drwxr-xr-x    2 1003     1003         4096 Feb 24 00:20 archimedes
  drwxrwxr-x    4 0        1003         4096 Apr 16  2008 aspell
  lrwxr-xr-x    1 0        0              15 Mar 11  2005 aspell-dict-csb -> aspell/dict/csb
  lrwxrwxrwx    1 0        0              14 Nov 24  2003 aspell-dict-ga -> aspell/dict/ga
  lrwxrwxrwx    1 0        0              14 Mar 22  2004 aspell-dict-hr -> aspell/dict/hr
  lrwxrwxrwx    1 0        0              14 Mar 12  2004 aspell-dict-is -> aspell/dict/is
  lrwxrwxrwx    1 0        0              14 Nov 24  2003 aspell-dict-it -> aspell/dict/it
  lrwxrwxrwx    1 0        0              14 Apr 26  2004 aspell-dict-sk -> aspell/dict/sk
  drwxrwxr-x   13 0        1003         8192 Jul 03 14:05 auctex
  drwxrwxr-x    2 0        1003         4096 Aug 02 18:10 autoconf
  drwxr-xr-x    2 1003     1003         4096 Jul 06 14:30 autoconf-archive
  drwxrwxr-x   32 0        1003         4096 Jul 25 18:10 autogen
  drwxrwxr-x    2 0        1003         8192 Dec 08  2009 automake
  drwxrwxr-x    2 0        1003         4096 Aug 26  2007 avl
  drwxr-xr-x    2 1003     1003         4096 Jul 15  2009 ballandpaddle
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 barcode
  drwxrwxr-x    8 0        1003         4096 Jan 18  2010 bash
  drwxrwxr-x    3 0        1003         8192 Jan 18  2007 bayonne
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 bc
  drwxrwxr-x    2 0        1003         4096 Mar 03 15:25 binutils
  drwxrwxr-x    2 0        1003         4096 Aug 06 02:25 bison
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 bool
  drwxr-xr-x   10 1003     1003         4096 Jul 30  2007 bpel2owfn
  -rw-r--r--    1 1003     65534         420 Nov 15  2000 brl.README
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 calc
  drwxrwxr-x    2 0        1003         4096 Dec 16  2008 ccaudio
  drwxrwxr-x    2 0        1003         4096 Oct 25  2009 ccrtp
  drwxrwxr-x    2 0        1003         8192 May 19 00:50 ccscript
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 cfengine
  drwxr-xr-x    2 1003     1003         4096 Jul 11  2009 cflow
  drwxrwxr-x    2 0        1003         4096 Nov 14  2009 cgicc
  drwxrwxr-x    2 0        1003         4096 Jan 12  2004 chess
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 cim
  drwxrwxr-x    2 0        1003         4096 Mar 25 17:25 classpath
  drwxrwxr-x    2 0        1003         4096 Apr 28  2007 classpathx
  drwxrwxr-x    6 0        1003         4096 Jul 07 17:30 clisp
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 clx
  drwxr-xr-x    2 1003     1003         4096 Jun 05  2004 combine
  lrwxrwxrwx    1 0        0               9 Nov 18  2003 commonc++ -> commoncpp
  drwxrwxr-x    2 0        1003         8192 Aug 11 06:30 commoncpp
  drwxrwxr-x    2 0        1003         4096 Feb 13  2008 config
  drwxrwxr-x    2 0        1003         8192 Apr 23 16:45 coreutils
  drwxrwxr-x    2 0        1003         4096 Mar 10 13:20 cpio
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 cpp2html
  drwxr-xr-x    2 1003     1003         4096 Mar 18 06:45 cppi
  drwxr-xr-x    2 1003     1003         4096 Apr 11  2009 cssc
  drwxrwxr-x    2 0        1003         4096 Feb 21  2008 dap
  -rw-r--r--    1 1003     65534         110 Jun 06  1999 dc.README
  drwxrwxr-x    2 0        1003         4096 Feb 11  2009 ddd
  drwxr-xr-x    2 1003     1003         4096 Apr 06 18:50 ddrescue
  drwxrwxr-x    2 0        1003         4096 Jan 30  2004 dejagnu
  drwxr-xr-x    2 1003     1003         4096 Jul 07 20:50 denemo
  -rw-r--r--    1 1003     65534         145 May 22  2001 dia.README
  drwxr-xr-x    2 1003     1003         4096 Jul 07 19:35 dico
  drwxrwxr-x    2 0        1003         4096 Sep 17  2007 diction
  -rw-r--r--    1 1003     65534         134 Apr 15  2002 dictionary.README
  drwxrwxr-x    2 0        1003         4096 May 03 17:00 diffutils
  drwxr-xr-x    2 1003     1003         4096 Apr 11 11:55 dionysus
  drwxrwxr-x    2 0        0            4096 Apr 03  2007 dismal
  -rw-r--r--    1 1003     65534         492 Apr 03  2007 djgpp.README
  drwxr-xr-x    2 1003     1003         4096 Feb 18  2005 dominion
  drwxrwxr-x    5 0        1003         4096 Dec 10  2008 dotgnu
  -rw-r--r--    1 1003     65534          96 Feb 09  1999 dumb.README
  drwxrwxr-x    2 0        1003         4096 Jul 10  2009 ed
  drwxrwxr-x    2 0        1003         4096 Apr 08 18:05 edma
  drwxrwxr-x    2 0        1003         4096 Feb 17 00:20 electric
  -rw-r--r--    1 1003     65534         835 Jan 24  1999 elisp-archive.README
  drwxrwxr-x    3 0        1003         4096 May 08 04:01 emacs
  drwxr-xr-x    2 1003     1003         4096 Sep 16  2008 emms
  drwxrwxr-x    2 0        1003         4096 Jun 01 23:25 enscript
  drwxr-xr-x    2 1003     1003         4096 Jan 26  2008 erc
  drwxr-xr-x    2 1003     1003         4096 Jan 10  2010 fdisk
  drwxr-xr-x    2 1003     1003         4096 Nov 16  2008 ferret
  drwxrwxr-x    2 0        1003         4096 Jun 06  2009 findutils
  drwxrwxr-x    2 0        0            4096 Mar 20  2007 flex
  drwxrwxr-x    2 0        1003         4096 Aug 02  2003 fontutils
  drwxr-xr-x    2 1003     1003         4096 Apr 20 21:05 freedink
  drwxrwxr-x    2 0        1003         4096 Jan 04  2009 freefont
  @||})

(define TEXT-FILE @S{
  1. Sometimes diffs between two versions were either too large to be
  worth making, or too difficult.  In those cases where a .diff file is
  missing, please just FTP the latest version.

  2. The .diff file suffix signifies a patch file produced by the GNU
  'diff' program.

  A diff file like this has all of the changes from one version of a
  program to the next (i.e. gcc-2.6.2-2.6.3.diff will take gcc-2.6.2 and
  produce gcc 2.6.3).

  You can use the "patch" program to apply the diff to your sources.
  (The "patch" program is available on prep.ai.mit.edu in directory
  /pub/gnu/ if it isn't already installed on your system.)

  (You might also want to take a look at the diff file, the format is
  pretty obvious and could be educational. :-)

  Thank You!
  @||})

(define (SERVER-OUTPUT pasv1-port pasv2-port pasv3-port)
  @S{220 GNU FTP server ready.
     230-Due to U.S. Export Regulations, all cryptographic software on this
     230-site is subject to the following legal notice:
     230-
     230-    This site includes publicly available encryption source code
     230-    which, together with object code resulting from the compiling of
     230-    publicly available source code, may be exported from the United
     230-    States under License Exception "TSU" pursuant to 15 C.F.R. Section
     230-    740.13(e).
     230-
     230-This legal notice applies to cryptographic software only. Please see
     230-the Bureau of Industry and Security (www.bxa.doc.gov) for more
     230-information about current U.S. regulations.
     230 Login successful.
     250-If you have problems downloading and are seeing "Access denied" or
     250-"Permission denied", please make sure that you started your FTP client
     250-in a directory to which you have write permission.
     250-
     250-Please note that all files ending in `.gz' are compressed with `gzip',
     250-not with the unix `compress' program.  Get the file below for more
     250-info.
     250-
     250-For a list of mirrors and other ways of getting GNU software, FTP the
     250-file /pub/gnu/GNUinfo/FTP from ftp.gnu.org or one of its mirror sites.
     250-
     250-Programs that are directly in this directory are actually GNU
     250-programs, developed under the auspices of GNU.
     250-
     250-We do, however, distribute some non-GNU programs through our FTP
     250-server, or provide pointers to where they are.  We put these
     250-programs/pointers in the subdirectory non-gnu since they are not
     250-developed by the GNU project.  They are, of course, part of the GNU
     250-system. See:
     250-http://www.gnu.org/philosophy/categories.html#TheGNUsystem
     250 Directory successfully changed.
     227 Entering Passive Mode (127,0,0,1,@pasv1-port)
     200 Switching to Binary mode.
     150 Here comes the directory listing.
     226 Directory send OK.
     227 Entering Passive Mode (127,0,0,1,@pasv2-port)
     200 Switching to Binary mode.
     150 Opening BINARY mode data connection for =README-about-.diff-files (745 bytes).
     226 File send OK.
     227 Entering Passive Mode (127,0,0,1,@pasv3-port)
     200 Switching to Binary mode.
     150 Opening BINARY mode data connection for =README-about-.diff-files (745 bytes).
     226 File send OK.
     250 Delete operation successful.
     257 test created
     250 Remove directory operation successful.
     350 Ready for RNTO.
     250 Rename successful.
     221 Goodbye.
     @||})

(define EXPECTED-USER-OUTPUT
  @(lambda xs (regexp-replace* #rx"\n" (apply S xs) "\r\n")){
     USER anonymous
     CWD gnu
     PASV
     TYPE I
     LIST
     PASV
     TYPE I
     RETR =README-about-.diff-files
     PASV
     TYPE I
     STOR =README-about-.diff-files
     DELE 3dldf/test.file
     MKD test
     RMD test
     RNFR test1
     RNTO test2
     QUIT
     @||})

(module+ test (require (submod ".." main))) ; for raco test & drdr
