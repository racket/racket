#lang scribble/doc

@(require scribble/manual
          (for-label (except-in scheme/base file-size)
	  	     teachpack/htdp/dir))

@title[#:tag "dir"]{Working with Files and Directories: dir.ss}

@declare-exporting[teachpack/htdp/dir]

The teachpack provides structures and operations for working with files and
directories: 

@defstruct[dir ([name string?][dirs (list-of dir?)][files (list-of file?)])]{}

@defstruct[file ([name string?][content (list-of char?)])]{}

@defproc[(create-dir [path string?]) dir?]{
 Turns the directory found at @scheme[path] on your computer into an instance of @scheme[dir?].}

Sample: Set teachpack to <code>dir.ss</code> and click RUN:
@(begin
#reader scribble/comment-reader
(schemeblock
> (create-dir ".")
(make-dir
  '|.|
  empty
  (cons (make-file 'ball1.gif 1289 empty)
        (cons (make-file 'blueball.gif 205 empty)
              (cons (make-file 'greenbal.gif 204 empty)
                    (cons (make-file 'redball.gif 203 empty)
                          (cons (make-file 'ufo.gif 1044 empty)
                                (cons (make-file 'gif-test.ss 5811 empty)
                                      empty)))))))
))
Using ``.'' usually means the directory in which your program is
located. In this case, the directory contains no sub-directories and six
files. 
 
Note: Softlinks are always treated as if they were empty files.
