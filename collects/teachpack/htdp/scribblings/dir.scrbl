#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label (except-in scheme/base file-size)
                     teachpack/htdp/dir
                     scheme/contract))

@teachpack["dir"]{Working with Files and Directories}

@declare-exporting[teachpack/htdp/dir]

The teachpack provides structures and operations for working with files and
directories: 

@defstruct[dir ([name symbol?][dirs (listof dir?)][files (listof file?)])]{}

@defstruct[file ([name symbol?][size integer?][content (listof char?)])]{}

@defproc[(create-dir [path symbol?]) dir?]{
 Turns the directory found at @scheme[path] on your computer into an instance of @scheme[dir?].}

Sample: Set teachpack to @filepath{dir.ss} and click RUN:
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
 
Note: Soft links are always treated as if they were empty files.
