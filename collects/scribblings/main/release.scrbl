#lang scribble/doc
@(require scribble/manual
          setup/dirs
          "private/utils.ss")

@(define (rl-link path . content)
   (apply link (apply build-path (find-doc-dir) "release-notes" path)
               content))
@(define (mzport doc from to)
   (rl-link (list "mzscheme" doc) (format "Porting from ~a to ~a" from to)))

@main-page['release]

@itemize[#:style "compact"]{

 @item{@rl-link['("drscheme" "HISTORY.txt")]{DrScheme}}
 @item{@rl-link['("mzscheme" "HISTORY.txt")]{MzScheme}
       @itemize[#:style "compact"]{
         @item{@mzport["MzScheme_4.txt" "v3xx" "v4.x"]}
         @item{@mzport["MzScheme_300.txt" "v2xx" "v3xx"]}
         @item{@mzport["MzScheme_200.txt" "v1xx" "v2xx"]}}}
 @item{@rl-link['("mred" "HISTORY.txt")]{MrEd}}
 @item{@rl-link['("stepper" "HISTORY.txt")]{Stepper}}

}
