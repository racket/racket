#lang scribble/doc
@(require scribble/manual
          setup/dirs)

@(define (rl-link path . content)
   (apply link (apply build-path (find-doc-dir) "release-notes" path)
               content))

@title{Release Notes}

@itemize[#:style "compact"]{

 @item{@rl-link['("drscheme" "HISTORY.txt")]{DrScheme}}
 @item{@rl-link['("mzscheme" "HISTORY.txt")]{MzScheme}
       @itemize[#:style "compact"]{
         @item{@rl-link['("mzscheme" "MzScheme_4.txt")]{Porting from v3xx to v4.x}}
         @item{@rl-link['("mzscheme" "MzScheme_300.txt")]{Porting from v2xx to v3xx}}
         @item{@rl-link['("mzscheme" "MzScheme_200.txt")]{Porting from v1xx to v2xx}}
       }}
 @item{@rl-link['("mred" "HISTORY.txt")]{MrEd}}
 @item{@rl-link['("stepper" "HISTORY.txt")]{Stepper}}

}
