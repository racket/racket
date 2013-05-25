#lang scribble/doc
@(require scribble/manual
          scribble/extract
          (for-label file/gif))

@title[#:tag "gif"]{GIF File Writing}

@defmodule[file/gif]

The @racketmodname[file/gif] library provides functions for
writing GIF files to a stream, including GIF files with multiple
images and controls (such as animated GIFs).

A GIF stream is created by @racket[gif-start], and then individual
images are written with @racket[gif-add-image]. Optionally,
@racket[gif-add-control] inserts instructions for rendering the
images. The @racket[gif-end] function ends the GIF stream.

A GIF stream can be in any one of the following states:

@itemize[

 @item{@racket['init] : no images or controls have been added to the
       stream}

 @item{@racket['image-or-control] : another image or control can be
       written}

 @item{@racket['image] : another image can be written (but not a
       control, since a control was written)}

 @item{@racket['done] : nothing more can be added}

]


@defproc[(gif-stream? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a GIF stream created by
 @racket[gif-write], @racket[#f] otherwise.
}
 
@defproc[(image-ready-gif-stream? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a GIF stream that is not in
 @racket['done] mode, @racket[#f] otherwise.}

@defproc[(image-or-control-ready-gif-stream? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a GIF stream that is in
 @racket['init] or @racket['image-or-control] mode, @racket[#f]
 otherwise.}

@defproc[(empty-gif-stream? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a GIF stream that in
 @racket['init] mode, @racket[#f] otherwise.}

@defproc[(gif-colormap? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] represets a colormap,
  @racket[#f] otherwise.  A colormap is a list whose size is a power
  of @math{2} between @math{2^1} and @math{2^8}, and whose elements
  are vectors of size 3 containing colors (i.e., exact integers
  between @math{0} and @math{255} inclusive).}

@defproc[(color? [v any/c]) boolean?]{
  The same as @racket[byte?].}

@defproc[(dimension? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is an exact integer between
 @racket[#x0] and @racket[#xFFFF] inclusive, @racket[#f]
 otherwise.}

@defproc[(gif-state [stream gif-stream?]) symbol?]{
 Returns the state of @racket[stream].}

@defproc[(gif-start [out output-port?]
                    [w dimension?]
                    [h dimension?]
                    [bg-color color?]
                    [cmap (or/c gif-colormap? #f)])
         gif-stream?]{

 Writes the start of a GIF file to the given output port, and
 returns a GIF stream that adds to the output port.

 The width and height determine a virtual space for the overall
 GIF image.  Individual images added to the GIF stream must fit
 within this virtual space.  The space is initialized by the
 given background color.

 Finally, the default meaning of color numbers (such as the
 background color) is determined by the given colormap, but
 individual images within the GIF file can have their own
 colormaps.

 A global colormap need not be supplied, in which case a
 colormap must be supplied for each image. Beware that
 @racket[bg-color] is ill-defined if a global colormap is not
 provided.}

@defproc[(gif-add-image [stream image-ready-gif-stream?]
                        [left dimension?]
                        [top dimension?]
                        [width dimension?]
                        [height dimension?]
                        [interlaced? any/c]
                        [cmap (or/c gif-colormap? #f)]
                        [bstr bytes?])
           void?]{

 Writes an image to the given GIF stream. The @racket[left],
 @racket[top], @racket[width], and @racket[height] values
 specify the location and size of the image within the overall
 GIF image's virtual space.

 If @racket[interlaced?] is true, then @racket[bstr] should
 provide bytes ininterlaced order instead of top-to-bottom
 order. Interlaced order is:

 @(itemize @item{every 8th row, starting with 0}
           @item{every 8th row, starting with 4}
           @item{every 4th row, starting with 2}
           @item{every 2nd row, starting with 1})

 If a global color is provided with @racket[gif-start], a
 @racket[#f] value can be provided for @racket[cmap].

 The @racket[bstr] argument specifies the pixel content of the
 image.  Each byte specifies a color (i.e., an index in the
 colormap).  Each row is provided left-to-right, and the rows
 provided either top-to-bottom or in interlaced order (see
 above).  If the image is prefixed with a control that specifies
 an transparent index (see @racket[gif-add-control]), then the
 corresponding ``color'' doesn't draw into the overall GIF
 image.

 An exception is raised if any byte value in @racket[bstr] is
 larger than the colormap's length, if the @racket[bstr] length
 is not @racket[width] times @racket[height], or if the
 @racket[top], @racket[left], @racket[width], and
 @racket[height] dimensions specify a region beyond the overall
 GIF image's virtual space.}

@defproc[(gif-add-control [stream image-or-control-ready-gif-stream?]
                          [disposal (or/c 'any 'keep 'restore-bg 'restore-prev)]
                          [wait-for-input? any/c]
                          [delay dimension?]
                          [transparent (or/c color? #f)])
         void?]{
 Writes an image-control command to a GIF stream. Such a control
 must appear just before an image, and it applies to the
 following image.

 The GIF image model involves processing images one by one,
 placing each image into the specified position within the
 overall image's virtual space. An image-control command can
 specify a delay before an image is added (to create animated
 GIFs), and it also specifies how the image should be kept or
 removed from the overall image before proceeding to the next
 one (also for GIF animation).

 The @racket[disposal] argument specifies how to proceed:

 @(itemize @item{@racket['any] : doesn't matter (perhaps because
                 the next image completely overwrites the
                 current one)}
           @item{@racket['keep] : leave the image in place}
           @item{@racket['restore-bg] : replace the image with
                 the background color}
           @item{@racket['restore-prev] : restore the overall
                 image content to the content before the image
                 is added})

 If @racket[wait-for-input?] is true, then the display program
 may wait for some cue from the user (perhaps a mouse click)
 before adding the image.

 The @racket[delay] argument specifies a delay in 1/100s of a
 second.

 If the @racket[transparent] argument is a color, then it
 determines an index that is used to represent transparent
 pixels in the follow image (as opposed to the color specified
 by the colormap for the index).

 An exception is raised if a control is already added to
 @racket[stream] without a corresponding image.}

@defproc[(gif-add-loop-control [stream empty-gif-stream?]
                               [iteration  dimension?])
         void?]{
 Writes a control command to a GIF stream for which no images or
 other commands have already been written. The command causes
 the animating sequence of images in the GIF to be repeated
 `iteration-dimension' times, where 0 can be used to mean
 ``infinity.''

 An exception is raise if some control or image has been added
 to the stream already.}

@defproc[(gif-add-comment [stream image-or-control-ready-gif-stream?]
                          [bstr bytes?])
         void?]{
 Adds a generic comment to the GIF stream.

 An exception is raised if an image-control command was just
 written to the stream (so that an image is required next).}

@defproc[(gif-end [stream image-or-control-ready-gif-stream?])
         void?]{
 Finishes writing a GIF file. The GIF stream's output port is
 not automatically closed.

 An exception is raised if an image-control command was just
 written to the stream (so that an image is required next).}

@defproc[(quantize [bstr (and/c bytes?
                                (lambda (bstr)
                                  (zero? (remainder (bytes-length bstr) 4))))])
         (values bytes? gif-colormap? (or/c color? #f))]{
 Each image in a GIF stream is limited to 256 colors, including the
 transparent ``color,'' if any. The @racket[quantize] function
 converts a 24-bit image (plus alpha channel) into an
 indexed-color image, reducing the number of colors if necessary.
    
 Given a set of pixels expressed in ARGB format (i.e., each four
 bytes is a set of values for one pixel: alpha, red, blue, and
 green), @racket[quantize] produces produces
    
  @(itemize @item{bytes for the image (i.e., a array of colors,
                  expressed as a byte string)}
            @item{a colormap}
            @item{either @racket[#f] or a color index for the
                         transparent ``color''})
    
 The conversion treats alpha values less than 128 as transparent
 pixels, and other alpha values as solid.
    
 The quantization process uses Octrees @cite["Gervautz1990"] to construct an adaptive
 palette for all (non-transparent) colors in the image.  This implementation is
 based on an article by Dean Clark @cite["Clark1996"]. 
    
 To convert a collection of images all with the same quantization,
 simply append them for the input of a single call of
 @racket[quantize], and then break apart the result bytes.}
