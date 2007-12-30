#lang scribble/doc
@(require "common.ss"
          (for-label file/gif
                     (only-in scheme/gui bitmap%)))

@title{GIF File Writing}

@defmodule[file/gif]{The @schememodname[file/gif] library provides
functions for writing GIF files to a stream, including GIF files with
multiple images and controls (such as animated GIFs).}

This library does not rely on @schememodname[scheme/gui]. See
@scheme[bitmap%] (which is part of @schememodname[scheme/gui]) to read
a GIF file. See also @schememodname[mrlib/gif].

@defproc[(gif-start [out output-port?]
                    [width (integer-in 0 #xFFFFFFFF)]
                    [height (integer-in 0 #xFFFFFFFF)]
                    [bg-color byte?]
                    [colormap (or/c false/c gif-colormap?)])
         gif-stream?]{

Writes the start of a GIF file to the given output port, and returns a
GIF stream that adds to the output port.

The width and height determine a virtual space for the overall GIF
image. Individual images added to the GIF stream must fit within this
virtual space. The space is initialized by the given background color.

The default meaning of color numbers (such as the background color) is
determined by the given colormap, but individual images within the GIF
file can have their own colormaps.

A global colormap need not be supplied, in which case a colormap must
be supplied for each image. Beware that the @scheme[bg-color]'s
meaning is ill-defined if a global colormap is not provided.}

@defproc[(gif-stream? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a GIF stream created by
@scheme[gif-start].

A stream can be in any of several states, some of which are recognized
by more specific predicates:

@itemize{
 
 @item{@scheme[empty-gif-stream?] : no images or controls have been added to the stream}

 @item{@scheme[image-or-control-ready-gif-stream?] : another image or control can be written now}

 @item{@scheme[image-ready-gif-stream?] : another image can be written
               now (since a control was written).}

 @item{done : nothing more can be added}

}}

@defproc[(empty-gif-stream? [v any/c]) boolean?]{

See @scheme[gif-stream?].}


@defproc[(image-or-control-ready-gif-stream? [v any/c]) boolean?]{

See @scheme[gif-stream?].}


@defproc[(image-ready-gif-stream? [v any/c]) boolean?]{

See @scheme[gif-stream?].}


@defproc[(gif-colormap? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a list of vectors where each
vector must contain three bytes---one for red, one for blue, and one
for green---and the list length must be 2, 4, 8, 16, 32, 64, 128, or
256. The colors are indexed (starting from 0) by their order in the
list.}

@defproc[(gif-end [gs image-or-control-ready-gif-stream?]) void?]{

Finishes writing a GIF file. The GIF stream's output port is not
automatically closed.}


@defproc[(gif-add-image [gs image-or-control-ready-gif-stream?]
                        [left (integer-in 0 #xFFFFFFFF)]
                        [top (integer-in 0 #xFFFFFFFF)]
                        [width (integer-in 0 #xFFFFFFFF)]
                        [height (integer-in 0 #xFFFFFFFF)]
                        [interlaced? any/c]
                        [colormap (or/c gif-colormap? false/c)]
                        [bstr bytes?])
         void?]{

Writes an image to the given GIF stream @scheme[gs]. The
@scheme[left], @scheme[top], @scheme[width], and @scheme[height]
values specify the location and size of the image within the overall
GIF image's virtual space.

If @scheme[interlaced?] is true, then @scheme[bytes] should provide
bytes in interlaced order instead of top-to-bottom order. Interlaced
order is:

@itemize{

 @item{every 8th row, starting with 0}
 @item{every 8th row, starting with 4}
 @item{every 4th row, starting with 2}
 @item{every 2nd row, starting with 1}

}

If a global color is provided with @scheme[gif-start], a @scheme[#f]
value can be provided for @scheme[colormap].

The @scheme[bstr] argument specifies the pixel content of the
image. Each byte specifies a color (i.e., an index in the
colormap). Each row is provided left-to-right, and the rows provided
either top-to-bottom or in interlaced order (see above). If the image
is prefixed with a control that specifies an transparent index (see
@scheme[gif-add-control]), then the corresponding ``color'' doesn't
draw into the overall GIF image.

An exception is raised if any byte value in @scheme[bstr] is larger
than the colormap's length, if the @scheme[bstr] length is not
@scheme[width] times @scheme[height], or if the @scheme[top],
@scheme[left], @scheme[width], and @scheme[height] dimensions specify
a region beyond the overall GIF image's virtual space.}


@defproc[(gif-add-control [gs image-or-control-ready-gif-stream?]
                          [disposal (one-of/c 'any 'keep 'restore-bg 'restore-prev)]
                          [wait-for-input? any/c]
                          [delay-csec (integer-in 0 #xFFFFFFFF)]
                          [transparent (or/c byte? false/c)])
         void?]{

Writes an image-control command to a GIF stream. Such a control must
appear just before an image, and it applies to the following image.

The GIF image model involves processing images one by one, placing
each image into the specified position within the overall image's
virtual space. An image-control command can specify a delay before an
image is added (to create animated GIFs), and it also specifies how
the image should be kept or removed from the overall image before
proceeding to the next one (also for GIF animation).

The @scheme[disposal] argument specifies how to proceed:

@itemize{

 @item{@scheme['any] : doesn't matter (perhaps because the next image
       completely overwrites the current one)}

 @item{@scheme['keep] : leave the image in place}

 @item{@scheme['restore-bg] : replace the image with the background
       color}

 @item{@scheme['restore-prev] : restore the overall image content to the
       content before the image is added}

}
       
If @scheme[wait-for-input?] is true, then the display program may wait
for some cue from the user (perhaps a mouse click) before adding the
image.

The @scheme[delay-csec] argument specifies a delay in @math{1/100}s of
a second.

If the @scheme[transparent] argument is a byte, then it determines an
index that is used to represent transparent pixels in the follow image
(as opposed to the color specified by the colormap for the index).}


@defproc[(gif-add-loop-control [gs image-or-control-ready-gif-stream?]
                               [iteration (integer-in 0 #xFFFFFFFF)])
         void?]{

Writes an control command to a GIF stream for which no images or other
commands have already been written. The command causes the animating
sequence of images in the GIF to be repeated @scheme[iteration]
times, where 0 can be used to mean ``infinity.''}


@defproc[(gif-add-comment [gs image-or-control-ready-gif-stream?])
         void?]{

Adds a generic comment to the GIF stream.}


@defproc[(quantize [argb-bstr bytes?])
         (values bytes?
                 gif-colormap?
                 (or/c byte? false/c))]{

Each image in a GIF stream is limited to 256 colors, including the
transparent ``color,'' if any. The @scheme[quantize] function helps
converts a 24-bit image (plus alpha channel) into an indexed-color
image, reducing the number of colors if necessary.

Given a set of pixels expressed in ARGB format (i.e., each four bytes
is a set of values for one pixel: alpha, red, blue, and green),
@scheme[quantize] produces produces

@itemize{

 @item{bytes for the image (i.e., a array of colors, expressed as a byte
       string),}

 @item{a colormap, and}

 @item{either @scheme[#f] or a color index for the transparent ``color.''}

}

The conversion treats alpha values less than 128 as transparent
pixels, and other alpha values as solid.

The quantization process first attempts to use all (non-transparent)
colors in the image. If that fails, it reduces the image to 12-bit
color (3 bits per each of red, green, and blue) by rounding up pixel
values, and tries again. If that fails, it reduces the image to 6-bit
color (2 bits per each of red, green, and blue).

To convert a collection of images all with the same quantization,
append them for the input of a single call of @scheme[quantize], and
then break apart the result bytes.}
