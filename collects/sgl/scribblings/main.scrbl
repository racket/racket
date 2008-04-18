#lang scribble/doc
@(require "common.ss"
          scribble/eval
          (for-syntax scheme/base))

@title[#:tag "main"]{Scheme-Style OpenGL}

@defmodule[sgl]

The functions in @schememodname[sgl] use Scheme style names instead of
C style names.  To convert a C OpenGL name to a Scheme OpenGL name,
change the @tt{gl} prefix to @schemeidfont{gl-}, separate adjacent
words with hyphens, and convert to all lower case.  Functions that
have several variants to accommodate different numbers and types of
arguments are collapsed into one or two functions in
@schememodname[sgl].  For example, @schememodname[sgl] provides two
vertex functions: @scheme[vertex] and @scheme[vertex-v].  The
@scheme[vertex] function accepts 2, 3 or 4 numerical arguments, and
the @scheme[vertex-v] function accepts @scheme[gl-vector]s of length
2, 3 or 4.  The C language OpenGL interface, in contrast, has 24
vertex functions: @tt{glVertex3i}, @tt{glVertex4fv}, etc.

Functions in @schememodname[sgl] take symbols instead of integers for
@tt{GLenum} arguments.  Each function checks that the given symbol is
an acceptable argument and raises an exception if it is not.  Given
the name of a C-language @tt{#define} constant, determine the
corresponding symbolic argument by removing the leading @tt{GL_},
converting the letters to lower-case and replacing each @tt{_} with
@schemeidfont{-}.  For example, @tt{GL_TRIANGLES} becomes
@scheme['triangles], and @tt{GL_TRIANGLE_STRIP} becomes
@scheme['triangle-strip].  Additionally, the functions check the
length of any array arguments to ensure that OpenGL does not attempt
to write or read after the array.

The @schememodname[sgl] module is not as complete as the
@schememodname[sgl/gl] module.

@as-examples[
@schemeblock[
(require sgl sgl/gl-vectors)
(gl-begin 'triangles)
(gl-vertex 1 2 3)
(gl-vertex-v (gl-float-vector 1 2 3 4))
(gl-end)
]]

@defstruct[selection-record ([min-z real?] [max-z real?] [stack ....])]{

Represents a selection.}

@(define-syntax-rule (def-scheme-gl (id ...) body ...)
   @deftogether[(
    @defthing[id procedure?] ...
   ) body ...])

@def-scheme-gl[(
gl-accum
gl-active-texture
gl-alpha-func
gl-begin
gl-begin-query
gl-blend-color
gl-blend-equation
gl-blend-func
gl-blend-func-separate
gl-call-list
gl-check-extension
gl-clear
gl-clear-accum
gl-clear-color
gl-clear-depth
gl-clear-index
gl-clear-stencil
gl-clip-plane
gl-color
gl-color-mask
gl-color-material
gl-color-v
gl-copy-pixels
gl-cull-face
gl-cylinder
gl-delete-lists
gl-delete-queries
gl-depth-func
gl-depth-mask
gl-depth-range
gl-disable
gl-disk
gl-edge-flag
gl-enable
gl-end
gl-end-list
gl-end-query
gl-eval-coord
gl-eval-coord-v
gl-eval-mesh
gl-eval-point
gl-feedback-buffer->gl-float-vector
gl-finish
gl-flush
gl-front-face
gl-frustum
gl-gen-lists
gl-gen-queries
gl-get-error
gl-get-string
gl-hint
gl-index
gl-index-mask
gl-index-v
gl-init-names
gl-is-buffer
gl-is-enabled
gl-is-list
gl-is-query
gl-light
gl-light-model
gl-light-model-v
gl-light-v
gl-line-stipple
gl-line-width
gl-list-base
gl-load-identity
gl-load-matrix
gl-load-name
gl-load-transpose-matrix
gl-look-at
gl-map-grid
gl-material
gl-material-v
gl-matrix-mode
gl-mult-matrix
gl-mult-transpose-matrix
gl-multi-tex-coord
gl-multi-tex-coord-v
gl-new-list
gl-new-quadric
gl-normal
gl-normal-v
gl-ortho
gl-ortho-2d
gl-partial-disk
gl-pass-through
gl-perspective
gl-pick-matrix
gl-pixel-store
gl-point-parameter
gl-point-parameter-v
gl-point-size
gl-polygon-mode
gl-polygon-offset
gl-pop-attrib
gl-pop-client-attrib
gl-pop-matrix
gl-pop-name
gl-process-selection
gl-project
gl-push-matrix
gl-push-name
gl-quadric-draw-style
gl-quadric-normals
gl-quadric-orientation
gl-quadric-texture
gl-raster-pos
gl-raster-pos-v
gl-rect
gl-rect-v
gl-render-mode
gl-rotate
gl-sample-coverage
gl-scale
gl-scissor
gl-secondary-color
gl-secondary-color-v
gl-select-buffer->gl-uint-vector
gl-shade-model
gl-sphere
gl-stencil-func
gl-stencil-mask
gl-stencil-op
gl-tex-coord
gl-tex-coord-v
gl-tex-gen
gl-tex-gen-v
gl-translate
gl-u-get-string
gl-un-project
gl-un-project4
gl-vertex
gl-vertex-v
gl-viewport
gl-window-pos
gl-window-pos-v
)]{

Scheme-style variants of the OpenGL functions.}


@defproc[(process-selection [vec gl-uint-vector?]
                            [hits exact-nonnegative-integer?])
         (listof selection-record?)]{

Parses the contents of @scheme[vec] from the format used by
@scheme[glSelectBuffer].  The second argument should be the number of
hits as returned by @scheme[glRenderMode].}


@defproc[(get-gl-version-number) exact-nonnegative-integer?]{

Returns the run-time OpenGL version number as an integer: @scheme[10],
@scheme[11], @scheme[12], @scheme[13], @scheme[14], @scheme[15], or
@scheme[20].}

@defproc[(get-glu-version-number) exact-nonnegative-integer?]{

Returns the run-time GLU version number as an integer: @scheme[10],
@scheme[11], @scheme[12], or @scheme[13].}
