#lang scribble/doc
@(require scribble/manual
          (for-label lang/htdp-beginner))

@title{Test Box Recovery Tool}

The text-box recovery tool allows DrRacket or DrScheme v370 and later to read
programs created using v360 and earlier that include test-case boxes.

When opened using this tool, test-case boxes are turned into
@scheme[check-expect] forms.

Test boxes plain-text tests and expected results are converted to
plain-text @scheme[check-expect] forms. 

If either the test or expected-result expression contains non-text
(e.g., an image), the converted form is a comment box containing a
@scheme[check-expect] form. The box should be easy to remove using the
@menuitem["Scheme" "Uncomment"] menu item in DrRacket.
