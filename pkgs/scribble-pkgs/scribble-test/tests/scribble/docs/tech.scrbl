#lang scribble/manual

Check case and ``s'' normalization:
Here is @deftech{apple}.
@tech{Apples} are great!
We all like the
@techlink[#:key "APPLE"]{fruit of an apple tree}.

Check case and ``ies'' normalization:
Here is @deftech{cherry}.
@tech{CHERRIES} are great!

Check non-normalization:
Here is @deftech[#:normalize? #f]{egGPlant}.
No one likes @tech[#:normalize? #f]{egGPlant}.
It's the @techlink[#:key "egGPlant" #:normalize? #f]{fruit of an egGPlant plant}.
Here is @deftech[#:normalize? #f]{EGgpLANT},
which is completely different.

Check space and hyphen normalization:
Here is @deftech{granola bar}.
A @tech{granola-bar} breakfast is good enough.
A @tech{granola--bar} combination is close enough.
You can eat a spacey @tech{granola  bar}, too.



