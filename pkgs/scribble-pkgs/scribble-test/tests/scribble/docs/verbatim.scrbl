#lang scribble/base

@verbatim{One fish.}

@verbatim{
  One fish.
  Two fish.
}

@verbatim[#:indent 3]{
  One fish.
  Two fish.
}

@verbatim[#:indent 3]{
  One fish.
  Two fish.
  @bold{Red} fish.
}

@verbatim[#:indent 3]|{
  One fish.
  Two fish.
  @bold{Red} fish.
}|

@verbatim[#:indent 3]|{
  One fish.
  Two fish.
  |@bold{Red} fish.
}|

@verbatim[@bold{One fish.} "\nTwo fish."]

@verbatim["One fish\n" @bold{Two fish.}]

@verbatim[@bold{One fish.}]

@verbatim["One fish\n" @bold{Two fish.} "\nRed fish."]
