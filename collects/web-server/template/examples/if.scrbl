#lang web-server/template
@(monkeys monkey-limit monkey-minimum)
@(if (monkeys . > . monkey-limit)
     @t{<p>There are too many monkeys!</p>}
     @t{@(if (monkeys . < . monkey-minimum)
             @t{<p>There aren't enough monkeys!</p>}
             @t{<p>There are just enough monkeys!</p>})})

@; (template 5 10 1)
@;"\n<p>There are just enough monkeys!</p>\n"
@; (template 11 10 1)
@;"\n<p>There are too many monkeys!</p>\n"
@; (template 0 10 1)
@;"\n<p>There aren't enough monkeys!</p>\n"
@;