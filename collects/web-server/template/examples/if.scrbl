#lang web-server/template
@(if ($monkeys . > . $monkey-limit)
     @t{<p>There are too many monkeys!</p>}
     @t{@(if ($monkeys . < . $monkey-minimum)
             @t{<p>There aren't enough monkeys!</p>}
             @t{<p>There are just enough monkeys!</p>})})

@;{
> (template #:monkeys 5
           #:monkey-limit 10
           #:monkey-minimum 1)
"<p>There are just enough monkeys!</p>\n\n"
> (template #:monkeys 0
           #:monkey-limit 10
           #:monkey-minimum 1)
"<p>There aren't enough monkeys!</p>\n\n"
> (template #:monkeys 11
           #:monkey-limit 10
           #:monkey-minimum 1)
"<p>There are too many monkeys!</p>\n\n"
> 
}