#lang scribble/text

@define[name]{PLT Scheme}

Suggested price list for "@name"

@; test mutual recursion, throwing away inter-definition spaces
@; <-- this is needed to get one line of space only
@(define (items-num)
   (length items))

@(define average
   (delay (/ (apply + (map car items)) (length items))))

@(define items
   (list @list[99]{Home}
         @list[149]{Professional}
         @list[349]{Enterprize}))

@(for/list ([i items] [n (in-naturals)])
   @list{@|n|. @name @cadr[i] edition: $@car[i].99
         @||})@; <-- also needed

Total: @items-num items
Average price: $@|average|.99
