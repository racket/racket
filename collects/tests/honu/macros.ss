#lang honu

/*
=> 

x = function(q){
  print q;
  if (q < end){
    x(q+1);
  }
}
x(start);
*/

macro (to do end) {{ for looper:id = first:expr to last:expr do
  body
end
}}
{{
  /* display(2); */
  var x = function(looper){
    display(body); newline();
    if (looper < last){
      x(looper+1);
    }
  };
  x(first);
}}

for x = 1 to 10 do
  x
end
