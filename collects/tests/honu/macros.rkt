#lang honu

// display(1);

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

/*

// display(syntax ...);

macro (to2){{fuz
            x ... to2
               }}
{{
  display(x);
         ...
  }}

fuz 5 6 to2

// macro (to = do end) {{ for looper:id = first:expr to last:expr do

                           /*
macro (to = do end) {{ for looper = first to last do
  body ...
}}
{{
  /* display(2); */
  var x = function(looper){
    body ...
    if (looper < last){
      x(looper+1);
    }
  };
  x(first);
}}
                           

/*
for2 x = 1 to 10 do
  display(x);
  newline();
end
*/

for x = 1 to 10 do
  display(x);
  newline();

                           */
*/
