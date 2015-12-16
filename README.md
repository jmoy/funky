My attempt to learn about the implementation of 
functional languages.

Right now an interpreter of a simple pure language
with (recursive) let and lambda. There are only two 
data types: doubles and closures (not even cons cells).
There is only one primitive function: `add` which
adds all its arguments if they are doubles.

Sample program:

~~~~~~
;Test 
(let 
  (
    (f (lambda (x) (add x a))) 
    (a 2)
  )
  (f 4)
) 
~~~~~~
