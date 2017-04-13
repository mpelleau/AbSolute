
var
   x  {1 .. 2} >= -1e10, <= 1e10;
subject to
 circle: x[1]^2 + x[2]^2 = 1;
 parabola: x[1]^2 - x[2] = 0;
 

#solve;
#display x;
