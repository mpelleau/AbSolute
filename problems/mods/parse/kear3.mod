
var
   x  {1 .. 4} >= -2, <= 3;
subject to
cons1:       x[1] + 10*x[2] = 0;
cons2:       x[3] - x[4] = 0;
cons3:      (x[2]-2*x[3])^2 = 0;
cons4:      (x[1] - x[4])^2 = 0;

#solve;
#display x;
