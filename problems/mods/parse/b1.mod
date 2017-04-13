var
   x  {1 .. 4} >= -10, <= 10;
subject to
cons1:       x[1] + x[2] + x[3] + x[4] = 1;
cons2:       2 * x[3] = -2;
cons3:       2 * x[1] = 1;
cons4:       x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 = 4;

#solve;
#display x;
