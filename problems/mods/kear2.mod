var
   x  {1..2} >= -2, <= 3 := 3;
subject to
cons1:         x[1] + x[2] = 0;
cons2:         4*(x[1]+x[2]) + (x[1] - x[2])*((x[1]-2)^2 + x[2]^2 - 1) = 0;

#solve;
#display x;
