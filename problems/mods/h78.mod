var   x1 >= -4, <= -1e-8;
var   x2 >= 1e-8, <= 4;
var   x3 >= 1e-8, <= 4;
var   x4 >= -4, <= -1e-8;
var   x5 >= -4, <= -1e-8;

minimize f:
      x1 * x2 * x3 * x4 * x5;
subject to
cons1:       x1^2 + x2^2 + x3^2 + x4^2 + x5^2 = 10;
cons2:       x2*x3 = 5*x4*x5;
cons3:       x1^3 + x2^3 + 1 = 0;


#solve;
#display x1, x2, x3, x4, x5;
