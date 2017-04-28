var x1  >= -10000, <= 10000;
var x2  >= -10000, <= 10000;
var x3  >= -10000, <= 10000;
var x4  >= -10000, <= 10000;
var x5  >= -10000, <= 10000;
var x6  >= -10000, <= 10000;
var x7  >= -10000, <= 10000;

param best_val_found := 680.6300574;
param eps := 6.806300574; 		# = max(1, 1% x best_val_found)

subject to f:
      (x1 - 10)^2 + 5 * (x2 - 12)^2 + x3^4 + 3 * (x4 - 11)^2 +      10*x5^6 + 7*x6^2  + x7^4 - 4*x6*x7 - 10*x6 - 8 * x7 <= best_val_found + eps;
subject to
cons1:       127 - 2 * x1^2 - 3 * x2^4 - x3 - 4 * x4^2 - 5 * x5 >= 0;
cons2:       282 - 7 * x1 - 3 * x2 - 10 * x3^2 - x4 + x5 >= 0;
cons3:       196 - 23 * x1 - x2^2 - 6 * x6^2 + 8 * x7 >= 0;
cons4:       -4*x1^2 - x2^2 + 3*x1*x2 - 2*x3^2 - 5*x6 + 11*x7 >= 0;

#solve;
#display x;
