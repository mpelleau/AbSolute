#   Optimal Reactor Design 


var   x1 >= 6, <= 8;
var   x2 >= 2, <= 8; 
var  x3 >= 0.1, <= 8;
var   x4 >= 0.1, <= 8;
var   x5 >= 5, <= 8;
var   x6 >= 5, <= 8;
var   x7 >= 1, <= 8;
var   x8 >= 0.1, <= 8;

param best_val_found := 3.95116344;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f: 
      0.4 * x1^0.67 * x7^(-0.67) + 0.4 * x2^0.67 * x8^(-0.67) + 10 - x1 - x2 <= best_val_found + eps;

subject to
cons1:       0.4 * x1^0.67 * x7^(-0.67) + 0.4 * x2^0.67 * x8^(-0.67) + 10 - x1 - x2 <= 4.2;
cons2:       1 - 0.0588 * x5 * x7 - 0.1 * x1 >= 0;
cons3:       1 - 0.0588 * x6 * x8 - 0.1 * x1 - 0.1 * x2 >= 0;
cons4:       1 - 4 * x3/x5 - 2 * x3^(-0.71) * x5^(-1) - 0.0588 * x3^(-1.3) * x7 >= 0;
cons5:       1 - 4 * x4/x6 - 2 * x4^(-0.71) * x6^(-1) - 0.0588 * x4^(-1.3) * x8 >= 0;

#solve;
#display x1, x2, x3, x4, x5, x6, x7, x8;
