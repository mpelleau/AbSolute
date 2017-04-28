#   Heat Exchanger Design

var x1  >= 100, <= 10000;
var x2  >= 1000, <= 10000;
var x3  >= 1000, <= 10000;
var x4  >= 10, <= 1000;
var x5  >= 10, <= 1000;
var x6  >= 10, <= 1000;
var x7  >= 10, <= 1000;
var x8  >= 10, <= 1000;

param best_val_found := 7049.248021;
param eps := 70.49248021; 		# = max(1, 1% x best_val_found)

subject to f:
      x1 + x2 + x3 <= best_val_found + eps;

subject to
cons7:       0.0025 * (x4 + x6) <= 1;
cons8:       0.0025 * (-x4 + x5 + x7) <= 1;
cons9:       0.01 * ( - x5 + x8 ) <= 1;
cons10:       100 * x1 - x1 * x6 + 833.33252 * x4 - 83333.333 <= 0;
cons11:       x2 * x4 - x2 * x7 - 1250 * x4 + 1250 * x5 <= 0;
cons12:       x3 * x5 - x3 * x8 - 2500 * x5 + 1250000 <= 0;

#solve;
#display x;
