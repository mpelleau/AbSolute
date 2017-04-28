param a := 0.48;
param c1 := 2/3*1e-6;

var x1  >= 0, <= 1200;
var x2  >= 0, <= 1200;
var x3  >= -a, <= a;
var x4  >= -a, <= a;

param best_val_found := 5174.412695;
param eps := 51.74412695; 		# = max(1, 1% x best_val_found)

subject to f:
        x1 * (3 + 1e-6*x1^2) + x2 * (2 + c1*x2^2) <= best_val_found + eps;
subject to
cons5:         x4 - x3 + a >= 0;
cons6:         x3 - x4 + a >= 0;
cons7:         1000 * sin(-x3 - 0.25) + 1000 * sin(-x4-0.25) + 894.8 - x1 = 0;
cons8:         1000 * sin(x3 - 0.25) + 1000 * sin(x3-x4-0.25) + 894.8 - x2 = 0;
cons9:         1000 * sin(x4 - 0.25) + 1000 * sin(x4-x3-0.25) + 1294.8 = 0;



#solve;
#display x;
