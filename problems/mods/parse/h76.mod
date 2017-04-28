var x1  >= 0, <= 10000;
var x2  >= 0, <= 10000;
var x3  >= 0, <= 10000;
var x4  >= 0, <= 10000;


param best_val_found := -4.681818182;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
        x1^2 + 0.5 * x2^2 + x3^2 + 0.5*x4^2 - x1*x3 +        x3*x4 - x1 - 3*x2 + x3 - x4 <= best_val_found + eps;
subject to
cons2:         5 - x1 - 2*x2 - x3 - x4 >= 0;
cons3:         4 - 3*x1 - x2 - 2*x3 + x4 >= 0;
cons4:         x2 + 4*x3 - 1.5 >= 0;
