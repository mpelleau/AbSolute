param xinit{1..3};
var x{i in 1..3} := xinit[i], >= 0;

param best_val_found := 2;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	x[1]^3 - 6*x[1]^2 + 11*x[1] + x[2] + x[3] <= best_val_found + eps;
subject to cons1:
	4 <= x[1]^2 + x[2]^2 + x[3]^2 <= 10;
subject to cons2:
	x[3] <= 5;

data;
param xinit:= 1 0.1 2 0.1 3 3.0;

#solve;
#display f;
#display x;
