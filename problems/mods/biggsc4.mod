var x{1..4} >= 0.0, <= 5.0;

param best_val_found := -24.375;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	(-x[1]*x[3]-x[2]*x[4]) <= best_val_found + eps;
subject to cons1:
	0 <= x[1]+x[2] -2.5 <= 5.0;
subject to cons2:
	0 <= x[1]+x[3] -2.5 <= 5.0;
subject to cons3:
	0 <= x[1]+x[4] -2.5 <= 5.0;
subject to cons4:
	0 <= x[2]+x[3] -2.0 <= 5.0;
subject to cons5:
	0 <= x[2]+x[4] -2.0 <= 5.0;
subject to cons6:
	0 <= x[3]+x[4] -1.5 <= 5.0;
subject to cons7:
	x[1]+x[2]+x[3]+x[4]-5.0 >= 0;

#solve; 
#display f; 
#display x;
