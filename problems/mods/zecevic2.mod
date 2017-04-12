param xinit{1..2};
var x{i in 1..2} := xinit[i];

param best_val_found := -4.125;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	2*x[2]^2-2*x[1]-3*x[2] <= best_val_found + eps;
subject to cons1:
	x[1]+x[2] <= 2.0;
subject to cons2:
	x[1]+4*x[2] <= 4.0;
subject to cons3:
	0 <= x[1] <= 10;
subject to cons4:
	0 <= x[2] <= 10;

data;
param xinit:= 1 0.1 2 -0.1;

 
#solve; 
##display f; 
#display x;
