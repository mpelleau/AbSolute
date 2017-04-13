var x{1 .. 3} := 50.0;

param best_val_found := 5;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	-(6*x[2]+3*x[1]+2*x[3]-11)/(x[1]+4*x[2]+x[3]+1) <= best_val_found + eps;
subject to cons1:
	3*x[1]+4*x[2]+x[3]-2 <= 0;
subject to cons2:
	x[1]+4*x[2]+x[3]-1 <= 0; 
subject to cons3:
	-(6*x[2]+3*x[1]+2*x[3]-11)/(x[1]+4*x[2]+x[3]+1) >= 5.0;

#solve;
#display f;
#display x;
