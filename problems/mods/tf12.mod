param M := 100;
param h := 1/M;
var x{1..3};

param best_val_found := 0.6490311083;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	x[1]+0.5*x[2]+x[3]/3 <= best_val_found + eps;
subject to cons1{i in 0..M}:
	-x[1]-i*h*x[2]-(i*h)^2*x[3]+tan(i*h) <= 0;

#solve;
##display f;
#display x;
