param xinit{1 .. 2};
var x{i in 1 .. 2} := xinit[i];
var u := 7.5;

param best_val_found := -4.770489559e-18;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	u <= best_val_found + eps;
subject to cons1:
	u-0.5*x[1]-x[2]^2-5*x[1]/(x[1]+0.1) >= 0;
subject to cons2:
	u+0.5*x[1]-x[2]^2-5*x[1]/(x[1]+0.1) >= 0;
subject to cons3:
	u+0.5*x[1]+x[2]^2+5*x[1]/(x[1]+0.1) >= 0;

data;
param xinit:= 1 3.0 2 1.0;

#solve;/* #display f;*/ display x,u;
