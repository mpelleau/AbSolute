param xinit{1 .. 2};
var x{i in 1 .. 2} := xinit[i];

param best_val_found := -18.2;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	(-56*x[1]-256*x[2]+991+16*x[1]^2+16*x[2]^2-8*x[1]*x[2])/15 <= best_val_found + eps;

data;
param xinit:= 1 3.0 2 8.0;


#solve;
##display f;
#display x;
