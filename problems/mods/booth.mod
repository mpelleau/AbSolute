var x{1..2};

param best_val_found := 1.5925875e-23;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	(x[1]+2*x[2]-7)^2 + (2*x[1]+x[2]-5)^2 <= best_val_found + eps;


#solve;
##display f;
#display x;
