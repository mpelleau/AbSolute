var x{1..4};

param best_val_found := 5.74438491;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	x[3]-1 +
	x[1]^2+
	x[2]^2 + (x[3]+x[4])^2 +
	sin(x[3])^2 + x[1]^2*x[2]^2 + x[4]-3 +
	sin(x[3])^2 +
	(x[4]-1)^2 +
	(x[2]^2)^2+
	(x[3]^2 + (x[4]+x[1])^2)^2 +
	(x[1]-4 + sin(x[4])^2 + x[2]^2*x[3]^2)^2 +
	sin(x[4])^4 <= best_val_found + eps;


#solve;
##display f;
#display x;
