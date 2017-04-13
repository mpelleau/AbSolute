var x{1 .. 12} := 0;

param t{i in 1 .. 29} := i/29;

param best_val_found := 1.206024406e-07;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1 .. 29} (sum {j in 2 .. 12} (j-1)*x[j]*t[i]^(j-2) - (sum {j in 1 .. 12} x[j]*t[i]^(j-1))^2 - 1)^2 +
	x[1]^2 + (x[2]-x[1]^2-1)^2 <= best_val_found + eps;


#solve;
##display f;
#display x;
