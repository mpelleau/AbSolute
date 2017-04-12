param N:=10;
param M:=20;
var x{1..N} := 1.0;

param best_val_found := 6.135135135;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	2 + sum {i in 2..M-1} (sum {j in 2..N-1} x[j]*j*(i-1) - 1.0)^2 <= best_val_found + eps;


#solve;
##display f;
#display x;

