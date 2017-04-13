param N:=10;
var x{1 .. N} := 1/N;

param best_val_found := 3.625749082e-15;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1 .. N} (i*(cos(x[i])+sin(x[i])) + sum {j in 1 .. N} cos(x[j]) - (N+i) )^2 <= best_val_found + eps;

#solve; 
#display f; 
#display x;
