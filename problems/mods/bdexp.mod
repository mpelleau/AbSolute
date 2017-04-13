param N := 100;
param ngs := N-2;
var x{1 .. N} := 1.0;

param best_val_found := 4.373251654e-40;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1 .. ngs} (x[i]+x[i+1])*exp((x[i]+x[i+1])*(-x[i+2])) <= best_val_found + eps;

#solve;
#display f;
#display x;
