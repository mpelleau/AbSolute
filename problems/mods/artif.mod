param N;

var x{0 .. N+1} := 1.0;

param best_val_found := 2.342773523e-16;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1 .. N}(-0.05*(x[i] + x[i+1] + x[i-1]) + atan( sin( (i mod 100)*x[i] ) ))^2 <= best_val_found + eps;

subject to cons1:
	x[0] = 0.0;

subject to cons2:
	x[N+1] = 0.0;

data;
param N:=10;


#solve; #display x;

