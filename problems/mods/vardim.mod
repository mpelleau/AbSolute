param N := 10;
var x{i in 1 .. N} := 1-i/N;

param best_val_found := 1937336.183;
param eps := 19373.36183; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1 .. N} (x[i]-1)^2 + (sum {i in 1 .. N} i*x[i] - N*(N+1)/2)^2 + (sum {i in 1 .. N} i*x[i] - N*(N+1)/2)^4 <= best_val_found + eps;
subject to cons1:
	sum {i in 1 .. N} (x[i]-1)^2 + (sum {i in 1 .. N} i*x[i] - N*(N+1)/2)^2 + (sum {i in 1 .. N} i*x[i] - N*(N+1)/2)^4 
>= 0;

#solve;
#display f;
#display x;
