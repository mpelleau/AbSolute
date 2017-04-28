param N:=10;
param al{i in 1 .. N} := i;
param a{k in 1 .. N} := if (k=1) then sum {i in 1 .. N} al[i] else 
			sum {i in 1 .. N} exp(k*log(al[i]));
var x{i in 1 .. N} := (i-1)/N;

param best_val_found := 1.22941e+34;
param eps := 1.229e+32;

subject to f:
	(sum {i in 1 .. N} x[i] - a[1])^4 +
	sum {k in 2 .. N} (sum {i in 1 .. N} x[i]^k - a[k])^4 <= best_val_found + eps;
subject to cons1{i in 2 .. N}:
	x[i]-x[i-1] >= 0;


#solve;
#display x;
