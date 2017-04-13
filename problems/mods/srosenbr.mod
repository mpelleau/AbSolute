param N:=10;

var x{i in 1 .. N} := if (i mod 2 == 1) then -1.2 else 1;

param best_val_found := 7.519518519e-18;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 2 .. N/2} ( (10*(x[2*i]-x[i-1]^2))^2 + (1-x[2*i-1])^2 ) <= best_val_found + eps;


#solve;
##display f;
#display x;
