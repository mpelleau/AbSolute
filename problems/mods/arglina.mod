param N:=100;
param M:=200;
var x{1 .. N} := 1.0;

param best_val_found := 300;
param eps := 3; 		# = max(1, 1% x best_val_found)

subject to f:
sum {i in 1 .. N} ((sum{j in 1 .. i-1} -2*x[j]/M) + x[i]*(1-2/M) + (sum {j in i+1 .. N} -2*x[j]/M) - 1)^2 +
sum {i in N+1 .. M} (sum{j in 1 .. N} -2*x[j]/M - 1)^2 <= best_val_found + eps;


#solve;
##display f;
#display x;
