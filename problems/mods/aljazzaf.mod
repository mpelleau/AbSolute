param N := 3;
param N1 := 2;
param Biga := 100.0;
param F := (Biga^2-1.0)/(N-1);
param F2 := (Biga^2-1.0)/(Biga*(N-1));
param A{i in 1 .. N} := Biga-(i-1)*F2;
param B{i in 1 .. N} := (i-1)*F+1.0;
var x{1 .. N} := 0.0, >= 0.0;

param best_val_found := 75.005;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	A[1]*(x[1]-0.5)^2 + sum {i in 2 .. N1} A[i]*(x[i]+1.0)^2 + sum {i in N1+1 .. N} A[i]*(x[i]-1.0)^2 <= best_val_found + eps;

subject to cons1:
	-B[1]*x[1]+B[1]+sum {i in 2 .. N1} B[i]*(x[i]-0.0)^2+sum {i in N1+1 .. N} B[i]*(x[i]-1.0)^2 = 0;

##display A, B, F, F2, f;

#solve;
##display f;
#display x;
