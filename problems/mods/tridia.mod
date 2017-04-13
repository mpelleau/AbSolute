param N;
param alpha;
param beta;
param gamma;
param delta;

var x{1 .. N}:=1.0;

param best_val_found := 3.132827601e-24;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	gamma*(x[1]*delta-1.0)^2 + sum {i in 2 .. N} i*(-beta*x[i-1]+alpha*x[i])^2 <= best_val_found + eps;

data;
param N:=30;
param alpha:=2.0;
param beta:=1.0;
param gamma:=1.0;
param delta:=1.0;


#solve; #display x;

