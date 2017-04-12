param M:=10;

param x_init{1..3};
var x{i in 1..3} := x_init[i];

param t{i in 1..M} := 0.1*i;

param best_val_found := 0.1503832352;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1..M} (exp(-t[i]*x[1])-exp(-t[i]*x[2])-x[3]*exp(-t[i])-exp(-10*t[i]))^2 <= best_val_found + eps;
subject to cons1:
	x[3] = 1.0;
data;
param x_init := 1 0 2 10 3 20;


#solve;
##display f;
#display x;
