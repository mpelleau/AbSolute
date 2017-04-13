param ndp:=12;
param h:=1/(ndp-1);
var x{i in 1 .. ndp} := ( (i-1)*h )*( (i-1)*h-1 );

param best_val_found := 2.962027638e-13;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 2 .. ndp-1} ( -x[i-1]+2*x[i]-x[i+1]+0.5*h^2*(x[i]+i*h+1)^3 )^2 <= best_val_found + eps;
subject to cons1:
	x[1] = 0.0;
subject to cons2:
	x[ndp] = 0.0;

#solve;
#display f;
#display x;
