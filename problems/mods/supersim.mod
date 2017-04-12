var x >= 0;
var y;
param best_val_found := 0.6666666667;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	x <= best_val_found + eps;
subject to cons1:
	x+2*y-2 = 0;
subject to cons2:
	2*x+y-2 = 0;


#solve;
##display f;
#display x,y;
