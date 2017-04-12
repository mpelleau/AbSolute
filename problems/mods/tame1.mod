var x >= 0;
var y >= 0;

param best_val_found := 0;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	(x-y)^2 <= 1;
subject to cons1:
	x+y-1 = 0;


solve;
##display f;
#display x,y;
