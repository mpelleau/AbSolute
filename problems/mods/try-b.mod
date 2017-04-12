var x >= 0, := 10.0;
var y >= 0, := 10.0;

param best_val_found := 1;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	(x-1)^2 <= best_val_found + eps;
subject to cons1:
	-1+(x-1)^2+(y-10)^2 = 0;


#solve;
##display f;
#display x,y;
