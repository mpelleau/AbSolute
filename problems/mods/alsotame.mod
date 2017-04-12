var x;
var y;

param best_val_found := 0.08208499862;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	exp(x-2*y) <= best_val_found + eps;

subject to cons1:
	sin(-x+y-1)=0;
subject to cons2:
	-2 <= x <= 2;
subject to cons3:
	-1.5 <= y <= 1.5;


#solve;
##display f;
#display x, y;
