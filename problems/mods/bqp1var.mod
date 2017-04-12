var x1:=0.25;
param best_val_found := 0;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	x1+x1^2 <= best_val_found + eps;
subject to cons1:
	0.0 <= x1 <= 0.5;


#solve;
##display f;
#display x1;
