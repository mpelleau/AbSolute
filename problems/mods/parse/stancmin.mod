var x1 >= 50.0, <=50;
var x2 >= 50.0, <=50;
var x3 >= 50.0, <=50;

param best_val_found := 5;
param eps := 1; 		# = max(1, 1% x best_val_found)
subject to f:
	-(6*x2+3*x1+2*x3-11)/(x1+4*x2+x3+1) <= best_val_found + eps;
subject to cons1:
	3*x1+4*x2+x3-2 <= 0;
subject to cons2:
	x1+4*x2+x3-1 <= 0;
subject to cons3:
	-(6*x2+3*x1+2*x3-11)/(x1+4*x2+x3+1) >= 5.0;
