var x1 >= -10000, <= 10000;
var x2 >= 1, <= 100000;
var x3 >= -10000, <= 1;
var x4 >= 2, <=2 ;

param best_val_found := 30.49654415;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	x3-1 +
	x1^2+
	x2^2 + (x3+x4)^2 +
	sin(x3)^2 + x1^2*x2^2 + x4-3 +
	sin(x3)^2 +
	(x4-1)^2 +
	(x2^2)^2+
	(x3^2 + (x4+x1)^2)^2 +
	(x1-4 + sin(x4)^2 + x2^2*x3^2)^2 +
	sin(x4)^4 <= best_val_found + eps;
subject to cons4:
	x1^2+x2^2 -1 <= 0;

#solve;
##display f;
#display x;
