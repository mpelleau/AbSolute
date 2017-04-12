param N:=2;
var x{1..N} := 1.0;

param best_val_found := 3.025614878e-15;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	(-1.5+x[1]*(1.0-x[2]))^2 + (-2.25+x[1]*(1.0-x[2]^2))^2 + (-2.625+x[1]*(1.0-x[2]^3))^2 <= best_val_found + eps;


#solve;
##display f;
#display x;
