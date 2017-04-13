param s2 := sqrt(2);
var
        x {1 .. 5} >= 0, <= 4;
param best_val_found := 0.2415051288;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
        2*x[1]*(x[1]-x[2]-1) + 1 + x[2]^2 + (x[3] - 1)^2 + (x[4] - 1)^4 + (x[5] - 1)^6 <= best_val_found + eps;
subject to
cons1:         x[1]^2 * x[4] + sin(x[4] - x[5]) - 2 * s2 = 0;
cons2:         x[2] + x[3]^4*x[4]^2 - 8 - s2 = 0;


#solve;
#display x;
