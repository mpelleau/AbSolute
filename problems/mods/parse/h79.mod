param s2 := sqrt(2);
var
    x {1 .. 5} >= 0, <= 4;
param best_val_found := 0.07877682087;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
        (x[1] - 1)^2 + (x[1]-x[2])^2 + (x[2]-x[3])^2 + (x[3]-x[4])^4 + (x[4]-x[5])^4 <= best_val_found + eps;
subject to
cons1:         x[1] + x[2]^2 + x[3]^3 = 2 + 3*s2;
cons2:         x[2] - x[3]^2 + x[4] + 2 = 2 * s2;
cons3:         x[1] * x[5] = 2;


#solve;
#display x;
