var
    x{1 .. 2} >= -4, <= 4;
param best_val_found := -1.732050808;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
log(1 + x[1]^2) - x[2] <= best_val_found + eps;
subject to
cons1:         (1 + x[1]^2)^2 + x[2]^2 = 4;

#solve;
#display x;
