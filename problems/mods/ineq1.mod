
var
    x {1..4} >= -10, <= 5;

param best_val_found := -0.4126055723;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
        - x[1] <= best_val_found + eps;
subject to
  cons1: x[1]^3 - x[2] + x[3]^2 >= 0;
  cons2: x[1]^2 - x[2] - x[4]^2 >= 0;
  cons3: x[1]^3 - x[2] + x[3]^2 <= 0.1;
  cons4: x[1]^2 - x[2] - x[4]^2 <= 0.1;

#solve;
#display x;
