var
  x {1..3} >= -4, <= 4;
param best_val_found := 3.320547646e-07;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
  x[1] >= best_val_found - eps;
subject to
  cons_eq2: x[1]^2*(1+x[1]) + x[2]^2 - x[3]^2 = 0;
  soft_cons1: x[2] >= 0;
  soft_cons2: x[3] >= 0;

#solve;
#display x;
