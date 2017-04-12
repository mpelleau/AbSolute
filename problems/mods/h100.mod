var
      x  {1..7};
param best_val_found := 680.6300574;
param eps := 6.806300574; 		# = max(1, 1% x best_val_found)

subject to f: 
      (x[1] - 10)^2 + 5 * (x[2] - 12)^2 + x[3]^4 + 3 * (x[4] - 11)^2 +      10*x[5]^6 + 7*x[6]^2  + x[7]^4 - 4*x[6]*x[7] - 10*x[6] - 8 * x[7] <= best_val_found + eps;
subject to
cons1:       127 - 2 * x[1]^2 - 3 * x[2]^4 - x[3] - 4 * x[4]^2 - 5 * x[5] >= 0;
cons2:       282 - 7 * x[1] - 3 * x[2] - 10 * x[3]^2 - x[4] + x[5] >= 0;
cons3:       196 - 23 * x[1] - x[2]^2 - 6 * x[6]^2 + 8 * x[7] >= 0;
cons4:       -4*x[1]^2 - x[2]^2 + 3*x[1]*x[2] - 2*x[3]^2 - 5*x[6] + 11*x[7] >= 0;

#solve;
#display x;
