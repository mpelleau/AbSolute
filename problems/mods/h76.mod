var
    x {1 .. 4};
param best_val_found := -4.681818182;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f: 
        x[1]^2 + 0.5 * x[2]^2 + x[3]^2 + 0.5*x[4]^2 - x[1]*x[3] +        x[3]*x[4] - x[1] - 3*x[2] + x[3] - x[4] <= best_val_found + eps;
subject to
cons1 {i in 1 .. 4}: x[i] >= 0;
cons2:         5 - x[1] - 2*x[2] - x[3] - x[4] >= 0;
cons3:         4 - 3*x[1] - x[2] - 2*x[3] + x[4] >= 0;
cons4:         x[2] + 4*x[3] - 1.5 >= 0;



#solve;
#display x;
