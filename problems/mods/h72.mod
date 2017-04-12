#   optimal sample size 

param u{i in 1..4} := (5 - i)*1e5;

var
   x  {1..4};

param best_val_found := 727.6794128;
param eps := 7.276794128; 		# = max(1, 1% x best_val_found)

subject to f: 
       1 + x[1] + x[2] + x[3] + x[4] <= best_val_found + eps;

subject to
cons1 {i in 1..4}: x[i] >= 1e-3;
cons2 {i in 1..4}: x[i] <= u[i];
cons3:        0.0401 - 4/x[1] - 2.25/x[2] - 1/x[3] - 0.25/x[4] >= 0;
cons4:        0.010085 - 0.16/x[1] - 0.36/x[2] - 0.64/x[3] - 0.64/x[4] >= 0;


#solve;
#display x;
