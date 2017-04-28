#   optimal sample size

param u1 := 5e5;
param u2 := 4e5;
param u3 := 3e5;
param u4 := 2e5;

var x1  >= 0, <= u1;
var x2  >= 0, <= u2;
var x3  >= 0, <= u3;
var x4  >= 0, <= u4;

param best_val_found := 727.6794128;
param eps := 7.276794128; 		# = max(1, 1% x best_val_found)

subject to f:
       1 + x1 + x2 + x3 + x4 <= best_val_found + eps;

subject to
cons3:        0.0401 - 4/x1 - 2.25/x2 - 1/x3 - 0.25/x4 >= 0;
cons4:        0.010085 - 0.16/x1 - 0.36/x2 - 0.64/x3 - 0.64/x4 >= 0;


#solve;
#display x;
