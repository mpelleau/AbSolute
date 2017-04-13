# Objective  function: convex nonlinear
# Constraint functions: none

# Computes the median of m numbers by minimizing
# the sum of the distances.  When m is odd, median
# lies at the middle data point.  The corresponding
# distance term becomes "singular"
# and loqo fails.

param m;
param a {1 .. m};

var x;

param best_val_found := 4.626327416;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to sum_dists: sum {i in 1 .. m} abs(x-a[i]) <= best_val_found + eps;

data;
param m := 19;
let {i in 1 .. m} a[i] := Uniform01();

let x := 0.5;

#solve;
#display x;
