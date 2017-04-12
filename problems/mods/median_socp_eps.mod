# Objective function: linear
# Constraint functions: second-order cone
# Feasible set: convex

# Computes the median of m numbers by minimizing
# the sum of the distances.  When m is odd, median
# lies at the middle data point.  The corresponding
# second-order cone constraint becomes almost "singular"
# but doesn't cause any problems.

param eps;

param m := 3;
param a {1..m};

var x;
var t {1..m};

param best_val_found := 0.7321190187;
param eps1 := 1;                 # = max(1, 1% x best_val_found)

subject to lin: sum {i in 1..m} t[i] <= best_val_found + eps1;

subject to dist {i in 1..m}: sqrt(eps+(x-a[i])^2) <= t[i];

data;

let eps := 1.0e-8;

let {i in 1..m} a[i] := Uniform01();

#solve;

let eps := 0;
##display lin;
#display x,t;
