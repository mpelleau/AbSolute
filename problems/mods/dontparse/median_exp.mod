# Objective function: linear
# Constraint functions: second-order cone
# Feasible set: convex

# Computes the median of m numbers by minimizing
# the sum of the distances.  When m is odd, median
# lies at the middle data point.  The corresponding
# second-order cone constraint becomes "singular"
# and loqo fails.

param m;
param a {1 .. m};

var x;
var t {1 .. m} >= 0;

minimize lin: sum {i in 1 .. m} t[i];

subject to dist {i in 1 .. m}: exp((x-a[i])^2 - t[i]^2) <= 1;

data;
param m := 19;
let {i in 1 .. m} a[i] := Uniform01();

let x := 0.5;

#solve;
#display x, t;
