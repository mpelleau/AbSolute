# Objective: linear
# Constraints: convex quadratic
# Feasible set: convex

# This model finds the shape of a hanging chain
# The solution is known to be y = cosh(a*x) + b
# for appropriate a and b.

param N := 100;	# number of chainlinks
param L := 1;	# difference in x-coords of endlinks

param h := 2*L/N;	# length of each link

var x {0 .. N};	# x-coordinates of endpoints of chainlinks
var y {0 .. N};	# y-coordinates of endpoints of chainlinks

param best_val_found := -67.19921528;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to pot_energy: sum{j in 1 .. N} (y[j-1] + y[j])/2 <= best_val_found + eps;

subject to x_left_anchor: x[0] = 0;
subject to y_left_anchor: y[0] = 0;
subject to x_right_anchor: x[N] = L;
subject to y_right_anchor: y[N] = 0;

subject to link_up {j in 1 .. N}: (x[j] - x[j-1])^2 + (y[j] - y[j-1])^2 <= h^2;

let {j in 0 .. N} x[j] := j*L/N;
let {j in 0 .. N} y[j] := 0;

#solve;

#display x,y;
