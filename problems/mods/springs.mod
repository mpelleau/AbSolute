# Objective: convex quadratic
# Constraints: convex second-order cone
# Feasible set: convex


# This model finds the shape of a hanging chain where each
# link is a spring that buckles under compression and each
# node has a weight hanging from it.  The springs are assumed weightless.
#
# Reference: "Applications of Second-Order Cone Programming",
# M.S. Lobo, L. Vandenberghe, S. Boyd, and H. Lebret, 1998

param N := 10;	# number of chainlinks

# a = (a_x, a_y) and b = (b_x, b_y) are positions of end nodes.
param a_x := 0;	
param a_y := 0;	
param b_x := 2;	
param b_y := -1;	

param l0 := 2*sqrt((a_x-b_x)^2 + (a_y-b_y)^2)/N;  # rest length of each spring

param g := 9.8;	# acceleration due to gravity
param m {1..N-1}, default 1; 	# mass of each hanging node

param k := 100;	# stiffness of springs

var x {0..N};		# x-coordinates of nodes
var y {0..N};		# y-coordinates of nodes
var t {1..N} >= 0;	# extension of each spring

param best_val_found := -185.4460619;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to pot_energy: sum {j in 1..N-1} m[j]*g*y[j] 
	     + (k/2)*sum {j in 1..N}   t[j]^2  <= best_val_found + eps;

subject to x_left_anchor: x[0] = a_x;
subject to y_left_anchor: y[0] = a_y;
subject to x_right_anchor: x[N] = b_x;
subject to y_right_anchor: y[N] = b_y;

subject to link_up {j in 1..N}: 
    sqrt((x[j] - x[j-1])^2 + (y[j] - y[j-1])^2)  <= l0 + t[j];

let {j in 0..N} x[j] := (j/N)*b_x + (1-j/N)*a_x;
let {j in 0..N} y[j] := (j/N)*b_y + (1-j/N)*a_y;

#solve;

#display x,y;
#display t;
