# Objective: linear
# Constraints: convex
# Feasible set: convex

# This model finds the filter weights for a finite impulse response (FIR)
# filter.
#
# Reference: "Applications of Second-Order Cone Programming",
# M.S. Lobo, L. Vandenberghe, S. Boyd, and H. Lebret, 1998

param pi := 4*atan(1);

param beta := 0.01;

param omega_stop := 2*pi/3;
param omega_pass :=   pi/2;
param step := pi/180;
set OMEGA_STOP := {omega_stop .. pi by step};
set OMEGA_PASS := {0 .. omega_pass  by step};

param n := 20;	# must be even
param n2 := n/2;

var h {0 .. n2-1};
var t >= 1;

param best_val_found := 1.046487581;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to spread: t <= best_val_found + eps;

subject to passband_up_bnds {omega in OMEGA_PASS}:
           2* sum {k in 0 .. n2-1} h[k]*cos((k-(n-1)/2)*omega) <= t;

subject to passband_lo_bnds {omega in OMEGA_PASS}:
    1/t <= 2* sum {k in 0 .. n2-1} h[k]*cos((k-(n-1)/2)*omega);

subject to stopband_bnds {omega in OMEGA_STOP}:
    -beta <= 2* sum {k in 0 .. n2-1} h[k]*cos((k-(n-1)/2)*omega) <= beta;

let t := 1;

#solve;

##display 20*log10(t);
#display h;
#display t;
