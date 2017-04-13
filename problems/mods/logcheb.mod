# This is the problem described in Section 5
# of 
# "FIR Filter Design via Spectral Factorization and Convex Optimization"
# S.P. Wu, S. Boyd, and L. Vandenberghe

param n := 50;
param N := 300;
param pi := 4*atan(1);
param omega_a := 0.01*pi;
param omega_b := 1.00*pi;

set OMEGA_0 := {0 .. omega_a by pi/N};
set OMEGA_P := {omega_a .. omega_b by pi/N};
set OMEGA_1 := {omega_a .. pi by pi/N};
set OMEGA := OMEGA_0 union OMEGA_P union OMEGA_1;

var alpha >= 0;
var r {0 .. n-1};
var R {o in OMEGA} = r[0] + sum{k in 1 .. n-1} 2*r[k]*cos(k*o);

minimize ripple: alpha;

subject to ripple_up_bnds {o in OMEGA_P}: o*R[o]/alpha <= alpha;

subject to ripple_lo_bnds {o in OMEGA_P}: 1/alpha <= o*R[o]/alpha;

subject to nonneg_spectum {o in OMEGA}:   R[o] >= 0;

let alpha := 1;

#solve;

#display alpha;

#printf {o in OMEGA_P}: "%7.4f %10.3e \n", o, 20*log10(sqrt(R[o]))
#	> logcheb.out;
