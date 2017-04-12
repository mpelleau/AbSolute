# This is the problem whose solution is shown in Figure 2
# of 
# "FIR Filter Design via Spectral Factorization and Convex Optimization"
# S.P. Wu, S. Boyd, and L. Vandenberghe

param n := 30;
param N := 300;
param pi := 4*atan(1);
param alpha := 1.1;
param omega_p := 0.12*pi;
param omega_s := 0.24*pi;

set OMEGA_P := {0..omega_p by pi/N};
set OMEGA_S := {omega_s..pi by pi/N};
set OMEGA_I := {omega_p..omega_s by pi/N};
set OMEGA := OMEGA_P union OMEGA_S union OMEGA_I;

var delta >= 0;
var r {0..n-1};
var R {o in OMEGA} = r[0] + sum{k in 1..n-1} 2*r[k]*cos(k*o);

minimize stop_band_signal_bnd: delta;

subject to ripple_bnds {o in OMEGA_P}: 1/alpha^2 <= R[o] <= alpha^2;

subject to stop_bnd_def {o in OMEGA_S}: R[o] <= delta;

subject to nonneg_spectum {o in OMEGA}: R[o] >= 0;

#solve;

#display sqrt(delta);

#printf {o in OMEGA}: "%7.4f %10.3e \n", o, 20*log10(sqrt(R[o])) > lowpass.out;
