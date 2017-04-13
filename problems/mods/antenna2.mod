# This is the problem whose solution is shown in Figure 10
# of 
# "FIR Filter Design via Spectral Factorization and Convex Optimization"
# S.P. Wu, S. Boyd, and L. Vandenberghe

param n := 12;
param N := 300;
param pi := 4*atan(1);
param alpha := 1.58;
param d_over_lambda := 0.45;
param omega_0 := -2*pi*d_over_lambda*cos(0);
param omega_b := -2*pi*d_over_lambda*cos(pi/6);
param omega_s := -2*pi*d_over_lambda*cos(pi/4);
param omega_pi:= -2*pi*d_over_lambda*cos(pi);

set OMEGA_P := {omega_0 .. omega_b  by omega_pi/N};
set OMEGA_S := {omega_s .. omega_pi by omega_pi/N};
set OMEGA_I := {omega_b .. omega_s  by omega_pi/N};
set OMEGA := OMEGA_P union OMEGA_S union OMEGA_I;

var delta2 >= 0;
var r_real {0 .. n-1};
var r_imag {1 .. n-1};
var R {o in OMEGA} = 
    r_real[0] + 
    2*sum {k in 1 .. n-1} (r_real[k]*cos(k*o) + r_imag[k]*sin(k*o));

minimize stop_band_signal_bnd: delta2;

subject to ripple_bnds {o in OMEGA_P}: 1/alpha^2 <= R[o] <= alpha^2;

subject to stop_bnd_def {o in OMEGA_S}: R[o] <= delta2;

subject to nonneg_spectum {o in OMEGA}: R[o] >= 0;

#solve;

#display sqrt(delta2);

/*
printf {o in OMEGA}: "%7.4f %10.3e \n%7.4f %10.3e \n", 
    acos(-o/(2*pi*d_over_lambda)), 
    max(0,log10(sqrt( R[o])) + 3),
    -acos(-o/(2*pi*d_over_lambda)), 
    max(0,log10(sqrt( R[o])) + 3)
	> antenna.out;

printf "\n" >> antenna.out;
printf "%7.4f %10.3e \n", 0, (-0/20)+3 >> antenna.out;
printf "%7.4f %10.3e \n", 0, (-10/20)+3 >> antenna.out;
printf "%7.4f %10.3e \n", 0, (-20/20)+3 >> antenna.out;
printf "%7.4f %10.3e \n", 0, (-30/20)+3 >> antenna.out;
*/