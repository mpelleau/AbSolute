# Objective: nonconvex nonlinear
# Constraints: nonconvex nonlinear

# Maximum area for unit-diameter polygon of N sides.
# The following model started as a GAMS model by Francisco J. Prieto.

param N integer > 0;
set I := 1..N;

param pi := 4*atan(1.);

var rho{i in I} <= 1, >= 0      # polar radius (distance to fixed vertex)
                :=  4*i*(N + 1 - i)/(N+1)**2;

var the{i in I} >= 0            # polar angle (measured from fixed direction)
                := pi*i/N;

s.t. cd{i in I, j in i+1 .. N}:
        rho[i]**2 + rho[j]**2 - 2*rho[i]*rho[j]*cos(the[j]-the[i]) <= 1;

s.t. ac{i in 2..N}:
        the[i] >= the[i-1];

s.t. fix_theta: the[N] = pi;
s.t. fix_rho:   rho[N] = 0;

param best_val_found := 0.6749814429;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to area: .5*sum{i in 2..N} rho[i]*rho[i-1]*sin(the[i]-the[i-1]) >= best_val_found - eps;

data;
param N := 6;

#option loqo_options "pred_corr=0 mufactor=0.0 steplen=0.5 \
#	iterlim=100 verbose=2 sigfig=8 inftol=1.0e-5";

#option #solver loqo;	# fails after 0m0.61s
#option #solver minos;   # Optimal after 0m0.13s
#option #solver lancelot;   # Optimal after 0m0.39s

#solve;

#display rho,the;
