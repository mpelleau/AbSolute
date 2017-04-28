# Objective: nonconvex nonlinear
# Constraints: bounds

param n := 20;
param pi := 4*atan(1);

var theta {0 .. n} >= 0, <= 2*pi;

param best_val_found := -1.224646799e-16;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to area: 
     0.5 * sum {i in 0 .. n-1} sin(theta[i+1] - theta[i]) >= best_val_found - eps;

#subject to ordered {i in 1 .. n-1}: theta[i] >= theta[i-1];

subject to anchored0: theta[0] = 0;
subject to anchoredn: theta[n] = 2*pi;

#option loqo_options "pred_corr=0 mufactor=0.0 steplen=0.5 \
#	iterlim=100 verbose=2 sigfig=8 inftol=1.0e-5";

#option #solver loqo;	# Optimal after 0m0.09s
#option #solver minos;	# Optimal after 0m0.04s.  Wrong answer
#option #solver lancelot;    # Optimal after 0m0.05s.  Wrong answer

#solve;

#display theta;
##display area, n*sin(2*pi/n)/2;

