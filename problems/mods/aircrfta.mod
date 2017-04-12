var rollrate := 0.0;
var pitchrat := 0.0;
var yawrate := 0.0;
var attckang := 0.0;
var sslipang := 0.0;
var elevator := 0.0;
var aileron := 0.0;
var rudderdf := 0.0;

param best_val_found := 3.006036375e-16;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
(-3.933*rollrate+0.107*pitchrat+0.126*yawrate-9.99*sslipang-45.83*aileron-7.64*rudderdf-0.727*pitchrat*yawrate+
8.39*yawrate*attckang-684.4*attckang*sslipang+63.5*pitchrat*attckang)^2 + (-0.987*pitchrat-22.95*attckang-
28.37*elevator+0.949*rollrate*yawrate+0.173*rollrate*sslipang)^2 + (0.002*rollrate-0.235*yawrate+5.67*sslipang-
0.921*aileron-6.51*rudderdf-0.716*rollrate*pitchrat-1.578*rollrate*attckang+1.132*pitchrat*attckang)^2 + (pitchrat-
attckang-1.168*elevator-rollrate*sslipang)^2 + (-yawrate-0.196*sslipang-0.0071*aileron+rollrate*attckang)^2 <= best_val_found + eps;

subject to cons1:
	elevator = 0.1;
subject to cons2:
	aileron = 0.0;
subject to cons3:
	rudderdf = 0.0;


#solve;
##display f;
#display rollrate, pitchrat, yawrate, attckang, sslipang, elevator, aileron, rudderdf;
