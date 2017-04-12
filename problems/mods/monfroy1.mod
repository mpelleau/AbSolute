# -----------------------------------------------------------------
# REFERENCE:
# Eric Monfroy, Michael Rusinowitch, Rene Schott.
# Implementing non-linear constraints with cooperative #solvers.
# ACM Symposium of Apllied Computing, 1996.
# (example 6.1)
# -----------------------------------------------------------------


# Domains
var Vx  >= 0, <= 1.0e5;
var Vy  >= 0, <= 1.0e5;
var Xf  >= 0.1, <= 1.0e5;
var Tf  >= 0, <= 1.0e5;

# Constants
param Tan := 1;
param V0 := 500;
param Yf := 0;


subject to
cons1 : Vy = Vx*Tan;
cons2 : Vx^2 + Vy^2 = V0;
cons3 : Xf = Vx*Tf;
cons4 : Yf + 4.905*Tf^2 = Vy*Tf;

# 
# POSSIBLE REDUNDANT CONSTRAINT:
# Vy^2 = 250
# 


#solve;
#display Tf, Vx, Vy, Xf;
