# hs6.mod		QQR2-RN-2-1
# Original AMPL coding by Elena Bobrovnikova (summer 1996 at Bell Labs).

# Ref.: W. Hock and K. Schittkowski, Test Examples for Nonlinear Programming
# Codes.  Lecture Notes in Economics and Mathematical Systems, v. 187,
# Springer-Verlag, New York, 1981, p. 29.

# Number of variables: 2
# Number of constraints: 1
# Objective quadratic
# Nonlinear constraint

var x{1 .. 2};

param best_val_found := 4.083108452e-20;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
         (1 - x[1])^2 <= best_val_found + eps;

s.t. Constr:
         10 * (x[2] - x[1]^2) = 0;


data;
var x:=
    1 -1.2
    2  1 ;

#solve;

##display f;
#display x;
