# hs8.mod		CQR2-RN-2-2
# Original AMPL coding by Elena Bobrovnikova (summer 1996 at Bell Labs).

# Ref.: W. Hock and K. Schittkowski, Test Examples for Nonlinear Programming
# Codes.  Lecture Notes in Economics and Mathematical Systems, v. 187,
# Springer-Verlag, New York, 1981, p. 31.

# Number of variables: 2
# Number of constraints: 2
# Objective constant
# Nonlinear constraints

var x{1..2};

param best_val_found := -1;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f: -1 <= best_val_found + eps;

s.t. C1: x[1]*x[1] + x[2]*x[2] == 25;
s.t. C2: x[1]*x[2] == 9;

data;

var x :=
    1    2
    2    1  ;

#solve;

##display f;
#display x;
