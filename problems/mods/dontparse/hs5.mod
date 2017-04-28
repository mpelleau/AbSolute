# hs5.mod		OBR2-MN-2-4
# Original AMPL coding by Elena Bobrovnikova (summer 1996 at Bell Labs).

# Ref.: W. Hock and K. Schittkowski, Test Examples for Nonlinear Programming
# Codes.  Lecture Notes in Economics and Mathematical Systems, v. 187,
# Springer-Verlag, New York, 1981, p. 28.

# Number of variables: 2
# Number of constraints:  4
# Objective nonseparable
# Objective nonconvex
# Simple bound constraints

var x{1 .. 2};

param best_val_found := -1.913222955;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
         sin(x[1] + x[2]) + (x[1] - x[2])^2 - 1.5 * x[1] + 2.5 * x[2] + 1 <= best_val_found + eps;

s.t. C1:
     -1.5 <= x[1] <= 4;
s.t. C2:
     -3 <= x[2] <= 3;

data;

var x:=
    1   0
    2   0;

#solve;

##display f;
#display x;
