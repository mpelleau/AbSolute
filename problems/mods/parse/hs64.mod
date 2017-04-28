# hs64.mod	OOR2-MN-3-4
# Original AMPL coding by Elena Bobrovnikova (summer 1996 at Bell Labs).

# Ref.: W. Hock and K. Schittkowski, Test Examples for Nonlinear Programming
# Codes.  Lecture Notes in Economics and Mathematical Systems, v. 187,
# Springer-Verlag, New York, 1981, p. 86.

# Number of variables: 3
# Number of constraints: 4
# Objective separable
# Objective nonconvex
# Nonlinear constraints

var x1  >= 0, <= 10000;
var x2  >= 0, <= 10000;
var x3  >= 0, <= 10000;
param best_val_found := 6299.842428;
param eps := 62.99842428; 		# = max(1, 1% x best_val_found)

subject to Obj:
         5 * x1 + 50000 / x1 + 20 * x2 + 72000 / x2 + 10 * x3 +
         144000 / x3 <= best_val_found + eps;

cons: 1 >= 4/x1 + 32/x2 + 120/x3;

#solve;

##display Obj;
#display x;
