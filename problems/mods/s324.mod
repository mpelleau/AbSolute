# s324.mod	QQR2-AN-2-3
# Original AMPL coding by Elena Bobrovnikova (summer 1996 at Bell Labs).

# Ref.: K. Schittkowski, More Test Examples for Nonlinear Programming Codes.
# Lecture Notes in Economics and Mathematical Systems, v. 282,
# Springer-Verlag, New York, 1987, p. 145.

# Number of variables:  2
# Number of constraints:  3
# Objective convex separable quadratic
# Quadratic constraints

var x{1 .. 2} := 2;

param best_val_found := 5;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to Obj:
         0.01 * x[1]^2 + x[2]^2 <= best_val_found + eps;

s.t. G1:
     x[1] * x[2] - 25 >= 0;
s.t. G2:
     x[1]^2 + x[2]^2 - 25 >= 0;
s.t. B1:
     x[1] >= 2;

#solve;

##display Obj;
#display x;
