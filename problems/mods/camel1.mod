# camel1.mod OBR2-AN-2-4
# Original AMPL coding by Elena Bobrovnikova (summer 1996 at Bell Labs).

# Six Hump Camel back

# Ref.: C. Jansson and O. Knueppel, "A Global Minimization Method:
# the Multi-Dimensional Case", Technische Informatik III,
# TU Hamburg-Hamburg, Jan. 1992, p. 33 (problem "C").

# Number of variables: 2
# Number of constraints: 4
# Onjective nonseparable
# Objective nonconvex
# Simple bound constraints

# There are two global minima with Fhump = -1.031628453,
# (0.089842,-0.712656) and (-0.089842, 0.712656).

var x{1 .. 2} >= -5, <= 5, := 1;

param best_val_found := -1.031628453;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to Fhump:
	4*x[1]^2 - 2.1*x[1]^4 + x[1]^6/3 + x[1]*x[2] - 4*x[2]^2 + 4*x[2]^4 <= best_val_found + eps;

#solve;

##display Fhump;

##display Fhump - -1.031628453;

#display x;
