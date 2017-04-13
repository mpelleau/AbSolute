# schwefel.mod	OBR2-AN-5-10
# Original AMPL coding by Elena Bobrovnikova (summer 1996 at Bell Labs).

# Ref.: C. Jansson and O. Knueppel, "A Global Minimization Method:
# the Multi-Dimensional Case", Technische Informatik III,
# TU Hamburg-Hamburg, Jan. 1992, p. 103 (problem "whs25").

# Schwefel function

# Number of variables:  5
# Number of constraints:  10
# Objective separable convex
# Simple bound constraints

# The global minimum is Fs = 0, x = (0,0,0,0,0).

set I := {1 .. 5};

var x{I} <= 0.4, >= -0.5, := .4;

param best_val_found := 1.683893205e-12;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to Fs:
     sum {i in I} x[i]^10 <= best_val_found + eps;

#solve;

##display Fs;

##display Fs - 0;
#display x;
