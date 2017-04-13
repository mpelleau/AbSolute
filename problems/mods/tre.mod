#  tre.mod             OBR2-AN-2-4
# Original AMPL coding by Elena Bobrovnikova (summer 1996 at Bell Labs).

# Ref.: C.Jansson and O.Knuppel. A global minimization method: the multi-
# dimensional case. Technische Informatik III, TU Hamburg-Hamburg, Jan.1992.

# Treccani function

# Number of variables: 2
# Number of constraints: 4
# Objective separable
# Objective nonconvex
# Simple bound constraints

# The global minimum is Ftre = 0, x = (0,0).
# There are saddle points at (-1,0) and (-2,0).

var x{1 .. 2} >= -5, <= 5;

param best_val_found := -1.776355967e-15;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to Ftre:
         x[1]^4 + 4*x[1]^3 + 4*x[1]^2 + x[2]^2 <= best_val_found + eps;

data;	# starting point

var x :=
    1  -1.1
    2   0.1;

#solve;

##display Ftre;

##display Ftre - 0;
#display x;
