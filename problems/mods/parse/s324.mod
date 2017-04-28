var x{1 .. 2} >= 2, <=2;

param best_val_found := 5;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to Obj:
         0.01 * x[1]^2 + x[2]^2 <= best_val_found + eps;

G1: x[1] * x[2] - 25 >= 0;
G2: x[1]^2 + x[2]^2 - 25 >= 0;
B1: x[1] >= 2;
