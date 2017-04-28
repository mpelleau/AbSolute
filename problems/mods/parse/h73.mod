var x1  >= 0, <= 1;
var x2  >= 0, <= 1;
var x3  >= 0, <= 1;
var x4  >= 0, <= 1;

param best_val_found := 29.89437816;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
      24.55*x1 + 26.75*x2 + 39*x3 + 40.50*x4 <= best_val_found + eps;

subject to
cons1:       x1+x2+x3+x4 = 1;
cons2:       2.3*x1 + 5.6*x2 + 11.1*x3+ 1.3*x4 >= 5;
cons3:       12*x1 + 11.9*x2 + 41.8*x3 + 52.1*x4 - 21 -
      1.645*sqrt(0.28*x1^2 + 0.19*x2^2 + 20.5*x3^2 + 0.62*x4^2)>= 0;


#solve;
#display x;
