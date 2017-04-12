#    cattle feed 
var
   x {1..4} >= 1e-15, <= 1;

param best_val_found := 29.89437816;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f: 
      24.55*x[1] + 26.75*x[2] + 39*x[3] + 40.50*x[4] <= best_val_found + eps;

subject to
cons1:       sum {i in 1..4} x[i] = 1;
cons2:       2.3*x[1] + 5.6*x[2] + 11.1*x[3]+ 1.3*x[4] >= 5;
cons3:       12*x[1] + 11.9*x[2] + 41.8*x[3] + 52.1*x[4] - 21 -
      1.645*(0.28*x[1]^2 + 0.19*x[2]^2 + 20.5*x[3]^2 + 0.62*x[4]^2)^0.5 >= 0; 


#solve;
#display x;
