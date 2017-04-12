var
      x  {1..10} >= 2.001, <= 9.999;
param best_val_found := -45.77846971;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
      sum {i in 1..10} (log(x[i]-2))^2 + 
      sum {i in 1..10} (log(10-x[i]))^2 - 
      (prod {i in 1..10 } x[i])^0.2 <= best_val_found + eps;

#solve;
#display x;
