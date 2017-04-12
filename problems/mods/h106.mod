#   Heat Exchanger Design 
var
      x {1..8};

param best_val_found := 7049.248021;
param eps := 70.49248021; 		# = max(1, 1% x best_val_found)

subject to f: 
      x[1] + x[2] + x[3] <= best_val_found + eps;

subject to
cons1:       x[1] >= 100;
cons2:       x[1] <= 10000;
cons3 {i in 2..3}: x[i] >= 1000;
cons4 {i in 2..3}: x[i] <= 10000;
cons5 {i in 4..8}: x[i] >= 10;
cons6 {i in 4..8}: x[i] <= 1000;
cons7:       0.0025 * (x[4] + x[6]) <= 1;
cons8:       0.0025 * (-x[4] + x[5] + x[7]) <= 1;
cons9:       0.01 * ( - x[5] + x[8] ) <= 1;
cons10:       100 * x[1] - x[1] * x[6] + 833.33252 * x[4] - 83333.333 <= 0;
cons11:       x[2] * x[4] - x[2] * x[7] - 1250 * x[4] + 1250 * x[5] <= 0;
cons12:       x[3] * x[5] - x[3] * x[8] - 2500 * x[5] + 1250000 <= 0;

#solve;
#display x;
