
param B1 := 4.97;
param B2 := -1.88;
param B3 := -29.08;
param B4 := -78.02;

var
   x  {1..6};

param best_val_found := 0.01561952524;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f: 
      4.3 * x[1] + 31.8 * x[2] + 63.3 * x[3] + 15.8 * x[4] + 68.5 * x[5] + 4.7 * x[6] <= best_val_found + eps;

subject to
cons1 {i in 1..6}: x[i] >= 0;
cons2:       	x[1] <= 0.31; 
cons3: 		x[2] <= 0.046; 
cons4:		x[3] <= 0.068; 
cons5:		x[4] <= 0.042; 
cons6:		x[5] <= 0.028; 
cons7:		x[6] <= 0.0134;
cons8: 		17.1 * x[1] + 38.2 * x[2] + 204.2 * x[3] + 212.3 * x[4] + 623.4 * x[5] +
    1495.5 * x[6] - 169 * x[1] * x[3] - 3580 * x[3] * x[5] - 3810 * x[4] * x[5] -
            18500 * x[4] * x[6] - 24300 * x[5] * x[6] >= B1;
cons9:       17.9 * x[1] + 36.8 * x[2] + 113.9 * x[3] + 169.7 * x[4] + 337.8 * x[5] +
            1385.2 * x[6] - 139 * x[1] * x[3] - 2450 * x[4] * x[5] - 16600 * x[4] * x[6] - 17200
            * x[5] * x[6] >= B2;
cons10:       -273 * x[2] - 70 * x[4] - 819 * x[5] + 26000 * x[4] * x[5] >= B3;
cons11:       159.9 * x[1] - 311 * x[2] + 587 * x[4] + 391 * x[5] + 2198 * x[6]
            - 14000 * x[1] * x[6] >= B4;


#solve;
#display x;
