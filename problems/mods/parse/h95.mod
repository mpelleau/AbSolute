
param B1 := 4.97;
param B2 := -1.88;
param B3 := -29.08;
param B4 := -78.02;

var x1  >= 0, <= 0.31;
var x2  >= 0, <= 0.046;
var x3  >= 0, <= 0.068;
var x4  >= 0, <= 0.042;
var x5  >= 0, <= 0.028;
var x6  >= 0, <= 0.0134;

param best_val_found := 0.01561952524;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
      4.3 * x1 + 31.8 * x2 + 63.3 * x3 + 15.8 * x4 + 68.5 * x5 + 4.7 * x6 <= best_val_found + eps;
subject to
cons8: 		17.1 * x1 + 38.2 * x2 + 204.2 * x3 + 212.3 * x4 + 623.4 * x5 +
    1495.5 * x6 - 169 * x1 * x3 - 3580 * x3 * x5 - 3810 * x4 * x5 -
            18500 * x4 * x6 - 24300 * x5 * x6 >= B1;
cons9:       17.9 * x1 + 36.8 * x2 + 113.9 * x3 + 169.7 * x4 + 337.8 * x5 +
            1385.2 * x6 - 139 * x1 * x3 - 2450 * x4 * x5 - 16600 * x4 * x6 - 17200
            * x5 * x6 >= B2;
cons10:       -273 * x2 - 70 * x4 - 819 * x5 + 26000 * x4 * x5 >= B3;
cons11:       159.9 * x1 - 311 * x2 + 587 * x4 + 391 * x5 + 2198 * x6
            - 14000 * x1 * x6 >= B4;


#solve;
#display x;
