
param a := 0.55;  
param c1 := 2/3*1e-6;

var
    x {1 .. 4};
param best_val_found := 5126.49811;
param eps := 51.2649811; 		# = max(1, 1% x best_val_found)

subject to f: 
        x[1] * (3 + 1e-6*x[1]^2) + x[2] * (2 + c1*x[2]^2) <= best_val_found + eps;
subject to
cons1 {i in 1 .. 2}: x[i] >= 0;
cons2 {i in 1 .. 2}: x[i] <= 1200;
cons3 {i in 3 .. 4}: x[i] >= -a;
cons4 {i in 3 .. 4}: x[i] <= a;
cons5:         x[4] - x[3] + a >= 0;
cons6:         x[3] - x[4] + a >= 0;
cons7:         1000 * sin(-x[3] - 0.25) + 1000 * sin(-x[4]-0.25) + 894.8 - x[1] = 0;
cons8:         1000 * sin(x[3] - 0.25) + 1000 * sin(x[3]-x[4]-0.25) + 894.8 - x[2] = 0;
cons9:         1000 * sin(x[4] - 0.25) + 1000 * sin(x[4]-x[3]-0.25) + 1294.8 = 0;



#solve;
#display x;
