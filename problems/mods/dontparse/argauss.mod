param xinit{1 .. 3};
param rhs{1 .. 15};
var x{i in 1 .. 3} := xinit[i];

param best_val_found := 1.12793277e-08;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1 .. 15} (x[1]*exp(-0.5*x[2]*(0.5*(8-i)-x[3])^2) - rhs[i])^2 <= best_val_found + eps;

data;
param rhs:=
1	0.0009
2	0.0044
3	0.0175
4	0.0540
5	0.1295
6	0.2420
7	0.3521
8	0.3989
9	0.3521
10	0.2420
11	0.1295
12	0.0540
13	0.0175
14	0.0044
15	0.0009;

param xinit:=
1	0.4
2	1.0
3	0.0;


#solve;
##display f;
#display x;

