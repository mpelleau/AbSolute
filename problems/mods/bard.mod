param N := 3;
param M := 15;

param y{1 .. M};
param u{i in 1 .. M} := i;
param v{i in 1 .. M} := 16-i;
param w{i in 1 .. M} := min(u[i],v[i]);

var x{1 .. N} := 1;

param best_val_found := 0.008214877307;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1 .. M} ( y[i]-(x[1]+u[i]/(v[i]*x[2]+w[i]*x[3])) )^2 <= best_val_found + eps;

data;
param y:=
1	0.14
2	0.18
3	0.22
4	0.25
5	0.29
6	0.32
7	0.35
8	0.39
9	0.37
10	0.58
11	0.73
12	0.96
13	1.34
14	2.10
15	4.39;


#solve;
##display f;
#display x;
