param N:=6;
param M:=13;
param xinit{1..N};
var x{i in 1..N}:=xinit[i];

param best_val_found := 2.245854679e-15;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1..M} (-exp(-0.1*i)+5*exp(-i)-3*exp(-0.4*i)
	+ x[3]*exp(-0.1*i*x[1]) - x[4]*exp(-0.1*i*x[2]) +
	x[6]*exp(-0.1*i*x[5]))^2 <= best_val_found + eps;
subject to cons1:
	x[6] = 3;
data;
param xinit:=
1	1
2	2
3	1
4	1
5	4
6	3;


#solve;
##display f;
#display x;

