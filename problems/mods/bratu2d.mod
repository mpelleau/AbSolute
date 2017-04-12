param P:=7;
param lambda := 4.0;
param h:= 1/(P-1);
param c:= h^2*lambda;

var u{1..P,1..P};

param best_val_found := 1.116134513e-16;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 2..P-1, j in 2..P-1}
	(4*u[i,j]-u[i+1,j]-u[i-1,j]-u[i,j+1]-u[i,j-1]-c*exp(u[i,j]))^2 <= best_val_found + eps;
subject to cons1{j in 1..P}:
	u[1,j] = 0;
subject to cons2{j in 1..P}:
	u[P,j] = 0;
subject to cons3{i in 2..P-1}:
	u[i,P] = 0;
subject to cons4{i in 2..P-1}:
	u[i,1] = 0;


#solve;
##display f;
#display u;
