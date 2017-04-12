param N:=100;
var x{1..N} := 1.0;

minimize f:
	sum {i in 1..N-1} (-4*x[i]+3.0) + sum {i in 1..N-1} (x[i]^2+x[N]^2)^2;


#solve;
##display f;
#display x;
