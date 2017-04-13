param f {1 .. 5};

param c {1 .. 5, 1 .. 5};

param d {1 .. 5};

param a{1 .. 10, 1 .. 5};

param b {1 .. 10};

var
    x {1 .. 5};
param best_val_found := -43.47440554;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to function: 
        sum {j in 1 .. 5} f[j]*x[j] + sum {i in 1 .. 5} sum {j in 1 .. 5} c[i,j]*x[i]*x[j] + sum {j in 1 .. 5} d[j] * x[j]^3 <= best_val_found + eps;
subject to
cons1 {i in 1 .. 5}: x[i] >= 0;
cons2 {i in 1 .. 10}: sum {j in 1 .. 5} a[i,j]*x[j] * x[j] - b[i] >= 0;

data;
param f := 
1 -15,
2 -27,
3 -36,
4 -18,
5 -12 ;

param c :
1 2 3 4 5 :=
1 30 -20 -10 32 -10
2 -20 39 -6 -31 32
3 -10 -6 10 -6 -10
4 32 -31 -6 39 -20
5 -10 32 -10 -20 30;

param d :=
1 4,
2 8,
3 10,
4 6,
5 2;

param a :
1 2 3 4 5 :=
1 -16 2 0 1 0
2 0 -2 0 4 2
3 -3.5 0 2 0 0
4 0 -2 0 -4 -1
5 0 -9 -2 1 -2.8 
6 2 0 -4 0 0
7 -1 -1 -1 -1 -1
8 -1 -2 -3 -2 -1
9 1 2 3 4 5
10 1 1 1 1 1;

param b := 
1 -40
2 -2
3 -0.25
4 -4
5 -4
6 -1
7 -40
8 -60
9 5
10 1;

#solve;
#display x;
