# Objective: convex
# Constraints: convex quadratic
# Feasible set: convex

################################################################
# Narrow band 3-dimensional beam pattern optimization
################################################################

param D := 2; # dimension
param pi := 4*atan(1);

param d := 0.5;
set Els := setof {i in -4..4, j in -4..4: -4 <= i+j && i+j <= 4 } 
		(d*(i+j/2), d*j*sqrt(3)/2);

#display card(Els);

param width := sqrt(card(Els));
param delta := 1/(2*width);
#display delta;

#param x {1..D, Els};

param delmax := ceil(2/(sqrt(3)*delta));
set P := setof {i in -delmax..delmax, j in -delmax..delmax:
		(i + j/2)^2 + 3*j^2/4 <= 1/delta^2} 
		(delta*(i+j/2), delta*j*sqrt(3)/2);

# The x,y,z components of the look direction
param lx := sin(pi*22.5/180)*cos(pi*30/180);  #22.5 degrees that's half way
param ly := sin(pi*22.5/180)*sin(pi*30/180);  #22.5 degrees
param lz := sqrt(1-lx^2-ly^2);

param beamwidth := 20;  # in degrees (actually halfwidth i.e. from center)
param cosbeamwidth := cos(beamwidth*pi/180);

# (px,py,pz) is inward pointing, whereas 
# (lx,ly,lz) is outward pointing
# p dot -l should be <= a threshold
set P_Sidelobes := setof {(px,py) in P: 
		-px*lx - py*ly + sqrt(abs(1-px^2-py^2))*lz <= cosbeamwidth}
		(px,py); 

printf {(px, py) in P}: "%6.2f %6.2f \n", px, py > p.out;
printf {(elx, ely) in Els}: "%6.2f %6.2f \n", elx, ely > el.out;
#display card(P);

var w_real {Els};
var w_imag {Els};

var A_real {(px,py) in P union {(-lx,-ly)} } = 
    sum {(elx,ely) in Els} (w_real[elx,ely]*cos(-2*pi*(px*elx + py*ely))
			  - w_imag[elx,ely]*sin(-2*pi*(px*elx + py*ely))
			   );

var A_imag {(px,py) in P union {-lx}cross{-ly} } = 
    sum {(elx,ely) in Els} (w_real[elx,ely]*sin(-2*pi*(px*elx + py*ely))
			  + w_imag[elx,ely]*cos(-2*pi*(px*elx + py*ely))
			   );

param rho := 10^(-15/10);   # 15 dB
param eps := 1.0e-8;

minimize L_one: 
    sum {(px,py) in P_Sidelobes} 
    sqrt((eps + A_real[px,py]^2 + A_imag[px,py]^2)/(1.01-(px^2+py^2)));

subject to chebychev {(px,py) in P_Sidelobes}:
    A_real[px,py]^2 + A_imag[px,py]^2 <= rho;

subject to gain_real:
    A_real[-lx,-ly] = 1;

subject to gain_imag:
    A_imag[-lx,-ly] = 0;

let {(elx,ely) in Els} w_real[elx,ely] := Uniform01();
#solve;

printf {(elx,ely) in Els}: "%6.2f %6.2f %6.2f %6.2f \n", elx, ely,
    w_real[elx,ely], w_imag[elx,ely] > nb.out;

#param pz {(px,py) in P} := sqrt(1-(px^2+py^2));
#param r  {(px,py) in P} 
#   := max(20*log10(sqrt(A_real[px,py]^2 + A_imag[px,py]^2)) + 50, 0);
#
#printf {(px,py) in P}: "%6.2f %6.2f %6.2f \n", 
#    r[px,py]*px, r[px,py]*py, r[px,py]*pz[px,py] > vertices;

# this finer grid is just for finer detail in the output
param delta2 := delta/3;
param delmax2 := ceil(2/(sqrt(3)*delta2));
set P2 := setof {i in -delmax2..delmax2, j in -delmax2..delmax2:
		(i + j/2)^2 + 3*j^2/4 <= 1/delta2^2} 
		(delta2*(i+j/2), delta2*j*sqrt(3)/2);

param A2_real {P2};
let {(px,py) in P2} A2_real[px,py] := 
    sum {(elx,ely) in Els} (w_real[elx,ely]*cos(-2*pi*(px*elx + py*ely))
			  - w_imag[elx,ely]*sin(-2*pi*(px*elx + py*ely))
			   );

param A2_imag {P2};
let {(px,py) in P2} A2_imag[px,py] := 
    sum {(elx,ely) in Els} (w_real[elx,ely]*sin(-2*pi*(px*elx + py*ely))
			  + w_imag[elx,ely]*cos(-2*pi*(px*elx + py*ely))
			   );

param r;
param x {0..1,0..1};
param y {0..1,0..1};
for {i in -delmax2-1..delmax2+1, j in -delmax2-1..delmax2+1} {
    for {ii in 0..1, jj in 0..1} {
        let x[ii,jj] := delta2*((i+ii)+(j+jj)/2);
        let y[ii,jj] := delta2*(j+jj)*sqrt(3)/2;
    }
    if (x[0,0]^2 + y[0,0]^2 <= 1 &&
        x[0,1]^2 + y[0,1]^2 <= 1 &&
        x[1,0]^2 + y[1,0]^2 <= 1) then {

        for {ii in 0..1, jj in 0..1: ii+jj<=1} {
            let r := max(10*log10(A2_real[x[ii,jj],y[ii,jj]]^2
                                + A2_imag[x[ii,jj],y[ii,jj]]^2) + 50, 0);
            printf "%6.2f %6.2f %6.2f ", x[ii,jj], y[ii,jj], r > faces;
        }
        printf "\n" > faces;
    };
    if (x[1,1]^2 + y[1,1]^2 <= 1 &&
        x[0,1]^2 + y[0,1]^2 <= 1 &&
        x[1,0]^2 + y[1,0]^2 <= 1) then {

        for {ii in 0..1, jj in 0..1: ii+jj>=1} {
            let r := max(10*log10(A2_real[x[ii,jj],y[ii,jj]]^2
                                + A2_imag[x[ii,jj],y[ii,jj]]^2) + 50, 0);
            printf "%6.2f %6.2f %6.2f ", x[ii,jj], y[ii,jj], r > faces;
        }
        printf "\n" > faces;
    };
}

