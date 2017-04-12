#----------------------------------------------------------------
# Problem TD (Truss Design) - Full dimensional
# Converted into AMPL by xuan-ha.vu@epfl.ch, EPFL/I&C/LIA
#----------------------------------------------------------------

#----------------------------------------------------------------
# Constants
#----------------------------------------------------------------
param E := 210e6; # (Unit = kN/m2, Youngs modulus of steel)
param T := 235000.0; # (Unit = kN/m2, yield stress of steel)
param A := 0.25; # Area of cross section of truss members
param r := 0.5; # Radius of gyration of the cross section of truss members
param Pi := 4*atan(1);
param Minf := 1e+32; # a very big positive number
param Epsilon := 0.00000001; # sufficiently small positive number

#----------------------------------------------------------------
# 2 parameters; Enter your values here
#----------------------------------------------------------------
param P := 400;
param H := 6;  #height of truss ???
param L := 10; #length of truss ???


#----------------------------------------------------------------
# 5 variables
#----------------------------------------------------------------
var x1 >=0.01, <=10;
var y1 >=0.01, <=10;
#var P >=350, <=450;
#var H >=5, <=10;
#var L >=1, <=10;

#----------------------------------------------------------------
# extra boolean variables for converting conditional constraints
# into conjunctive ones.
# ---------------------------------------------------------------

var bl >=0, <=1 integer;
var bl1 >=0, <=1 integer;
var bl2 >=0, <=1 integer;

#----------------------------------------------------------------
# Constraints
#----------------------------------------------------------------

subject to

#Original C00 : (P/tan(atan((H-y1)/(L-x1)))) < T*A;
C00 : (P/tan(atan((H-y1)/(L-x1)))) <= T*A - Epsilon;

#Original C01 : ((P*L/H)*tan(atan(y1/x1))) < T*A;
C01 : ((P*L/H)*tan(atan(y1/x1))) <= T*A - Epsilon;

#Original C02 : (P/sin(atan((H-y1)/(L-x1)))) < (Pi^2*E/((((L-x1)^2+(H-y1)^2)^0.5)/r)^2)*A;
C02 : (P/sin(atan((H-y1)/(L-x1)))) <= (Pi^2*E/((((L-x1)^2+(H-y1)^2)^0.5)/r)^2)*A - Epsilon;

#Original C03 : (P/sin(atan((H-y1)/(L-x1)))) < T*A;
C03 : (P/sin(atan((H-y1)/(L-x1)))) <= T*A - Epsilon;

#Original C04 : ((P*L/H)/cos(atan(y1/x1))) < (Pi^2*E/(((x1^2+y1^2)^0.5)/r)^2)*A;
C04 : ((P*L/H)/cos(atan(y1/x1))) <= (Pi^2*E/(((x1^2+y1^2)^0.5)/r)^2)*A - Epsilon;

#Original C05 : ((P*L/H)/cos(atan(y1/x1))) < T*A;
C05 : ((P*L/H)/cos(atan(y1/x1))) <= T*A - Epsilon;

#Original C06 : abs((((P*L/H) - (P/tan(atan((H-y1)/(L-x1)))))/cos(atan((H-y1)/x1)))) < T*A;
C06 : abs((((P*L/H) - (P/tan(atan((H-y1)/(L-x1)))))/cos(atan((H-y1)/x1)))) <= T*A - Epsilon;

#Original C07 : if (((P*L/H) - (P/tan(atan((H-y1)/(L-x1)))))/cos(atan((H-y1)/x1))) <= 0 then -(((P*L/H) - (P/tan(atan((H-y1)/(L-x1)))))/cos(atan((H-y1)/x1))) <= (Pi^2*E/(((x1^2+(H-y1)^2)^0.5)/r)^2)*A - Epsilon;
C07 : if (((P*L/H) - (P/tan(atan((H-y1)/(L-x1)))))/cos(atan((H-y1)/x1))) <= 0 then -(((P*L/H) - (P/tan(atan((H-y1)/(L-x1)))))/cos(atan((H-y1)/x1))) <= (Pi^2*E/(((x1^2+(H-y1)^2)^0.5)/r)^2)*A - Epsilon;

#Original C08 : x1 < L;
C08 : x1 <= L - Epsilon;

#Original C09 : y1 < H;
C09 : y1 <= H - Epsilon;

#solve;
##display x1, y1, P, H, L;
#display x1,y1;
