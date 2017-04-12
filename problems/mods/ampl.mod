#  NLP written by GAMS Convert at 02/13/04 18:07:17
#  
#  Equation counts
#     Total       E       G       L       N       X       C
#         3       2       0       1       0       0       0
#  
#  Variable counts
#                 x       b       i     s1s     s2s      sc      si
#     Total    cont  binary integer    sos1    sos2   scont    sint
#         3       3       0       0       0       0       0       0
#  FX     0       0       0       0       0       0       0       0
#  
#  Nonzero counts
#     Total   const      NL     DLL
#         5       3       2       0
# 
#  Reformulation has removed 1 variable and 1 equation


var x1 := 0.5, >= 0;
var x2 := 0.333333333333333, >= 0;

minimize obj:  - 0;

subject to

e2:    x1 + x2 = 1;

e3: (x1 - x2)^2 <= 1;

solve;