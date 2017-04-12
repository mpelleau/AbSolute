# 
# Fabien Seyfert (INRIA, MIAOU)
# Related to a filter design problem. 
# 
# Characteristics:
#      Number of variables: 9. 
#      The number of complex roots is 192. 
# 
# Ref: D. Bini and B. Mourrain. Polynomial test suite. 1996.
# 
# Nombre de solutions reelles par Inbox : 128 dans [-1.0e8,1.0e8]^9
# 


#  Heuristics for one solution 
# Bisection = max-narrow;

#  Heuristics for all solutions 


# Domains
var a >= -1.0e8, <= 1.0e8;
var b >= -1.0e8, <= 1.0e8;
var m{1..7} >= -1, <= 1;



subject to


cons1 : 0.01     = m[2]*m[4]*m[6];
cons2 : 7/500    = a*m[4]*b;
cons3 : 2/25     = a^2 + m[1]^2;
cons4 : 37/50    = b^2 + m[7]^2;
cons5 : 0.9401   = m[2]^2 + m[3]^2 + m[4]^2 + m[5]^2 + m[6]^2;

cons6 : 0.038589 = m[2]^2*(m[4]^2 + m[5]^2 + m[6]^2)+ m[3]^2*(m[5]^2 + m[6]^2)+ m[4]^2*m[6]^2;

cons7 : -0.0081  = (m[5]*m[7] - m[6]*b)*(m[1]*m[3] - a*m[2]);


# 
#   0.039/25 = m[4]*(m[1]*m[2]*m[3]*b
#              + a*(m[5]*m[6]*m[7]
#              - b*(m[6]^2 + m[2]^2))), 
# 
# 

cons8 : m[4]*(m[1]*m[2]*m[3]*b + a*m[5]*m[6]*m[7]) - (7/500)*(m[6]^2+m[2]^2) =  39/25000;


cons9 : 2.7173/4 = m[7]^2*((9401/10000) - m[6]^2)+ b^2*((9401/10000) - m[4]^2 - m[5]^2)- 2*b*m[5]*m[6]*m[7];



# ORIGINAL SYSTEM:
# 
# m[2]*m[4]*m[6] = 1/100,
# a*m[4]*b = 7/500,
# a^2+m[1]^2 = 2/25,
# b^2+m[7]^2 = 37/50,
# m[3]^2+m[5]^2+m[4]^2+m[2]^2+m[6]^2 = 9401/10000,
# 
# m[4]^2*m[6]^2+m[2]^2*m[4]^2+m[3]^2*m[6]^2+m[2]^2*m[5]^2+m[3]^2*m[5]^2
# +m[2]^2*m[6]^2 -(38589/1000000) = 0,
# 
# m[1]*m[3]*m[5]*m[7] - m[6]*m[1]*m[3]*b + m[2]*m[6]*a*b - m[2]*a*m[5]*m[7] + 81/10000 =0,
# 
# -m[1]*m[2]*m[3]*m[4]*b - a*m[4]*m[5]*m[6]*m[7] + a*m[4]*b*m[6]^2
# + a*m[2]^2*m[4]*b + 39/25000 =0,
# 
# m[4]^2*m[7]^2 + m[3]^2*m[7]^2 - 2*m[5]*m[6]*b*m[7] + m[2]^2*m[7]^2 + m[5]^2*m[7]^2
# + m[3]^2*b^2 + b^2*m[6]^2 + m[2]^2*b^2 - 27173/40000 =0
# 



#solve;
#display a, b, m;
