Gibson, Bagenal, and Low 1996 model of magnetostatic corona
Based on dipole + octopole (axisymmetric) magnetic field
With current sheets around and above the central closed field structure
And hydrostatic density with step from closed to open region
Balancing magnetic pressure jump across current sheet
;
Model is based on initial formulation of Low 1986
;
Note the following typos were found in both papers:


Low86 (note his parameter a is our parameter bpar):

1) He says he plots 3*a*a*Z1 + 4*Z3 in Fig 4. But eq 29 says 3*a*a*Z1 - 4*Z3.  
But neither reproduce Fig 4.
If we assume the goal is to balance dipole and octopole at the open-closed boundary,
taking the limit a->infinity, maybe this should be 45*a*a*Z1 - 4*Z3

2) Also, eq 9 seems to be missing a factor of sin(theta). 
The stream functions in the appendix Z1 and Z3 are all sin^2 theta not sin theta, 
and this is necessary to end up with a dipole/octopole field when a --> infinity.


GBL:  

To be consistent with Low86, and to balance out singularities, 
the last term in eq A1 needs to be 4/pi/bpar * eta, not 2/pi/bpar * U. 
Also  Z3 last term should be eta instead of U

