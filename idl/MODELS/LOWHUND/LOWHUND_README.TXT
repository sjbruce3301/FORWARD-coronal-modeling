The beginnings of an implementation of the Low-Hundhausen Cartesian flux rope model.

Starting from Equation 16 of Low and Hundhausen,

At the moment, the prominence current sheet (Aprom) is not included.

Also, Adip is set to zero 

Thus -- this is the solution discussed in section 3.2.4, and in particular, Figure 10.

Observable properties such as 

	cavity radius, height, external vertical field, external density at the photosphere and the ratio of density at the flux rope axis to surrounding hydrostatic density (same height) 

fully constrain model parameters.


Allowing Adip and eventually Aprom to vary may give some freedom in flux rope twist profile, for example, and possibly also cavity depletion profile.  However -- WARNING -- Adip hasn't really been tested and there may be units issues to take into consideration (e.g., Rsun or Rsun^2) in choosing values of adip

NOTE -- the temperature profile is increasing with height in this model, which is maybe ok for low coronal (below temperature maximum).  However, we have a flag "isothermal" which allows fixing a temperature above the bubble (fixed to value at z=ro), which is useful for the LOS integrated quantities.  If iso is set to a value, then the temperature is fixed everywhere (including below the bubble top) to that value.

NOTE -- the model is 2D Cartesian -- it is fit into the spherical coordinates of the FORWARD codes by assuming invariance in Phi.  This introduces an error in the magnetic force balance, and there is also an error in the graviational description which is gz.  Both should be minimal for small-radius flux rope positioned close to photosphere.
