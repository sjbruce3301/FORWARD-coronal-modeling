General information about the various models that are currently coded
into the FORWARD code.
------------------------------------------------------------------
The main model codes are all in directories MODELS/$MODNAME/, named $modname.pro (they may call subroutines)

Model parameters are set within function MODELS/$MODNAME/$modnameprams.pro

All models can be called via
	
	$modname,r,theta,phi,ModPramsStruct,ModSolStruct
		(numerical models have an additional keyword, nreinit, see below)

	**r should be in units of Rsun, theta and phi in radians.**

	-- ModPramsStruct is a structure containing all model parameters, 
	set up as an output of function $modnameprams.pro.  Many (if not all) 
	of these parameters may be changed by the user via keywords of 
	$modnameprams.pro which are passed through from for_drive via /extra.

	-- ModSolStruct is the output of $modname.pro, containing Dens, Pres, Temp 
	at a minimum, and for some cases also Br, Bth, Bph (other output parameters 
	may be also included, see e.g. cavmorph.pro)

If you are looking for more information about the keywords associated with a given model, 
see $MODNAME/MODNAMEHELP.TXT.

By default, model structures should be centered on central meridian, equator: 
different views may be obtained by positioning the structure at 
THETAO, PHIO< or moving the viewer (keywords CMER, BANG). Also
------------------------------------------------------------------

There are two types of models that we use in the forward code, analytic and numerical

ANALYTIC: 

The analytic models solve for plasma properties at a given point 
in three-dimensional space, (r,theta,phi) via equations.

Information on each of the models is inside (MODNAME)/(MODNAME)_README. 
Also see $FORWARD_DOCS/EXAMPLES/examples_$modname.txt

NUMERICAL:

The numerical codes calculate plasma parameters at a given point in 
three-dimensional space, (r,theta,phi), based on an existing grid of values. 
New numerical datacubes can be added at any time as we now describe

********* NUMCUBE *********

The NUMCUBE model allows users to import any numerical datacube of
the solar corona and use it with the FORWARD code. 

The steps to using the NUMCUBE model are:

    1 - Make sure that the output from your simulation is in spherical
    coordinates with cgs units (except velocity, which should be km/sec,
    if it is included).  The grid must be regular and ordered,
    but does not need to be uniform. At minimum, your inital
    simulation must include r, theta, phi, br, btheta, bphi. You can
    also include density, temperature, pressure, and velocity, but they are
    optional. If your cube is not axisymmetric, bear in mind that
    for_drive expects models to be centered on phi = 0, theta=90 (equator). 
    However, the default POS plot will be from the viewer position
    of central meridian -90, so that your cube will be seen centered
    at the West (right) limb. Check $FORWARD/MODELS/NUMCUBE/make_my_cube.pro 
    for more detailed information about what is needed from your initial
    simulation.

    2 - Convert your simulation to the format required for the FORWARD
    code. Read and run $FORWARD/MODELS/NUMCUBE/make_my_cube.pro to do
    this step. Because the FORWARD code is updated regularly, your
    final simname.dat file should be kept in a working directory, not
    in the $FORWARD/ directory.

    3 - Decide how to handle the plasma and field outside of your
    cube. This information is set via the keywords bout, bo, hydro,
    densprof, te. Check $FORWARD/MODELS/NUMCUBE/numcubeprams.pro for
    more information. These keywords will be entered into your
    for_drive command.

    4 - You can also rotate your cube about a radial axis using the cuberot keyword. 
    Check $FORWARD/MODELS/NUMCUBE/numcubeprams.pro for more information on that.

    5 - Run for_drive with your numerical datacube and your keyword choices. 


-- Note on speed of numerical datacube calculations: If you are running multiple
 plots on the same datacube a given SSWIDL session, you can run the
 first for_drive as usual or with the keyword nreinit=1. You can then
 use nreinit=0 in subsequent calls to the same datacube. This will
 make use of internal common blocks so that you don't have to load
 your datacube each time, it cuts down on computing time.

***************ADAPTCUBE*************************
Parallel numerical cube interpolation scheme for adaptive/non-uniform meshes

Requires processing datacube with A_MAKEMYCUBE

UNDER DEVELOPMENT!!!!

