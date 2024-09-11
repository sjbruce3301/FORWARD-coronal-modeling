The PFSSMOD is a numerical model that works with Potential Field
Source Surface extrapolation of the coronal field from a photospheric
magnetogram. 

	http://www.lmsal.com/~derosa/pfsspack/

The density, temperature, and pressure for this model are
analytic and based on a set of input keywords (step 2).

In order to use the PFSS model with the forward code, you must first
install the 'pfss' SolarSoft package and add it to your SSW_INSTR
list.

The steps to using the PFSSMOD model are:

    1 - If you do not already have a pfss save file, create
    one.  There are three ways to get these files:

	 - In sswidl type 'pfss_viewer', select a date, and then hit the Load
		button. Click OK when the program tries to access the file
		online. IDL will print a line like "pfss_viewer: restoring
		/tmp/Bfield_20080405_120400.h5", which tells you which file you
		downloaded and where it is stored. Copy
		or move the file to your working directory. Note: these files are
		created with a set grid, which may not be as fine as you require.

	 - Create your own save file from a magnetogram. Follow the
		instructions in $FORWARD/MODELS/PSFFMOD/make_pfss_save.pro. DON'T run
		it as a program file, it is only named .pro so you get the right
		encoded character coloring.

	- Run for_drive for PFSS model with a date set.  A save set will be downloaded to your local or working directory (when keyword working_dir set)

    2 - Decide what plasma profiles you want (density, temperature,
    pressure). The relevant keywords are: hydro, densprof, t0, topology. They are
    dsecribed in $FORWARD/MODELS/PFSSMOD/pfssmodprams.pro and in PFSSMODHELP.txt and the
    DEFAULTS directory. Your choices will be set as keyword inputs into the for_drive command.

    3 - Run the for_drive code on your pfss save file using the chosen
    plasma parameters.  See the DEFAULTS codes or the
    $FORWARD_DOCS/EXAMPLES/examples_pfss.txt file for more description on the
    specific keywords that go with this model.

NOTES:

    - If you are using a field that is external to your source surface
      (i.e. a continuance of the radial field), and your source
      surface is not the standard 2.5 r_sun, set rtop to the correct
      value. **Note if you run for_drive with a date and an rtop set, the
      save file will have a source surface set at rtop**

    - Each carrington magnetogram is a full sun, the location of the
      zero-meridian (the solar longitude closest to the Earth) is not
      automatically set to correspond to the date that you want. 

     **Note now for_drive will adjust cmer and bang to
     consistent with date, and also if your save file has a date it will set
     to be consistent with that*
 
     If you created your own save file, you will have to look online to
     find the central meridian for the time frame you want to view.
     **unless the date is part of the name**
