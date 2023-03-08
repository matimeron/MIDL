Pro Load_cryst_data, filename

;+
; NAME:
;		LOAD_CRYST_DATA
; VERSION:
;		4.2
; PURPOSE:
;		Loads monochromator crystal data and assorted constants from an XDR
;		binary data file into the common block MONO_STUFF.
; CATEGORY:
;		X-ray calculations
; CALLING SEQUENCE:
;		LOAD_CRYST_DATA [,FILENAME]
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;	FILENAME
;		If this parameter is specified then it must be the name of a binary
;		XDR file containing the appropriate crystal data.
;		If this parameter is not specified, then the default filename is
;		mono_data\crystal_data.dat where mono_data is an environment variable
;		pointing to the mono|data subdirectory in MIDL.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None explicit, data put in common block
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		MONO_STUFF.  Contains the following:
;			CONV:		Conversion constant from wavelength to energy.
;						Equals 12.398424 (Angstrem*keV)
;			CER:		The classic electron radius.
;						Equals 2.8179409e-5 (Angstrem)
;			ASFTAB:		Structure, containing crystal data.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The data file to be read from must be defined with the environment
; PROCEDURE:
;		Checks to see if it has been previously called. If so, it just returns
;		immediately.  This makes it convenient to call this routine in
;		every procedure which needs the crystal data.
;
;		Reads the data and fills the common block MONO_STUFF (see above).  The
;		structure ASFTAB in this block is explained in SAVE_CRYST_DATA.
;
;		Calls DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2002 by Mati Meron, based on the SAVE_ABS_COEFFS routine
;-

	common mono_stuff, nfl, conv, cer, asftab

	if Default(nfl,0) eq 0 then begin
		fname = getenv('mono_data') + 'crystal_data'
		filename  = Default(filename,fname + '.sav')
		restore, filename
		nfl = 1
	endif

	return
end