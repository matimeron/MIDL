Function Ecrit, tet, enuns= dun, degrees= deg, elements= elar, num_elems= num,$
	dens= ro, dfac= dfac, weights= warr, formula= form, full= ful, _extra= _e

;+
; NAME:
;		ECRIT
; VERSION:
;		8.06
; PURPOSE:
;		Calculates Ec for a given material.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = ECRIT (TET, ELEMENTS = ELAR [, optional keywords])
; INPUTS:
;	TET
;		Angle of incidence, in radians, unless /DEGREES is set.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENUNS
;		Character value, specifies energy units.  Acceptable units are:
;		KEV, EV, NANOMETERS, ANGSTREM.  Only first 2 letters matter.  Default
;		units are specified by the variable ENUN in the common block SXR_STUFF,
;		initially set to the value of DENUN ('keV').  If provided, the value in
;		ENUNS replaces the previous ENUN.
;	/DEGREES
;		Switch.  Specifies that the TET is given in degrees. Default is radians.
;	ELEMENTS
;		Accepts a list of one or more elements which comprise the target.  Can
;		be provided as character array (each element represented by its
;		chemical symbol) or numeric array (each element represented by its Z).
;	NUM_ELEMS
;		Integer scalar or array.  Contains the numbers of elements in the 
;		superphase and subphase.  Note the following:
;			1:	If the NUM_ELEMS is scalar, or absent, it is ignored and it
;				is assumed that all the entries in ELEMENTS correspond to the
;				subphase, with the superphase being vacuum.
;			2:	If NUM_ELEMS has two entries, they represent the number of
;				elements in the superphase and subphase, respectively. In this
;				case the sum of the entries in NUM_ELEMS must equal the number
;				of entries in ELEMENTS.
;			3:	If NUM_elems has 3 or more entries, an error results.
;	DENS
;		Target density, scalar value or 2-element vector if NUM_ELEMS has 2 
;		elements.  If provided, overrides the value from the ABCTAB table.  
;		Must be provided if the target is not a pure element (i.e. if ELAR is a 
;		vector).
;	DFAC
;		Multiplier for elemental densities, used to account for the possibility
;		that layer densities differ from bulk densities.  Applies only to
;		single element targets.  Ignored if DENS is provided.  If NUM_ELEMS has
;		2 elements, DFAC, if provided, must have two elements as well.
;	WEIGHTS
;		In case the element list contains more than one element, the relative
;		partial weights of the elements in the target must be provided as a
;		numeric array, using this keyword.
;	/FORMULA
;		Switch.  If set and WEIGHTS (see above) are provided, they're taken as
;		"formula weights".
;	/FULL
;		Switch.  If set, the dispersion correction to Qc is calculated.
;	_EXTRA
;		A formal keyword used to pass keywords to the function DISPER_FUN.  Not
;		to be used directly.  See comment in RESTRICTIONS.
; OUTPUTS:
;		Returns the value(s) of Ec.  Output format is same as TET.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SXR_STUFF.  See LOAD_ABS_COEFFS for more information.
; SIDE EFFECTS:
;		If the keyword ENUNS is used, the value of ENUN in the common block
;		SXR_STUFF will be replaced by the new value.
; RESTRICTIONS:
;		1:	The elements specified must be among those in ABCTAB.
;		2:	The restrictions on NUM_ELEMS, see above.
; PROCEDURE:
;		Standard evaluation using QCRIT.  See details there.  Calls QCRIT from
;		SRUFF_LIB.  Also calls TOLER, FPU_FIX and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-AUG-2003 by Mati Meron as a front end to QCRIT.
;		Modified 25-JUN-2011 by Mati Meron.  Added keyword NUM_ELEMS and, 
;		through it, the option of arbitrary superphase.
;-

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

	on_error, 1

	if keyword_set(deg) then amult = !dtor else amult = 1.
	ccheck = (Wherinstruct('comp',_e))[0]
	if ccheck ge 0 then _e.(ccheck) = 0
	qc = (Qcrit(enuns= dun, elements= elar, num_elems= num, dens= ro, $
		dfac= dfac, weights= warr, formula= form, _extra= _e))[0]
	res = float(!srcon.conv)/(4*!pi*sin(amult*tet))*qc

	if keyword_set(ful) then begin
		dif = max(res)
		repeat begin
			pres = res
			pdif = dif
			qc = Qcrit(res, enuns= dun, elem= elar, num_elems= num, dens= ro,$
			dfac= dfac, weights= warr, formula= form, full= ful, _extra= _e)
			res = float(!srcon.conv)/(4*!pi*sin(amult*tet))*qc
			dif = max(abs(pres - res))
		endrep until dif le 2*Toler() or dif ge pdif
	endif

	return, FPU_fix(res)
end