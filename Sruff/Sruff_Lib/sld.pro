Function SLD, elements = elar, num_elems = num, dens = ro, dfac = dfac, $
	weights = warr, formula = form, _extra = _e

;+
; NAME:
;		SLD
; VERSION:
;		8.21
; PURPOSE:
;		Calculates scattering length density for a given material.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = SLD (ELEMENTS = ELAR [, optional keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
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
;	_EXTRA
;		A formal keyword used to pass keywords to the function DISPER_FUN.  Not
;		to be used directly.  See comment in RESTRICTIONS.
; OUTPUTS:
;		Returns the value (scalar) of the scattering length density, in A(-2).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1:	The elements specified must be among those in ABCTAB.
;		2:	The restrictions on NUM_ELEMS, see above.
; PROCEDURE:
;		Straightforward evaluation using QCRIT.  See details there.  Calls 
;		QCRIT from SRUFF_LIB.  Also calls FPU_FIX and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-SEP-2013 by Mati Meron.
;-

	on_error, 1

	ccheck = (Wherinstruct('comp',_e))[0]
	if ccheck ge 0 then _e.(ccheck) = 0

	res = Qcrit(elements= elar, num_elems= num, dens= ro, dfac= dfac, $
		weights= warr, formula= form, _extra= _e)^2/(16*!pi)

	return, FPU_fix(res)
end