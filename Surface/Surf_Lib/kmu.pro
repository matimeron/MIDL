Function kmu, en, elements= elar, dens= ro, dfac= dfac, weights= warr, $
	formula= form

;+
; NAME:
;		KMU
; VERSION:
;		4.3
; PURPOSE:
;		Calculates the product of k and absorption coefficient, for x-rays.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = KMU (EN,  ELEMENT = ELAR [, optional keywords])
; INPUTS:
;	EN
;		Energy, assumed in the units specified by the variable ENUN in the
;		common block SXR_STUFF, unless specified otherwise by the keyword ENUNS.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ELEMENTS
;		Accepts a list of one or more elements which comprise the target.  Can
;		be provided as character array (each element represented by its
;		chemical symbol) or numeric array (each element represented by its Z).
;	DENS
;		Target density, scalar value.  If provided, overrides the value from
;		the ABCTAB table.  Must be provided if the target is not a pure element
;		(i.e. if ELAR is a vector).
;	DFAC
;		Multiplier for elemental densities, used to account for the possibility
;		that layer densities differ from bulk densities.  Applies only to
;		single element targets.  Ignored if DENS is provided.
;	WEIGHTS
;		In case the element list contains more than one element, the relative
;		partial weights of the elements in the target must be provided as a
;		numeric array, using this keyword.
;	/FORMULA
;		Switch.  If set and WEIGHTS (see above) are provided, they're taken as
;		"formula weights".
; OUTPUTS:
;		Returns product k*mu (where k is 2*pi/wavelength and mu is the
;		absorption coefficient) in unit of inverse Angstrem squared, for all
;		input energies in EN.  Format of result is same as the format of EN.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other then those specified for ABS_COEFFS.
; PROCEDURE:
;		Front end for ABS_COEFFS, in SRUFF.  Also calls FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 10-FEB-2004 by Mati Meron.
;-

	conv = 12.398424
	return, FPU_fix(1e-8/conv*2*!pi*en* $
			Abs_coeff(en,elem=elar,dens=ro,dfac=dfac,wei=warr,form=form))
end