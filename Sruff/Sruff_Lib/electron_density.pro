Function Electron_density, elements = elar, dens = ro, dfac = dfac, $
	weights = warr, formula = form

;+
; NAME:
;		ELDEN
; VERSION:
;		8.16
; PURPOSE:
;		Electron density evaluation.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = ELECTRON_DENSITY( ELEMENT = ELAR [, optional keywords])
; INPUTS:
; 		None.
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
;		Density multiplier, used to account for the possibility that layer
;		densities differ from bulk densities.  Default value is 1.
;	WEIGHTS
;		In case the element list contains more than one element, the relative
;		partial weights of the elements in the target must be provided as a
;		numeric array, using this keyword.
;	/FORMULA
;		Switch.  If set and WEIGHTS (see above) are provided, they're taken as
;		"formula weights".
; OUTPUTS:
;		Returns electron density in units of e/A^3.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SXR_STUFF.  See LOAD_ABS_COEFFS for more information.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard evaluation using densities from ABCTAB (in common block
;		SXR_STUFF).  Uses calls to ELECOMP, and LOAD_ABS_COEFFS.  Also calls
;		DEFAULT and FPU_FIX, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-AUG-2012 by Mati Meron, through surgery on DIELECT.
;-

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

	on_error, 1
	Load_abs_coeffs

	dfac = Default(dfac,1.,/dtyp)
	eli = Elecomp(elar, number = nel)
	if nel gt 1 then begin
		if n_elements(ro) eq 1 then begin
			if n_elements(warr) eq nel then begin
				if keyword_set(form) then twarr = warr*abctab[eli].a $
				else twarr = warr
				tro = dfac*ro/total(twarr)*twarr
			endif else message, 'Bad or missing composition data!
		endif else message, 'Bad or missing density!'
	endif else tro = dfac*Default(ro,abctab[eli].ro)

	res = 1e-24*!pcon.na*total(abctab[eli].z*tro/abctab[eli].a)

	return, FPU_fix(res)
end