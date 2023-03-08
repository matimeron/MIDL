Function Scat_fact_old, x, crystal = crs, normalized = nrm

;+
; NAME:
;		SCAT_FACT
; VERSION:
;		4.3
; PURPOSE:
;		Calculates atomic scattering factors.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = SCAT_FACT( X, CRYSTAL = CRS)
; INPUTS:
;	X
;		Numeric, value(s) of sin(theta)/lambda.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	CRYSTAL
;		Character scalar corresponding to one of the recognized crystals.
;		Currently recognized are: Diamond, Silicon, Germanium.
;	/NORMALIZED
;		Switch.  If set, the results are divided by the Z of the crystal.
; OUTPUTS:
;		Returns the value(s) of the atomic scattering factor corresponding to
;		the input.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		MONO_STUFF.  See SAVE_CRYST_DATA for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Spline interpolation of tabulated values.  Calls SPLIN_EVAL and
;		STRMATCH_MM from MIDL.  Also calls LOAD_CRYST_DATA from MONO_LIB.
; MODIFICATION HISTORY:
;		Created 25-APR-2002 by Mati Meron.
;-

	common mono_stuff, nfl, conv, cer, asftab

	on_error, 1
	Load_cryst_data

	k = Strmatch_mm(crs,asftab.name,2)
	if k lt 0 then message, 'Crystal not in list!' $
	else res = Splin_eval(x,asftab[k].ftab[0:asftab[k].flen,*])
	if keyword_set(nrm) then res = res/asftab[k].z

	return, res
end