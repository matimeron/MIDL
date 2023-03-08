Function Scat_fact, x, crystal = crs, normalized = nrm

;+
; NAME:
;		SCAT_FACT
; VERSION:
;		8.72
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
;		Spline interpolation of tabulated values up to sin(theta)/lambda = 1.5,
;		extrapolation for higher values.  Calls SPLIN_EVAL and STRMATCH_MM from
;		MIDL.  Also calls LOAD_CRYST_DATA from MONO_LIB.
; MODIFICATION HISTORY:
;		Created 25-APR-2002 by Mati Meron.
;		Modified 5-OCT-2020 by Mati Meron.  Introduced extrapolation for high
;		values of sin(theta)/lambda.
;-

	common mono_stuff, nfl, conv, cer, asftab

	on_error, 1
	Load_cryst_data

	k = Strmatch_mm(crs,asftab.name,2)
	if k ge 0 then begin
		res = 0.*x
		xlim = asftab[k].ftab[asftab[k].flen,0]
		flim = asftab[k].ftab[asftab[k].flen,1]
		lo = where(x le xlim,nlo,comp=hi,ncomp=nhi)
		if nlo gt 0 then res[lo] = $
		Splin_eval(x[lo],asftab[k].ftab[0:asftab[k].flen,*])
		if nhi gt 0 then res[hi] = flim*xlim/x[hi]
	endif else message, 'Crystal not in list!'
	if keyword_set(nrm) then res = res/asftab[k].z

	return, res
end