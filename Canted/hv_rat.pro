Function HV_rat, ene, rsig = rsg, asig = asg, crystal = crs, index = ind, $
	pack = pac, hres = hrs, vres = vrs, _extra = _e

;+
; NAME:
;		HV_RAT
; VERSION:
;		8.714
; PURPOSE:
;		Calculates the intensity ratios between Bragg reflections in plane
;		perpendicular and parallel to polarization direction.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = HV_RAT ( ENE, keywords)
; INPUTS:
;	ENE
;		Scalar or vector, X-ray energy(s).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RSIG
;		Beam spatial sigma, in mm.  Must be given as a 2-element vector, in
;		[horizontal, vertical] order.  If not given, default values will be read
;		from the global parameter !BLPAR.
;	ASIG
;		Same as RSIg, for angular sigma, value(s) in mr.
;
;		Note:	While the default units for RSIG and ASIG are mm and mr,
;				respectively, the routine will recognize, within reasonable
;				limits, inputs provided in meter and radian, or micrometer and
;				microradian units, and will convert to the default units as
;				needed.
;	CRYSTAL
;		Character scalar translating to a name of recognized crystal.
;		Currently the recognized list includes : Diamond, Silicon, Germanium.
;		Only the first two letters are needed.
;	INDEX
;		The reflection index, given as 3 element vector.  Example: [1,1,1].
;	/PACK
;		Switch.  If set,  the result is returned as a 2-column array, where the
;		first column contains the energy values and the second contains the
;		reflectivity ratios.
;	_EXTRA
;		A formal keyword used to pass additional keywords to imbedded routines.
;		Not to be used directly.
; OUTPUTS:
;		Returns the ratio of crystal reflectivities for polarization
;		perpendicular and parallel to the reflection plane.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.  However, if the energy is below the minimum possible for
;		reflection, NaN is returned.
; PROCEDURE:
;		Straightforward calculation using standard formulas.
;		Calls ABC_GEN, ABC_INT and ABC_REFC, from BLOPT_LIB.
;		Calls BRAGG_ANGLE, MONO_ENERGY and REF_CURVE, from MONO_LIB.
;		Calls DEFAULT, FPU_FIX and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2018 by Mati Meron.
;		Modifed 15-SEP-2019 by Mati Meron.  Internal changes.
;-

	on_error, 1

	crs = Default(crs,'si')
	ind = Default(ind,[1,1,1],/dtyp)

	res = (hrs = (vrs = 0.*ene))
	mene = (1 + Toler())*Mono_energy(90,crys=crs,ind=ind,_extra=_e)
	dum = where(ene ge mene, ndum, comp=cdum, ncomp=ncdum)
	if ncdum gt 0 then res[cdum] = (hrs[cdum] = (vrs[cdum] = !values.f_nan))

	for i = 0, ndum-1 do begin
		j = dum[i]
		tet = Bragg_angle(ene=ene[j],crys=crs,ind=ind,eta=eta,/rad,_extra=_e)
		ref = Ref_curve(0,ene=ene[j],crys=crs,ind=ind)

		abc0 = ABC_gen(ene[j],rsig=rsg,asig=asg,ban=1,_extra=_e)
		full = ABC_int(abc0)

		abctv = ABC_refc(abc0,tet,!dpi/2,eta=eta,ref=ref,/rad,/cle)
		abcv = ABC_refc(abctv,tet,-!dpi/2,eta=eta,ref=ref,/rad,/cle)
		abcth = ABC_refc(abc0,tet,0,eta=eta,ref=ref,/rad,/cle)
		abch = ABC_refc(abcth,tet,!dpi,eta=eta,ref=ref,/rad,/cle)

		hrs[j] = sqrt(2)*ABC_int(abch)/(eta*full)
		vrs[j] = sqrt(2)*ABC_int(abcv)/(eta*full)
	endfor
	res[dum] = hrs[dum]/vrs[dum]

	if keyword_set(pac) then begin
		hrs = transpose([[ene],[hrs]])
		vrs = transpose([[ene],[vrs]])		
		res = transpose([[ene],[res]])
	endif

	return, FPU_fix(res)
end