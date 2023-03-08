Function Ref_curve, rtet, energy = ekev, wavelength = lamb, $
	crystal = crs, index = ind, asym_angle = asym, $
	radians= rad, degrees= deg, corr_center= ccen, uncorr_center= ucen,_extra=_e

;+
; NAME:
;		REF_CURVE
; VERSION:
;		8.4
; PURPOSE:
;		Calculates the Darwin reflection profile for a given crystal and
;		energy/wavelength.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = REF_CURVE ( RTET, keywords)
; INPUTS:
;	RTET
;		Set of angles, relative to the Bragg reflection angle.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENERGY									|	Either Energy or Wavelength,
;		Photon energy (in keV).				|	but never both, must be given.
;	WAVELENGTH								|
;		Photon wavelength (in Angstrem).	|
;	CRYSTAL
;		Character scalar translating to a name of recognized crystal.
;		Currently the recognized list includes : Diamond, Silicon, Germanium.
;		Only the first two letters are needed.
;	INDEX
;		The reflection index, given as 3 element vector.  Example: [1,1,1].
;	ASYM_ANGLE
;		Asymetry angle, scalar, defaults to zero.
;	/RADIANS
;		Switch.  If set, the input and ASYM_ANGLE are given in radians.
;	/DEGREES
;		Switch.  If set, the input and ASYM_ANGLE are given in degrees.
;
;		Comment:	Either RADIANS or DEGREES (but not both) may be set.  If
;					neither is set, the default is DEGREES.
;	/CORR_CENTER
;		Switch.  If set, the input is taken relative to the corrected (for
;		refraction) Bragg_angle.
;	/UNCORR_CENTER
;		Switch.  If set, the input is taken relative to the uncorrected
;		Bragg_angle
;
;		Comment:	Either CORR_CENTER or UNCORR_CENTER (but not both) may be
;					set.  If neither is set, the default is CORR_CENTER.
;	_EXTRA
;		A formal keyword used to pass additional keywords to QVAL.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the reflectivity curve corresponding to the energy/wavelength
;		given and the set of angles RTET.  Alternatively, may return a
;		reflectivity curve corresponding to a set of energy/wavelength values
;		and a *single* value of RTET.
;
;		Note:		Either RTET or energy/wavelength *must* be a scalar.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		MONO_STUFF.  See SAVE_CRYST_DATA for detailed description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the reflection must be physically realizable.
; PROCEDURE:
;		Straightforward calculation using standard formulas (see Warren).
;		Calls ABS_MM, DEFAULT, IMAGINARY_MM, ONE_OF, SIGN and STRMATCH_MM from
;		MIDL.  Also calls BRAGG_ANGLE and QVAL from MONO_LIB.
; MODIFICATION HISTORY:
;		Created 25-APR-2002 by Mati Meron.
;		Modifed 10-JAN-2005 by Mati Meron.  Internal changes.
;		Modified 10-JAN-2015 by Mati Meron.  Internal changes to accomodate
;		cryogenic silicon.
;-

	common mono_stuff, nfl, conv, cer, asftab

	on_error, 1
	i = complex(0,1)

	if (n_elements(ekev) gt 1 or n_elements(lamb) gt 1) $
	and n_elements(rtet) gt 1 then $
	message, 'Either the energy/wavelength or the angle range must be a scalar!'

	if (One_of(deg,rad) > 0) eq 0 then amult = !dtor else amult = 1.
	wtet = amult*rtet
	alp = amult*Default(asym,0.,/dtyp)
	tetb = Bragg_angle(ener=ekev,wave=lamb,cryst=crs,ind=ind,asym=alp,/rad,$
			del=dtet,/uncorr)
	b = sin(tetb-alp)/sin(tetb+alp)
	if min(b) le 0 then message, 'Asymetry out of range!'
	if (One_of(ccen,ucen) > 0) then wtet = wtet - dtet

	p = !pi/tan(tetb)*wtet
	q = Qval(cryst=crs,ind=ind,dref=dref,geo=gef,_extra=_e)

	cnum = Strmatch_mm(crs,asftab.name,2)
	elem = asftab[cnum].csym
	mu = 1e-8*dref/(2*sin(tetb))*Abs_coeff(conv/(2*dref*sin(tetb)),elem=elem)

	p = p + i*mu
	q = q/sqrt(b) - gef*i*mu
	tem = p^2 - (q^2)
	s = Sign(Imaginary_mm(tem),/noz)
	res = (p - s*sqrt(tem))/q

	return, Abs_mm(res)^2
end