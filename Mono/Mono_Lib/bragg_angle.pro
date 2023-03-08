Function Bragg_angle, energy = ekev, wavelength = lamb, asym_angle = asym, $
	crystal = crs, index = ind, radians= rad, degrees= deg, uncorrected= unc, $
	darwin_width= dwid, delta_theta= dtet, ext_len= exl, etaval= eta, _extra= _e

;+
; NAME:
;		BRAGG_ANGLE
; VERSION:
;		8.442
; PURPOSE:
;		Calculates the Bragg Angle for a given crystal and energy/wavelength.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = BRAGG_ANGLE (keywords)
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENERGY									|	Either Energy or Wavelength,
;		Photon energy (in keV).				|	but never both, must be given.
;	WAVELENGTH								|
;		Photon wavelength (in Angstrem).	|
;	ASYM_ANGLE
;		Asymetry angle, scalar, defaults to zero.
;	CRYSTAL
;		Character scalar translating to a name of recognized crystal.
;		Currently the recognized list includes : Diamond, Silicon, Germanium.
;		Only the first two letters are needed.
;	INDEX
;		The reflection index, given as 3 element vector.  Example: [1,1,1].
;	/RADIANS
;		Switch.  If set, the inputs and outputs are given in radians.  That
;		includes the output itself, ASYM_ANGLE, DARWIN_WIDTH and DELTA_THETA.
;	/DEGREES
;		Switch.  If set, the inputs and outputs are given in degrees.  See
;		RADIANS above for list.
;
;		Comment:	Either RADIANS or DEGREES (but not both) may be set.  If
;					neither is set, the default is DEGREES.
;	/UNCORRECTED
;		Switch.  If set, the output does not include the refractive correction.
;	DARWIN_WIDTH
;		Optional output, see below.
;	DELTA_THETA
;		Optional output, see below.
;	EXT_LEN
;		Optional output, see below.
;	ETAVAL
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass additional keywords to QVAL.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the value(s) of the Bragg angles corresponding to the
;		energies/wavelengths given.  The angles include the refractive
;		correction (see DELTA_THETA below), unless /UNCORRECTED is set.
; OPTIONAL OUTPUT PARAMETERS:
;	DARWIN_WIDTH
;		Returns the value(s) of the Darwin widths corresponding to the
;		energies/wavelengths given.
;	DELTA_THETA
;		Returns the value(s) of the refractive corrections for the Bragg angles
;		corresponding to the energies/wavelengths given.
;	EXT_LEN
;		Returns the value(s) of the extinction lengths (in Angstrem)
;		corresponding to the energies/wavelengths given.
;	ETAVAL
;		Returns the value(s) of delta_E/E corresponding to the
;		energies/wavelengths given.  Note that for symetric reflection the
;		value is a constant, independent of energy.
; COMMON BLOCKS:
;		MONO_STUFF.  See SAVE_CRYST_DATA for detailed description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the reflection must be physically realizable.
; PROCEDURE:
;		Straightforward calculation using standard formulas (see Warren).
;		Calls DEFAULT and ONE_OF from MIDL.  Also calls QVAL from MONO_LIB.
; MODIFICATION HISTORY:
;		Created 25-APR-2002 by Mati Meron.
;		Modified 10-SEP-2006 by Mati Meron.  Added keyword EXT_LEN.
;		Modified 10-JAN-2015 by Mati Meron.  Internal changes to accomodate
;		cryogenic silicon.
;		Modified 25-DEC-2015 by Mati Meron.  Internal changes.
;-

	common mono_stuff, nfl, conv, cer, asftab

	on_error, 1

	if (One_of(deg,rad) > 0) eq 0 then amult = !dtor else amult = 1.
	qzop = Qval(crys=crs,ind=ind,/transmit,_extra=_e)/!pi
	eta = Qval(crystal=crs,index=ind,dzer=dzer,dref=dref,_extra=_e)*(2/!pi)

	case One_of(ekev,lamb,val=input) of
		0	:	wlam = conv/input
		1	:	wlam = input
		else:	message, 'Missing energy/wavelength input!'
	endcase

	sintetb = wlam/(2*dref)
	if max(sintetb) le 1 then tetb = asin(sintetb) else $
	message, 'Energy/wavelength out of range!'
	if eta eq 0 then exl = replicate(!values.f_nan,n_elements(wlam)) $
	else exl = 2*dref/(eta*tan(tetb))

	alp = amult*Default(asym,0.,/dtyp)
	b = sin(tetb-alp)/sin(tetb+alp)
	if min(b) le 0 then message, 'Asymetry out of range!'

	eta = eta/sqrt(b)
	dwid = eta*tan(tetb)/amult
	tet = asin(sintetb*(1 + 0.5*(1 + 1/b)*qzop))
	dtet = (tet - tetb)/amult

	if keyword_set(unc) then res = tetb/amult else res = tet/amult
	return, res
end