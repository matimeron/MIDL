Function Mono_energy_old, tet, crystal = crs, index = ind, asym_angle = asym, $
	radians = rad, degrees = deg, uncorrected = unc, $
	wavelength = lamb, delta_e = dee

;+
; NAME:
;		MONO_ENERGY
; VERSION:
;		4.3
; PURPOSE:
;		Calculates the energy corresponding to a given crystal and angle.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = MONO_ENERGY ( TET, keywords)
; INPUTS:
;	TET
;		Bragg angle.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
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
;	WAVELENGTH
;		Optional output, see below.
;	DELTA_E
;		Optional output, see below.
; OUTPUTS:
;		Returns the value(s) of the energies corresponding to the Bragg angles
;		given.  The values include the refractive correction (see DELTA_E
;		below), unless /UNCORRECTED is set.
; OPTIONAL OUTPUT PARAMETERS:
;	WAVELENGTH
;		Returns the wavelengths corresponding to the output energies.
;	DELTA_E
;		Returns the value(s) of the refractive corrections to energy
;		corresponding to the Bragg angles given.
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
;-

	common mono_stuff, nfl, conv, cer, asftab

	on_error, 1

	cfl = 1 - keyword_set(unc)
	if (One_of(deg,rad) > 0) eq 0 then amult = !dtor else amult = 1.
	wtet = amult*tet
	alp = amult*Default(asym,0.,/dtyp)
	b = sin(wtet-alp)/sin(wtet+alp)
	if min(b) le 0 then message, 'Asymetry out of range!'
	bmult = (1 + 1/b)/2

	qzop = Qval(crys=crs,ind=ind,dref=dref,/transmit)/!pi
	lamb = 2*dref*sin(wtet)/(1 + cfl*bmult*qzop)
	eb = conv/(2*dref*sin(wtet))
	dee = eb*bmult*qzop

	res = eb + cfl*dee
	return, res
end