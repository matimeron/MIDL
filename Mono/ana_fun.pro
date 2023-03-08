Function Ana_fun, e, len, opt_range = opr, crystal = crs, index = ind, $
	radians = rad, degrees = deg, miscut = mis, symmetric = sym, $
	gap = gap, blade = bld, center_off = cof, beam_off = bof

;+
; NAME:
;		ANA_FUN
; VERSION:
;		4.3
; PURPOSE:
;		Calculating the spatial acceptance for channel-cut monochromator.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = ANA_FUN (E [, LEN], keywords)
; INPUTS:
;	E
;		Photon energy (in keV).  Numeric scalar or vector.
; OPTIONAL INPUT PARAMETERS:
;	LEN
;		Total length of crystal (arbitrary units).  Default value is 1.
; KEYWORD PARAMETERS:
;	OPT_RANGE
;		A two element vector specifying optimization
;		energy range (in keV).  Scalar input is allowed,
;		in this case the range becomes a point.
;	CRYSTAL
;		Character scalar translating to a name of recognized crystal.
;		Currently the recognized list includes : Diamond, Silicon, Germanium.
;		Only the first two letters are needed.
;	INDEX
;		The reflection index, given as 3 element vector.  Example: [1,1,1].
;	/RADIANS
;		Switch.  If set, the miscut angle is given in radians.
;	/DEGREES
;		Switch.  If set, tthe miscut angle is given in degrees.
;
;		Comment:	Either RADIANS or DEGREES (but not both) may be set.  If
;					neither is set, the default is DEGREES.
;	MISCUT
;		Magnitude of the miscut angle.  Default is zero.
;	/SYMMETRIC
;		Switch.  If set, limits the reflection to the part symmetric around
;		the centerline of the beam.
;	GAP
;		Width of the gap (channel) of the monochromator.  If not given, the
;		optimal value is used.
;	BLADE
;		Length of the blade (vane) of the monochromator.  If not given, the
;		optimal value is used.
;	CENTER_OFF
;		The offset of the center of rotation from the optimal position.  See
;		"Transverse Acceptance of a Channel-cut Crystal".  Default is zero.
;	BEAM_OFF
;		The transverse offset of the centerline of the beam from the optimal
;		location (passing through the center of rotation).  Default is zero.
; OUTPUTS:
;		Returns the values of transverse acceptance (in the same units
;		in which LEN is given) corresponding to the input energy(s).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Calculation following the writeup "Transverse Acceptance of a
;		Channel-cut Crystal".  Calls DEFAULT and ONE_OF from MIDL.  Also calls
;		BRAGG_ANGLE from MONO_LIB and OPT_VALS.
; MODIFICATION HISTORY:
;		Created 30-APR-2002 by Mati Meron.
;-

	on_error, 1

	len = Default(len,1.,/dtyp)
	if (One_of(deg,rad) > 0) eq 0 then amult = !dtor else amult = 1.

	if n_elements(opr) ne 0 then $
	oacp = Opt_vals(len,energy=opr,blade=obld,gap=ogap,crys=crs,ind=ind)
	tet = Bragg_angle(ene=e,crys=crs,ind=ind,/rad)

	wmis = amult*Default(mis,0.,/dtyp)
	tetp = tet + wmis
	tetm = tet - wmis

	wgap = Default(gap,ogap,/dtyp)
	wbld = Default(bld,obld,/dtyp)
	wcof = Default(cof,0.,/dtyp)
	wbof = Default(bof,0.,/dtyp)

	xd = len/2.
	xc = wbld - xd
	xb = - xc
	xa = - xd

	gaprop = wgap/tan(tetp)
	gaprom = wgap/tan(tetm)

	x1 = -0.5*gaprom - wcof/tan(tetm) + wbof/sin(tetm)
	x1p = (xb + gaprom) < xc < (xd - gaprop)
	x1m = ((xa + gaprom) > (xb + (gaprom - gaprop))> (xc - gaprop)) - gaprom

	acpp = (x1p - x1)*sin(tetm)
	acpm = (x1 - x1m)*sin(tetm)
	if keyword_set(sym) then acptot = 2*((acpp < acpm) > 0) $
	else acptot = (((3*acpm + acpp) < (3*acpp + acpm)) > 0)/2

	return, acptot
end