Function Opt_vals, len, energy_range = e, angle_range = t, $
	radians = rad, degrees = deg, crystal = crs, index = ind, $
	gap = gap, blade = bld, enr_used = eran, anr_used = tran

;+
; NAME:
;		OPT_VALS
; VERSION:
;		4.3
; PURPOSE:
;		Calculating optimal parameters for wide spatial acceptance channel-cut
;		monochromator.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = OPT_VALS ([LEN], keywords)
; INPUTS:
;	LEN
;		Total length of crystal (arbitrary units).  Default value is 1.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENERGY_RANGE											| One and only one
;		A two element vector specifying optimization		| of ENERGY_RANGE
;		energy range (in keV).  Scalar input is allowed,	| an ANGLE_RANGE
;		in this case the range becomes a point.				| must be used
;	ANGLE_RANGE												|
;		A two element vector specifying optimization		|
;		angle range (units specified by keywords).  As		|
;		above, scalar input is allowed.
;	/RADIANS
;		Switch.  If set, the angular inputs and outputs are given in radians.
;	/DEGREES
;		Switch.  If set, the angular inputs and outputs are given in degrees.
;
;		Comment:	Either RADIANS or DEGREES (but not both) may be set.  If
;					neither is set, the default is DEGREES.
;	CRYSTAL
;		Character scalar translating to a name of recognized crystal.
;		Currently the recognized list includes : Diamond, Silicon, Germanium.
;		Only the first two letters are needed.
;	INDEX
;		The reflection index, given as 3 element vector.  Example: [1,1,1].
;	GAP
;		Optional output, see below.
;	BLADE
;		Optional output, see below.
;	ENR_USED
;		Optional output, see below.
;	ANR_USED
;		Optional output, see below.
; OUTPUTS:
;		Returns the minimal value of transverse acceptance (in the same units
;		in which LEN is given) over the optimization range.
; OPTIONAL OUTPUT PARAMETERS:
;	GAP
;		Returns the optimal width of the gap (channel) of the monochromator.
;	BLADE
;		Returns the optimal length of the blade (vane) of the monochromator.
;	ENR_USED
;		Returns the energy range used in the calculation.
;	ANR_USED
;		Returns the angle range used in the calculation.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Calculation following the writeup "Transverse Acceptance of a
;		Channel-cut Crystal".  Calls DEFAULT and ONE_OF from MIDL.  Also calls
;		BRAGG_ANGLE and MONO_ENERGY from MONO_LIB.
; MODIFICATION HISTORY:
;		Created 30-APR-2002 by Mati Meron.
;-

	on_error, 1

	len = Default(len,1.,/dtyp)
	if (One_of(deg,rad) > 0) eq 0 then amult = !dtor else amult = 1.

	case One_of(e,t) of
		0	:	begin
					eran = 1.*[min(e,max=max),max]
					tran = reverse(Bragg_angle(ene=eran,crys=crs,ind=ind,/rad))
				end
		1	:	begin
					tran = amult*[min(t,max=max),max]
					eran = reverse(Mono_energy(tran,crys=crs,ind=ind,/rad))
				end
		else:	message, 'Either energy or angle range must be given'
	endcase

	mult = 2*len/(5*sin(tran[1]+tran[0])- sin(tran[1]-tran[0])+ sin(2*tran[0]))
	gap = mult*sin(tran[0])*(sin(tran[0]) + sin(tran[1]))
	bld = mult*(2*sin(tran[1]+tran[0]) - sin(tran[1]-tran[0]))
	acp = sin(tran[0])*bld

	return, acp
end