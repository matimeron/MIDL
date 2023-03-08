Pro Rock_curve, energy= ene, crystal= crs, index= ind, scaled= sca, output= pres

;+
; NAME:
;		ROCK_CURVE
; VERSION:
;		8.72
; PURPOSE:
;		Calculates an x-ray rocking curve.
; CATEGORY:
;		Experimental utility.
; CALLING SEQUENCE:
;		ROCK_CURVE, ENERGY = ENE, CRYSTAL = CRS, INDEX = IND [optional keywords]
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENERGY
;		X-ray energy in keV.  Scalar, mandatory.
;	CRYSTAL
;		Character scalar translating to a name of recognized crystal.
;		Currently the recognized list includes : Diamond, Silicon, Germanium.
;		Only the first two letters are needed.
;	INDEX
;		The reflection index, given as 3 element vector.  Example: [1,1,1].
;	/SCALED
;		Switch.  If set, the X coordinate of the resulting plot is in units of
;		rocking_angle/Darwin_width.  The defaault unit is microradians.
;	OUTPUT
;		Optional output, see below.
; OUTPUTS:
;		Normally only plots the rocking curve to the screen.  However, see
;		OUTPUT below.
; OPTIONAL OUTPUT PARAMETERS:
;	OUTPUT
;		Returns the rocking curve as a [2,N] array, where the first column is
;		angle value (scaled by Darwin width if /SCALED is set) and the second
;		column is the reflected intensity relative to peak intensity.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		Energy input needs to be a scalar.
; PROCEDURE:
;		Straightforward numeric integration.
;		Calls BRAGG_ANGLE, and REF_CURVE from MONO_LIB.  Calls INTEG, JOIN_XY,
;		and MAKE_GRID, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JAN-2018 by Mati Meron.
;		Documented 20-DEC-2020 by Mati Meron.
;-

	on_error, 1

	ndar = 4
	nstp = 50

	tet = Bragg_angle(ene=ene,crys=crs,index=ind,dar=dar,/rad)
	stet = Make_grid(ndar*nstp*[-1,1],1,/step)
	rtet = dar/nstp*stet
	ref = Ref_curve(rtet,ene=ene,crys=crs,index=ind,/cor,/rad)

	ctet = rtet[ndar*nstp/2:3*ndar*nstp/2]
	cref = ref[ndar*nstp/2:3*ndar*nstp/2]
	res = 0*ctet
	for i = 0, ndar*nstp do res[i] = Integ(ctet,ref[i:i+ndar*nstp]*cref,/val)

	res = res/max(res)
	if keyword_set(sca) then pang = ctet/dar else pang = 1e6*ctet
	plot, pang, res
	pres = Join_xy(pang,res)

	return
end