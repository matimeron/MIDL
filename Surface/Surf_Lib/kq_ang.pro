Function KQ_ang, hq, vq, alp = alp, lam = lam, energy = ene, k = kvl, $
	radians = rad, horizontal = hor, pack = pac

;+
; NAME:
;		KQ_ANG
; VERSION:
;		8.47
; PURPOSE:
;		Calculating the angle(s) between the K-vector and the Q-vector.
; CATEGORY:
;		Scattering specific.
; CALLING SEQUENCE:
;		Result = KQ_ANG ( HQ, VQ {,LAM= LAM, ENERGY= ENE, K= KVL} $
;				[, RADIANS = RAD] [, HORIZONTAL = HOR] [,PACK = PAC])
; INPUTS:
;	HQ
;		The value(s) of the horizontal (inplane) part of the Q-vector.
;	VQ
;		The value(s) of the vertical (Z) component of the Q-vector.
;
;		Note:	A number of input schemes are available, as follows:
;
;			1)	Only HQ given.  In this case HQ needs to be a [3,N,M] array
;				with M >= 2.  The HQ[0,*,*] part is used as HQ internally
;				while HQ[1,*,*] is used as VQ.
;			2)	Both inputs provided, as scalars.  Used as is.
;			3)	HQ provided as vector and VQ as scalar, or vice versa.  The
;				scalar input will be extended to a vector of same length as the
;				other input, with all components equal to the scalar value.
;			4)	HQ a vector of length M, VQ a vector of length N.  Both vectors
;				will be extended internally to an array of dimensions [M,N].
;			5)	HQ and VQ provided as 2D arrays.  Both arrays have to have same
;				dimensions.
;
;			Any other combination is unacceptable.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	ALP
; 		The Alpha angle of the incoming K-vector, measured relative to the
; 		horizontal direction.  K is assumed to be |K|*[0,cos(ALP),-sin(ALP)].
; 		Alpha is given in degrees, unless RADIANS is set.
;	LAM														|
;		Scalar, the value of the wavelength in Angstrem.	| One and only
;	ENE														| one of these
;		Scalar, the energy value in keV.					| three must be
;	K														| defined.
;		Scalar, the K value in inverse Angstrem.			|
;	/RADIANS
;		Switch.  If set, the the input ALP is assumed to be in radians, and the
;		output is in radians.  Default is degrees.
;	/HORIZONTAL
;		Switch.  If set, the angle between the horizontal projections of K and Q
;		is evaluated.  Default is the angle between the vectors K and Q.
;	/PACK
;		Switch.  If set, the result is packed together with the HQ and VQ
;		values to an [3,M,N] array.  Active for input options 1,4 and 5 above,
;		ignored for options 2 and 3.
; OUTPUTS:
;		Returns the angle between the K and Q vectors, or between their
;		horizontal projections if /HORIZONTAL is set.  The format of the result
;		depends on the input option used (see NOTE following INPUTS, above), as
;		follows:
;			1)	An [N,M] array.
;			2)	A scalar.
;			3)  A vector of length equal to this of the vector input.
;			4)  An [M,N] array, where M and N are the lengths of HQ and Qz,
;				respectively.
;			5)	An [M,N] array, where [M,N] are the common dimensions of HQ and
;				VQ.
;
;		However, in cases 1, 4, 5, the result is a [4,M,N] array when /PACK is
;		set.  In this case Result[0,*,*] contains the HQ values, Result[1,*,*],
;		the VQ values, and Result[2,*,*] contains the actual calculated angles.
;		Result [4,*,*] does nothing (contains zeroes), it is present for
;		compatibility with the various PD routines.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The Q values must be physically possible.
; PROCEDURE:
;		Straightforward, from definition.  Calls INPUT_REFORM and K_GEN, from
;		SURF_LIB.  Calls CALCTYPE, CAST,FLTROUND and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2016 by Mati Meron.
;-

	on_error, 1
	eps = Toler()

	typ = Calctype(hq,vq,0.,def=4)
	Input_reform, hq, vq, whq=whq, wvq=wvq, type=5, dim=dim

	if keyword_set(rad) then mult = 1d else mult = !dpi/180d
	walp = mult*alp
	if size(walp,/dim) ne 0 then message, 'ALPHA must be a scalar!'

	k = K_gen(lam=lam, energy=ene, k=kvl, typ=5,/sing)

	arg = 0d*whq
	if keyword_set(hor) then begin
		kxy = k*abs(cos(walp))
		kz = -k*sin(walp)
		dum = where(abs(whq) gt eps,ndum)
		if ndum gt 0 then arg[dum] = $
		-(whq[dum]^2 + wvq[dum]^2 + 2*kz*wvq[dum])/(2*kxy*whq[dum])
	endif else begin
		qtot = sqrt(whq^2 + wvq^2)
		dum = where(abs(qtot) gt eps,ndum)
		if ndum gt 0 then arg[dum] = -qtot[dum]/(2*k)
	endelse
	if typ lt 5 then arg = Fltround(arg,dec_dig=7)
	if ndum gt 0 then begin
		check = max(abs(arg))
		if check le 1 + 4*eps then arg = -1 > arg < 1 $
		else message, 'Unacceptable values!'
	endif

	if keyword_set(pac) and n_elements(dim) eq 2 then begin
		res = make_array([4,dim],typ=5)
		res[0,*,*] = whq
		res[1,*,*] = wvq
		res[2,*,*] = acos(arg)/mult
	endif else res = acos(arg)/mult

	return, Cast(res,typ,typ,/fix)
end