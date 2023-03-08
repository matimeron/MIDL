Function Img_coo_conv, x, y, reform = ref, $
	dist = dst, hdist = hdst, pdist = pdst, ipixsize = pxs, center = cnt, $
	alp = alp, bet = bet, dth = dth, tau = tau, lam = lam, ene = ene, k = kvl, $
	xreverse = xrv, qvals = qvl, signed = sgn, radians = rad, order = ord, $
	ret_dth = rdth, ret_bet = rbet

;+
; NAME:
;		IMG_COO_CONV
; VERSION:
;		7.1
; PURPOSE:
;		Translates from Area Detector (AD) coordinates to polar angles or
;		q-vector components.
; CATEGORY:
;		X-ray utility.
; CALLING SEQUENCE:
;		Result = IMG_COO_CONV( X, Y [, keywords])
; INPUTS:
;	X
;		The X (horizontal) coordinate(s) on the AD.
;	Y
;		The Y (vertical) coordinate(s) on the AD.
;
;		Note: the following options are possible:
;
;		1)	Both X and Y are scalars.
;		2)  Both X and Y are vectors (not necessarily of same length).
;		3)  Both X and Y are 2D arrays (of same dimensions).
;		4)  X alone is provided, as a 3D array.  In this case X[0,*,*] is taken
;			as X and X[1,*,*] as Y.
;
;		All other combinations are forbidden.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/REFORM
; 		Switch.  If set, the X (and Y) inputs are reformed, to eliminate 
; 		degenerate dimensions.
;	DIST														|
;		The distance from the sample to the detector.  Units	|
;		are arbitrary but must be the same as the units used	|
;		for IPIXSIZE (see below).								| One and only
;	HDIST														| one of these
;		Horizontal distance from sample to detector.  Convenient| two must be
;		in situations where it is the horizontal distance rather| defined.
;		than the total one that remains  constant.  The comment |
;		above regarding units applies here as well.				|
;	PDIST
;		The pinhole-detector distance, for the horizontal pinhole configuration.
;		The presence of PDIST forces a pinhole coordinate transformation.
;		Values <= 0 are ignored.
;	IPIXSIZE
;		Pixel size, in the units used for DIST/HDIST.  Optional.  By default it
;		is assumed that X and Y are given in pixel units and IPIXSIZE provides
;		the actual scale.  If IPIXSIZE is not given it is assumed to be 1,
;		meaning that X and Y are assumed to be in same units as DIST (or HDIST).
;	CENTER
;		AD center location (in pixel units).  The center is the location hit
;		by the direct beam when ALP= BET= DTH= 0 (see below).  Default is [0,0].
;	ALP
;		Numeric scalar, the angle ALPHA which the incoming beam makes with the
;		surface.  Not needed for coordinates to angles conversion, mandatory for
;		coordinates to Q-values conversion.
;	BET
;		Numeric scalar, the angle BETA which the outgoing beam makes with the
;		surface.  Mandatory.
;	DTH
;		Numeric scalar, the angle between the incoming and outgoing plane.
;		mandatory
;	TAU
;		Numeric scalar, the "roll" angle of the detector.  Optional, defaults
;		to zero.
;
;		Note:	All angles are assumed to be in degrees, unless /RADIANS is
;				specified.
;	LAM															|
;		The wavelength of the incoming radiation, in Angstrem.	|
;		Only required if /QVALS is set.							| One and only
;	ENE															| one of these
;		The energy of the incoming radiation, in keV.			| three must be
;		Only required if /QVALS is set.							| defined.
;	K															|
;		The K value of the in coming radiation, in inverse		|
;		Angstrem.  Only required if /QVALS is set.
;	/XREVERSE
;		Switch.  If set, the values of X are reversed.  To be used when the
;		x-axis points outboard (default is inboard).				|
;	/QVALS
;		Switch.  If set, the coordinates are translated to Q-values (Q_z and
;		Q_xy), else they're translated to spherical angles.
;	/SIGNED
;		Switch.  If set (and /QVALS is set), the Q_xy values are given a
;		+- sign, according to the values of the output polar angle DTH (i.e.
;		phi).  If /QVALS is not set, /SIGNED has no effect.
;	/RADIANS
;		Switch.  If set, the input angles are assumed to be in radians (and
;		same for the output).  The default is degrees.
;	ORDER
;		Scalar integer, overrides the setting of !ORDER.  All even values
;		translate to 0, all odd to 1.  The external value of !ORDER is
;		unaffected.
;	RET_DTH
;		Optional output, see below.
;	RET_BET
;		Optional output, see below.
; OUTPUTS:
;		Converts the X, Y coordinates to either spherical angles, [Phi,Theta]
;		or, if /QVALS is set, Q_values, [Q_xy, Q_z] and returns the conversion
;		result in a 3D array of dimensions [2,X_dim,Y_dim], such that:
;		a)	Result[0,*,*] contains the Dth (or Q_xy) values corresponding to
;			all the input points.
;		b)	Result[1,*,*] contains the Beta (or Q_z) values corresponding to
;			all the input points.
;
;		If a Q_values output is generated, its physical dimensions is A^(-1).
; OPTIONAL OUTPUT PARAMETERS:
;	RET_DTH
;		Returns a vector of the DTH angles corresponding to to the Result 
;		points along horizontal line through the CENTER.
;	RET_BET
;		Returns a vector of the BETA angles corresponding to to the Result 
;		points along vertical line through the CENTER.
;
;		Note:	These two keywords return angles even when the trasformation is
;				to Q-values.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward geometry.  Calls ARREQ, CALCTYPE, CAST, DEFAULT, ISNUM,
;		ONE_OF and SIGN from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JAN-2005 by Mati Meron.
;		Modified 5-OCT-2005 by Mati Meron.  Internal changes only.
;		Updated and renamed IMG_COO_CONV (from the previous CCD_COO_CONV) on
;		15-AUG-2007, by Mati Meron.
;		Modified 25-MAR-2008 by Mati Meron.  Added horizontal pinhole geometry
;		capability.
;		Modified 15-APR-2008 by Mati Meron.  Changed keyword PIXSIZE to
;		IPIXSIZE to avoid conflicts.
;		Modified 5-MAR-2009 by Mati Meron.  Added keywords RET_DTH and RET_BET.
;		Modified 10-AUG-2009 by Mati Meron.  Added keyword REFORM.
;-

	on_error, 1

	if Isnum(ord,/int) then word = abs(ord mod 2) else word = !order
	xord = keyword_set(xrv)
	refl = keyword_set(ref)

	if n_params() eq 0 then message, 'Missing input!'
	sx = size(x)
	if sx[0] gt 0 and refl then sx = size(reform(x))
	sy = size(y)
	if sy[0] gt 0 and refl then sy = size(reform(y))
	typ = Calctype(0.,x,y,def=4)

	case sx[0] of
		0	:	begin
					if sy[0] eq 0 and sy[1] gt 0 then begin
						wx = Cast(x,typ)
						wy = Cast(y,typ)
						res = reform(Cast(fltarr(2),typ),2,1,1)
					endif else message, 'Inconsistent X-Y inputs!'
				end
		1	:	begin
					if sy[0] eq 1 then begin
						wx = (wy = make_array(sx[1],sy[1],typ = typ))
						for j = 0l, sy[1]-1 do wx[*,j] = x
						for i = 0l, sx[1]-1 do wy[i,*] = y
						res = make_array(size=[3,size(wx)])
					endif else message, 'Inconsistent X-Y inputs!'
				end
		2	:	begin
					if Arreq(sx[0:2],sy[0:2]) then begin
						wx = Cast(reform(x),typ)
						wy = Cast(reform(y),typ)
						res = make_array(size=[3,size(wx)])
					endif else message, 'Inconsistent X-Y inputs!'
				end
		3	:	begin
					if sx[1] eq 2 and Arreq(sy,[0,0,0]) then begin
						wx = Cast(reform(x[0,*,*]),typ)
						wy = Cast(reform(x[1,*,*]),typ)
						res = Cast(make_array(size=sx),typ)
					endif else message, 'Unacceptable input!'
				end
		else:	message, 'Unacceptable input!'
	endcase

	if keyword_set(rad) then mult = 1. else mult = !dtor
	if Isnum(alp) then walp = mult*alp
	wbet = mult*bet
	wdth = mult*dth
	wtau = mult*Default(tau,0.)
	check = [(size(walp))[0],(size(wbet))[0],(size(wdth))[0],(size(wtau))[0]]
	if max(check) gt 0 then message, 'Only scalar angles allowed!'
	wha = One_of(dst,hdst,val=wdst)
	if wha eq 1 then wdst = wdst/cos(wbet) $
	else if wha eq -1 then message, 'Missing distance!'
	pdst = Default(pdst,wdst)
	if pdst le 0 then pdst = wdst
	pxs = Default(pxs,1.,/dtyp)
	cnt = Default(cnt,[0,0])
	wx = (-1)^xord*pxs*(wx - cnt[0])
	wy = (-1)^word*pxs*(wy - cnt[1])
	acnt = cnt
	dum = min(abs(wx[*,0]),mind)
	acnt[0] = mind
	dum = min(abs(wy[0,*]),mind)
	acnt[1] = mind

	if wtau ne 0 then begin
		tem = wx*cos(wtau) + wy*sin(wtau)
		wy = -wx*sin(wtau) + wy*cos(wtau)
		wx = tem
	endif
	if wdst ne pdst then wy = wy - (wdst - pdst)* $
	(wx*sin(wbet)*cos(wdth) + wy*sin(wdth))/ $
	(wdst*sin(wdth) - wx*cos(wbet)*cos(wdth))

	u = wx
	v = pdst*cos(wbet) - wy*sin(wbet)
	w = pdst*sin(wbet) + wy*cos(wbet)
	res[0,*,*] = (wdth - atan(u,v))
	res[1,*,*] = atan(w,sqrt(u^2 +v^2))

	if arg_present(rdth) then rdth = reform(res[0,*,acnt[1]])/mult
	if arg_present(rbet) then rbet = reform(res[1,acnt[0],*])/mult

	if keyword_set(qvl) then begin
		if not Isnum(walp) then message, 'Missing ALPHA!'
		opt = One_of(lam,ene,kvl,val=lval)
		case opt of
			0	:	k = 2*!pi/lval
			1	:	k = 2*!pi*lval/!srcon.conv
			2	:	k = lval
			else:	message,'Energy or Wavelength value is needed for Q values!'
		endcase
		apb = (walp + reform(res[1,*,*]))/2
		amb = (walp - reform(res[1,*,*]))/2
		phh = reform(res[0,*,*])/2

		res[0,*,*] = 2*k*sqrt((sin(apb)*sin(amb))^2 + $
					cos(apb+amb)*cos(apb-amb)*sin(phh)^2)
		if keyword_set(sgn) then res[0,*,*] = Sign(sin(phh))*res[0,*,*]
		res[1,*,*] = 2*k*sin(apb)*cos(amb)
	endif else res = res/mult

	return, res
end