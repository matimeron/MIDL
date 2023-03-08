Function Limg_coo_conv, xy, $
	dist= dst, hdist= hdst, pdist= pdst, ipixsize= pxs, center= cnt,$
	alp= alp, bet= bet, dth= dth, lam= lam, ene= ene, k= kvl, radians= rad, $
	direction= dir, xreverse= xrv, qvals= qvl, qtot= qto, signed= sgn

;+
; NAME:
;		LIMG_COO_CONV
; VERSION:
;		7.02
; PURPOSE:
;		Translates from Line Detector (LD) coordinates to polar angles or
;		q-vector components.
; CATEGORY:
;		X-ray utility.
; CALLING SEQUENCE:
;		Result = LIMG_COO_CONV( XY [, keywords])
; INPUTS:
;	XY
;		Vector or scalar, coordinate (in pixel units) along the linear detector.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
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
;		LD center location (in pixel units).  The center is the location hit
;		by the direct beam when ALP= BET= DTH= 0 (see below).  Default is 0.
;	ALP
;		Numeric scalar, the angle ALPHA which the incoming beam makes with the
;		surface.  Not needed for coordinates to angles conversion, mandatory for
;		coordinates to Q-values conversion.
;	BET
;		Numeric scalar, the angle BETA which the outgoing beam makes with the
;		surface.  Mandatory.
;	DTH
;		Numeric scalar, the angle between the incoming and outgoing plane.
;		Not needed for vertical direction, mandatory for horizontal.
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
;	/RADIANS
;		Switch.  If set, the input angles are assumed to be in radians (and
;		same for the output).  The default is degrees.
;	DIRECTION
;		Character value, specifies the detector's direction.  Possible inputs
;		are "hor" and "ver" (only first letter matters).  Alternatively, a
;		numerical input may be given, 0 for horizontal, 1 for vertical.
;	/XREVERSE
;		Switch.  If set, the values of X are reversed.  To be used when the
;		x-axis points outboard (default is inboard).				|
;	/QVALS
;		Switch.  If set, the coordinates are translated to Q-values (Q_z for
;		vertical direction, Q_xy for horizontal), else they're translated to
;		spherical angles.
;	/QTOT
;		Switch.  If set, the total absolute value of Q is used, instead of Q_xy
;		or Q_z, when /QVALS is set.
;	/SIGNED
;		Switch.  If set (and /QVALS is set), the Q_xy values are given a
;		+- sign, according to the values of the output polar angle DTH (i.e.
;		phi).  If /QVALS is not set, /SIGNED has no effect.
; OUTPUTS:
;		Converts the XY input to angles or Q_values.  Result depends on the
;		setting of DIRECTION as follows:
;
;		HOR	-	Result contains DTH or (if /QVALS is set) Q_xy.
;		VER	-	Result contains BETA or (if /QVALs is set) Q_z.
;
;		If both QVALS and /QTOT are set, the result contains |Q| instead.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward geometry.  Calls QVEC.  Calls DEFAULT, ISNUM, ONE_OF,
;		STRMATCH_MM, FPU_FIX and SIGN from MIDL.
; MODIFICATION HISTORY:
;		Created 20-OCT-2007 by Mati Meron as a simplified variation of the 2D
;		IMG_COO_CONV routine.
;		Modified 15-APR-2008 by Mati Meron.  Changed keyword PIXSIZE to
;		IPIXSIZE to avoid conflicts.
;		Modified 25-APR-2008 by Mati Meron.  Streamlined and added keyword QTOT.
;		Modified 5-APR-2009 by Mati Meron.  Added pinhole transformation.
;-

	on_error, 1

	if Isnum(dir) then wdir = long(dir) $
	else wdir = Strmatch_mm(dir,['hor','ver'],1)
	if wdir lt 0 then message, 'Direction must be given!'
	xord = (1 - wdir)*keyword_set(xrv)
	qfl = keyword_set(qvl)
	tfl = keyword_set(qto)

	wha = One_of(dst,hdst,val=wdst)
	if wha eq 1 then wdst = wdst/cos(wbet) $
	else if wha eq -1 then message, 'Missing distance!'
	pdst = Default(pdst,wdst)
	if pdst le 0 then pdst = wdst
	sca = Default(pxs,1.,/dtyp)
	cnt = Default(cnt,0.,/dtyp)
	wxy = (-1)^xord*sca*(reform(xy) - cnt)

	if keyword_set(rad) then mult = 1. else mult = !dtor
	if Isnum(alp) then walp = mult*alp $
	else if qfl then message, 'Missing ALPHA!'
	wbet = mult*bet
	if Isnum(dth) then wdth = mult*dth $
	else if not wdir then message, 'Missing DTH!'
	check = [(size(walp))[0],(size(wbet))[0],(size(wdth))[0]]
	if max(check) gt 0 then message, 'Only scalar angles allowed!'

	if wdir then begin
		wx = 0
		wy = wxy
	endif else begin
		wx = wxy
		wy = 0
	endelse
	if wdst ne pdst then begin
		wdth = Default(wdth,0.)
		wy = wy - (wdst - pdst)*(wx*sin(wbet)*cos(wdth) + wy*sin(wdth))/ $
		(wdst*sin(wdth) - wx*cos(wbet)*cos(wdth))
	endif
	u = wx
	v = pdst*cos(wbet) - wy*sin(wbet)
	w = pdst*sin(wbet) + wy*cos(wbet)
	cbet = atan(w,sqrt(u^2 +v^2))

	if wdir then begin
		if qfl then res = $
		Qvec(walp,cbet,lam=lam,ene=ene,k=kvl,/rad,qz=1-tfl,qt=tfl) $
		else res = cbet/mult
	endif else begin
		cdth = (wdth - atan(u,v))
		if qfl then begin
			res = $
			Qvec(walp,cbet,dth=cdth,lam=lam,ene=ene,k=kvl,/rad,qxy=1-tfl,qt=tfl)
			if keyword_set(sgn) and not tfl then res = res*Sign(cdth)
		endif else res = cdth/mult
	endelse

	return, FPU_fix(res)
end