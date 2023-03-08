Pro PD_XR_match, fir, sec, slit = sli, back_ang = bag, left= lef, right= rig, $
	center= cnt, locate= lct, free= fre, tolerance= tol, title= tit, zoom= zom,$
	rcenter = rcn, factors = mfac, _extra = _e

;+
; NAME:
;		PD_XR_MATCH
; VERSION:
;		8.01
; PURPOSE:
;		Checks matching of PD reflectivity scans.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		Result = PD_XR_MATCH( FIR, SEC [, keywords])
; INPUTS:
;	FIR, SEC
;		Either both FIR and SEC are individual scan numbers or FIR is a list of
;		two scan numbers with SEC being undefined.  In either case the list of 
;		scans must include exactly two scans.
;		
;		Alternatively, FIR and SEC may contain actual scan data, i.e. [3,n] 
;		arrays.  In this case all the keywords with the exception of ZOOM and 
;		FACTORS (see below) are inactive. 
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SLIT
;		A 2 element vector specifying electronic slit dimensions, in pixels, in
;		[horizontal,vertical] order.
;	BACK_ANG
;		Numeric scalar, the "sideways" offset angle used to evaluate background,
;		in degrees.  If not given, no background subtraction is performed.
;	/LEFT
;		Switch.  Specifies taking left sided background.
;	/RIGHT
;		Switch.  Specifies taking right sided background.
;
;		Note:	By default, double sided background is used.  Therefore, setting
;				both LEFT and RIGHT is same as not setting both.
;	CENTER
;		Location of the center of the reflectivity peak, in pixels.  Provided
;		as a 2-element vector ([xy] order.  If not given, the center will be
;		determined from the first scan in SNUM.
;	/LOCATE
;		Switch,  If set, the center is located automatically.  If the center
;		such located is beyond the approved range around the nominal center (see
;		TOLERANCE), it is rejected and the nominal center is used instead.
;	/FREE
;		Switch.  If set and if LOCATE is set, PD_XR is free to use an individual
;		center for each frame within each scan, as located.  Default (when
;		LOCATE is set) is to use the mean center of the evaluated frames within
;		each scan.
;	TOLERANCE
;		Numeric scalar, value of the acceptable shift of the reflectivity peak
;		from the nominal center, in either direction, in pixels, when automatic
;		center location is used.  If not given, defaults to 3 pixels
;		(about 0.5mm).
;	TITLE
;		An optional title to be used with the plot.  By default a title is 
;		generated internally.
;	/ZOOM
;		Switch.  If set, only the overlap region of the two scans is displayed.
;	RCENTER
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Standard output is graphics only, a plot of reflectivity as a function
;		of Q_z.  Additional outputs are provided through the output parameters.
; OPTIONAL OUTPUT PARAMETERS:
;	RCENTER
;		Returns the locations (in pixels) used for the reflectivity peak, as a
;		[2,2] array, where RCENTER[*,i] provides center location for scan
;		#i.
;	FACTOR
;		Scalar returning the multiplicative factor of the match.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		The reflectivity values for each scan are evaluated by SCAN_PD_XR, then
;		the spatching sonstant is determined using standard techniques.
;		Calls SCAN_FIELD_READ, SCAN_JOIN, SCAN_ORDER, SCAN_PD_CENTER, 
;		SCAN_PD_XR, SCAN_SCALE, SCAN_SHOW, SCAN_VER and SPEC_FILE_CHECK.  
;		Also calls ARRPACK, DEFAULT, ISNUM and SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAR-2009 by Mati Meron.
;		Modified 30-MAR-2009 by Mati Meron.  Internal changes.
;		Modified 10-NOV-2009 by Mati Meron.  Internal changes, added processed
;		scans input capability.  Also added keyword TITLE.
;		Modified 5-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 20-FEB-2011 by Mati Meron.  Added far detector support.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if Scan_ver(fir) and Scan_ver(sec) then begin
		rfir = fir
		rsec = sec
		wtit = Default(tit,'')
	endif else begin
		lis = long(Arrpack(fir,sec))
		Spec_file_check, lis,/pildet,par_const=['Det_th'],nsc=nsc,_extra=_e
		if nsc ne 2 then message, '2 and only 2 scans allowed!'
		sord = Scan_order(lis,col=[2,-1,-2])
		slis = lis[sord]
		rcn = fltarr(2,nsc)
		if max(Scan_field_read(slis,'PDfar')) eq 0 then begin
			if Isnum(cnt) then begin
				wcnt = cnt
				flct = 0
			endif else begin
				wcnt = Scan_PD_center(slis[0])
				flct = 1
			endelse
			slct = keyword_set(lct)
		endif
	
		rfir = Scan_PD_XR(slis[0],slit=sli,back=bag,left=lef,right=rig,$
			cen=wcnt,loc=flct,free=fre,tol=tol,rcen=scnt,_extra=_e)
		rcn[*,0] = scnt
		rsec = Scan_PD_XR(slis[1],slit=sli,back=bag,left=lef,right=rig,$
			cen=wcnt,loc=slct,free=fre,tol=tol,rcen=scnt,_extra=_e)
		rcn[*,1] = scnt

		snums = string(slis,form='(i0)')
		wtit = fildat.name+ '!c'+ 'First: '+ snums[0]+ ' ; Second: '+ snums[1]
	endelse
	dum = Scan_join(rfir,rsec,fact=fac,_extra=_e)
	mfac = fac[1]

	if keyword_set(zom) then begin
		nf = Split_xy(rfir,x=xf,y=yf)
		ns = Split_xy(rsec,x=xs,y=ys)
		dumf = where(xf ge xs[0], ndumf)
		if ndumf le 1 then dumf = nf + [-1,-2] else dumf = [dumf,nf] - 1
		dums = where(xs le xf[nf-1], ndums)
		if ndums le 1 then dums = [0,1] else dums = [0,dums+1]
		rfir = rfir[*,dumf]
		rsec = rsec[*,dums]
	endif

	stit = string(mfac,form='("mult. factor = ",f9.6)')
	xtit = 'Q!dz!n'
	Scan_show, rfir, Scan_scale(rsec,mfac), lcol = [!pcol.pink,!pcol.cyan], $
	tit=wtit,subtit=stit,xtit=xtit, call = 3, ymargin=[6,4],_extra = _e

	return
end