Pro PD_difmap, snum, verify= ver, slit= sli, back_ang=bag, left=lef, right=rig,$
	center= cnt, locate= lct, free= fre, tolerance= tol, bottom=bot, vnorm=vnm,$
	drop= drp, progress= prg, rcenter= rcn, factors=mfac, result=res, _extra= _e

;+
; NAME:
;		PD_DIFMAP
; VERSION:
;		8.15
; PURPOSE:
;		Generates an extended map of specular and diffuse data.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		PD_DIFMAP, SNUM [, keywords])
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form recognizable by
;		RANGE_PROC.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	SLIT
;		Numerical scalar, the horizontal electronic slit dimension, in pixels.  
;		Mandatory.
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
;	BOTTOM
;		Numeric scalar, the value of the minimum Beta value to be included in 
;		the map.  If BOTTOM is lower then the minimum Beta value present, the 
;		Beta range is extended downward to reach BOTTOM.
;	VNORM
;		Scalar, the value to which the maximum of the reflectivity curve is to 
;		be normalized.			|
;		Note:	If VNORM is given as 0, no normalization is performed.
;	DROP
;		Integer scalar.  If given, the first DROP frames of every scan are 
;		dropped.
;	/PROGRESS
;		Switch.  If set, the evaluation progress is printed to the screen.
;	RCENTER
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	RESULT
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
;		[2,NFRAMES] array, where RCENTER[*,i] provides center location for scan
;		#i.
;	FACTORS
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scans #i is multiplied relative to scan #0.  The 0-th value is
;		always 1.
;	RESULT
;		Returns the patched data in the standard 3D [4,*,*] format where
;
;		Page 0		:	Alpha angle
;		Page 1		:	Beta angle
;		Page 2		:	Data, each vertical line is horizontally integrated 
;						specular-diffuse curve
;		Page 3		:	Error values.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		The specular-diffuse maps for each scan are evaluated by SCAN_PD_DIFMAP,
;		then the results are patched together using standard techniques.
;		Calls IMAGE_JOIN, SCAN_FIELD_READ, SCAN_ORDER, SCAN_PD_CENTER, 
;		SCAN_PD_DIFMAP, SCAN_PD_SHOW and SPEC_FILE_CHECK.  Also calls CAST, 
;		DEFAULT and RANGE_COMP from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron as a modification of PD_XR.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum,/pildet,par_const=['Det_th'],nsc=nsc,lis=lis,_extra=_e
	sord = Scan_order(lis,col=[2,-1,-2])
	slis = lis[sord]
	tlis = string(slis,form='(i0)')
	nofar = (max(Scan_field_read(slis,'PDfar')) eq 0)
	rfac = fltarr(nsc)
	mfac = rfac + 1
	rcn = fltarr(2,nsc)

	if nofar then begin
		glct = keyword_set(lct)
		wlct = 1
		wcnt = Default(cnt,Scan_PD_center(slis[0]))
	endif
	prfl = keyword_set(prg)
	intfl = keyword_set(int)
	if intfl then drp = 0 else drp = Default(drp,0,/dtyp) > 0
	dls = lonarr(2,nsc)
	dls[0,*] = slis
	dls[1,*] = drp
	for i = 0l, nsc-1 do begin
		if prfl then print, slis[i]
		next = Scan_PD_difmap(slis[i],ver=ver,insp=intfl,$
		slit=sli,back=bag,left=lef,right=rig,/glob,cen=wcnt,loc=wlct,fre=fre,$
		tol=tol,drop=drp,bot=Default(bot,0.),rcen=scnt,tit=stit,_extra=_e)
		rcn[*,i] = scnt
		if i eq 0 then begin
			res = next
			if nofar then begin
				wcnt = scnt
				wlct = glct
			endif
		endif else begin
			res = Img_join(res,next,/hor,fact=fac,_extra=_e)
			mfac[i] = fac[1]
		endelse
	endfor

	wvnm = Default(vnm,1.,/dtyp) > 0
	if wvnm eq 0 then mult = 1 else mult = wvnm/max(res[2,*,*])
	res[2:3,*,*] = mult*res[2:3,*,*]
	res = Cast(res,5)

	if nsc eq 1 then tit= stit else tit= fildat.name + ' S# ' + Range_comp(slis)
	Scan_PD_show, res, xtit= 'Alpha', ytit= 'Beta', tit= tit, /log, _extra = _e

	return
end