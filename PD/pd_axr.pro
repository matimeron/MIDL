Pro PD_AXR, snum, verify= ver, slit= sli, back_ang= bag, left= lef, right= rig,$
	center = cnt, locate = lct, free = fre, tolerance = tol, abs_factor= abf, $
	snorm= snm, vnorm= vnm, qcrit = qcr, interactive= int, progress= prg, $
	previous= prv, cabs_factor= wabf, factors=mfac, result=res, rfresult=rfres,$
	_extra= _e

;+
; NAME:
;		PD_AXR
; VERSION:
;		8.15
; PURPOSE:
;		Patches and diplays reflectivity data from PD scans.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		PD_AXR, SNUM [, keywords])
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
;		
;		Note:	In the "far detector" mode both LOCATE and FREE are set
;				automatically in SCAN_PD_XR
;	TOLERANCE
;		Numeric scalar, value of the acceptable shift of the reflectivity peak
;		from the nominal center, in either direction, in pixels, when automatic
;		center location is used.  If not given, defaults to 3 pixels for the
;		Pilatus, 9 for the APEX (about 0.5mm).
;	ABS_FACTOR
;		Numeric scalar, the absorption factor for a single absorber foil.  If
;		provided and different from 0, this value will be used for the patching
;		of consecutive scans, else the patching is done by matching the 
;		consecutive absorption curves.
;	SNORM												|	At most one of these
;		Integer scalar, number of the direct beam scan	|	two may be used.  If
;		for normalization.								|	none is, the refl.
;	VNORM												|	curve is normalized
;		Scalar, the value to which the maximum of the	|	1 at the maximum.
;		reflectivity curve is to be normalized.			|
;
;		Note:	If VNORM is given as 0, no normalization is performed.
;	QCRIT
;		Scalar, the value of Q_critical.  If given, an additional result is 
;		generated, with reflectivity being scaled by the Fresnel reflectivity 
;		(see keyword RFRESULT).  Also, the Fresnel-scaled reflectivity is 
;		displayed to the screen.
;	/INTERACTIVE
;		Switch.  If set, PD_AXR stops at each scan, shows the frames (same as 
;		PD_XR_VIEW) and gives the user the option to drop bad frames.
;	/PROGRESS
;		Switch.  If set, the evaluation progress is printed to the screen.
;	PREVIOUS
;		A [3,L] array containing previous reflectivity data.  If given, it is 
;		combined with the currently processed data.  
;
;		Note:	No patching and/or renormalizing is done when the data is thusly
;		combined.
;	CABS_FACTOR
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	RESULT
;		Optional output, see below.
;	RFRESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Standard output is graphics only, a plot of reflectivity as a function
;		of Q_z.  Additional outputs are provided through the output parameters.
; OPTIONAL OUTPUT PARAMETERS:
; 	CABS_FACTOR
; 		Returns the calculated (from the data) absorption factor.  In order for
; 		the absorption factor to be calculated internally it is necessary that:
; 
; 		1)	An external absorption factor is not given.  If one is given, that's
; 			the value that is returned, with no calculation.
; 		2)	The data includes scans taken with different numbers of absorbers.
;	FACTORS
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scans #i is multiplied relative to scan #0.  The 0-th value is
;		always 1.
;		
;		Note:  Only groups of single-qz scans contribute to FACTORS.  If 
;		ABS_FACTOR (see above) is given, the values in FACTORS are calculated
;		from ABS_FACTOR.
;	RESULT
;		Returns the patched data in the standard [3,*] form:
;
;		Column 0	:	QZ
;		Column 1	:	Reflectivity.
;		Column 2	:	Data errors.
;	RFRESULT
;		Returns the patched data in the standard [3,*] form:
;
;		Column 0	:	QZ
;		Column 1	:	Reflectivity divided by the Fresnel reflectivity.
;		Column 2	:	Data errors.
;
;		Note:	While RESULT is always generated, RFRESULT is only generated 
;				when QCRIT is provided, else it returns !NULL.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		PD_AXR is derived from PD_XR, the difference being that SCAN_PD_XR is 
; 		used in the SUM mode, thus each scan delivers a single data point.  
; 		These data points are grouped into "super_scans" according to the 
; 		number of absorbers used, and the superscans are then patched together.
; 		If multiple-pont scans are present as well, they're patched into the
; 		previous patch.  Calls SCAN_COLUMN, SCAN_JOIN, SCAN_PATCH_SHOW, 
; 		SCAN_PD_CENTER, SCAN_PD_FRAMES, SCAN_PD_XR, SCAN_PRUNE, SCAN_RFNORM, 
; 		SCAN_SCALE, SCAN_SORT and SPEC_FILE_CHECK.  Also calls ARREQ, CAST, 
; 		DEFAULT, ISNUM, LEXISORT, LINFIT_MM, ONE_OF, RANGE_COMP and SORPURGE, 
; 		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAR-2011 by Mati Meron.
;		Modified 15-JUN-2011 by Mati Meron.  Internal changes.
;		Modified 25-JUL-2011 by Mati Meron.  Internal changes.  Added keyword
;		PREVIOUS.
;		Modified 10-DEC-2011 by Mati Meron.  Some bug fixes.  Added keyword
;		CABS_FACTOR.
;		Modified 15-DEC-2011 by Mati Meron.  Added keyword INTERACTIVE.
;		Modified 30-DEC-2011 by Mati Meron.  Internal changes, streamlining.
;		Modified 10-JUL-2012 by Mati Meron.  Added keywords QCRIT and RFRESULT.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	qtol = 1e-4

	intfl = keyword_set(int)
	prefl = 0
	psiz = size(prv)
	if psiz[0] gt 0 and psiz[1] eq 3 then begin
		wprv = prv
		if keyword_set(snm) then prefl = 1 $
		else message, 'Previous data requires SNORM', /con
	endif else if not Arreq(psiz,[0,0,0]) $
	then message, 'Invalid previous data, ignoring', /con

	Spec_file_check, snum,/pildet,par_const=['Det_th'],nsc=nsc,lis=lis,_extra=_e
	abs = fildat.scan[lis].nabso
	qvl = fltarr(nsc)
	cst = intarr(nsc)
	for i = 0l, nsc - 1 do begin
		qvl[i] = (Scan_column(lis[i],'L',/const,tol=qtol,sta=st))[0]
		cst[i] = st
	endfor

	sord = Lexisort(64-abs,qvl)
	scst = cst[sord]
	sqvl = qvl[sord]
	sabs = abs[sord]
	slis = lis[sord]
	tlis = string(slis,form='(i0)')

	afl = Isnum(abf)
	if afl then begin
		wabf = Cast(abf > 1./abf,4)
		rfac = wabf^(sabs - sabs[0])
	endif else rfac = fltarr(nsc)

	prfl = keyword_set(prg)
	nofar = (max(fildat.scan[slis].pdfar) eq 0)
	if nofar then glct = keyword_set(lct)

	a = where(scst,na,comp=b,ncomp=nb)

	if na gt 0 then begin
		dat = fltarr(3,na)
		ss = Sorpurge(64-sabs[a],/no_sort,net=nav)
		ssabs = (sabs[a])[ss]
		sfac = (rfac[a])[ss]

		mfac =  1. + fltarr(nav)
		if nofar then begin
			wcnt = Default(cnt,Scan_PD_center(slis[a[0]]))
			wlct = 1
		endif

		for i = 0l, na-1 do begin
			if prfl then print, slis[a[i]]
			sdat = Scan_PD_XR(slis[a[i]],$
			ver=ver,insp=intfl,sli=sli,back=bag,left=lef,right=rig,/sum,/glob,$
			cen=wcnt,loc=wlct,fre=fre,tol=tol,rcen=scnt,tit=stit,_extra=_e)
			dat[*,i] = sdat
			if i eq 0 and nofar then begin
				wcnt = scnt
				wlct = glct
			endif
		endfor

		for j = 0l, nav-1 do begin
			jlis = where(sabs[a] eq ssabs[j],jsc)
			next = Scan_prune(dat[*,jlis],qtol)
			if afl then next = Scan_scale(next,sfac[j])
			if j gt 0 then begin
				tres = res
				res = Scan_join(res,next,fact=fac,ext=afl,/merge,/rel,_extra=_e)
				if fac[1] gt 1 then begin
					mfac[j] = 1/fac[1]
					next = Scan_scale(next,mfac[j])
					res = Scan_join(tres,next,ext=1,/merge,/rel,_extra=_e)
				endif else mfac[j] = fac[1]
			endif else res = next
		endfor

		if not afl and nav gt 1 then begin
			wabf = exp((Linfit_mm(ssabs-ssabs[0],alog(mfac),ord=0,fac=1))[0])
			rfac = wabf^(sabs - sabs[0])
			afl = 1
		endif else mfac = sfac
	endif
	if not afl then wabf = 0.

	if nb gt 0 then begin
		if nofar then begin
			wcnt = Default(cnt,Scan_PD_center(slis[b[0]]))
			wlct = 1
		endif

		for i = 0, nb - 1 do begin
			if prfl then print, slis[b[i]]
			next = Scan_PD_XR(slis[b[i]],$
			ver=ver,insp=intfl,sli=sli,back=bag,left=lef,right=rig,/glob,$
			cen=wcnt,loc=wlct,fre=fre,tol=tol,rcen=scnt,tit=stit,_extra=_e)
			if afl then next = Scan_scale(next,rfac[b[i]])
			if i eq 0 and nofar then begin
				wcnt = scnt
				wlct = glct
			endif
			if i eq 0 and na eq 0 then res = next $
			else res = Scan_join(res,next,ext=afl,/merge,/rel,_extra=_e)
		endfor
	endif
	
	case One_of(snm,vnm) of
		0	:	begin
					if nofar then begin
						wcnt = Default(cnt,Scan_PD_center(slis[0]))
						wlct = 1
					endif
					if prfl then begin
						print
						print, long(snm)
					endif
					ndat = Scan_PD_XR($
					snm,ver=ver,slit=sli,back=bag,left=lef,right=rig,$
					/sum,/glob,cen=wcnt,loc=wlct,fre=fre,tol=tol,_extra=_e)
					mult = 1/ndat[1]
					nabs = fildat.scan[snm].nabso
					if nabs ne sabs[0] then begin
						if afl then mult = mult*wabf^(sabs[0] - nabs) else $
						message, "Absorber factor unknown, can't normalize",/con
					endif
				end
		1	:	if vnm gt 0 then mult = vnm/max(res[1,*]) else mult = 1
		else:	mult = 1./max(res[1,*])
	endcase
	res = Scan_scale(res,mult)
	if prefl then res = Scan_sort([[wprv],[res]])
	res = Scan_prune(res,qtol,pad=0)

	if nsc eq 1 then tit= stit else tit= fildat.name + ' S# ' + Range_comp(slis)
	if (size(res))[0] eq 2 then Scan_patch_show, res, /ylog, $
	xtit='Q!dz!n', ytit='Reflectivity', tit = tit, _extra= _e

	if Isnum(qcr) then begin
		rfres = Scan_rfnorm(res,qcr)
		cwin = !d.window
		window, (cwin + 1) mod 32
		Scan_patch_show, rfres, xtit='Q!dz!n', ytit='R/RF', tit=tit, _extra= _e
		wset, cwin
	endif else rfres = !null

	return
end