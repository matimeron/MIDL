Pro PD_BXR, snum, verify= ver, slit= sli, back_ang= bag, left= lef, right= rig,$
	center = cnt, locate = lct, free = fre, tolerance = tol, $
	abs_factor = abf, snorm = snm, vnorm = vnm, progress = prg, $
	rcenter = rcn, factors = mfac, result = res, _extra = _e

;+
; NAME:
;		PD_BXR
; VERSION:
;		8.01
; PURPOSE:
;		Patches and diplays reflectivity data from PD scans.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		PD_BXR, SNUM [, keywords])
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
;	TOLERANCE
;		Numeric scalar, value of the acceptable shift of the reflectivity peak
;		from the nominal center, in either direction, in pixels, when automatic
;		center location is used.  If not given, defaults to 3 pixels
;		(about 0.5mm).
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
;		Returns the patched data in the standard [3,*] form:
;
;		Column 0	:	QZ
;		Column 1	:	Reflectivity.
;		Column 2	:	Data errors.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		PD_XRA is derived from PD_XR, the difference being that SCAN_PD_XR is 
; 		used in the SUM mode, thus each scan delivers a single data point.  
; 		These data points are grouped into "super_scans" according to the 
; 		number of absorbers used, and the superscans are then patched together.
;		Calls SCAN_COLUMN, SCAN_JOIN, SCAN_PATCH_SHOW, SCAN_PD_CENTER, 
;		SCAN_PD_XR, SCAN_SCALE and SPEC_FILE_CHECK.  Also calls CAST, DEFAULT, 
;		DIF, ISNUM, LEXISORT, LINFIT_MM, ONE_OF, RANGE_COMP and SORPURGE,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAR-2011 by Mati Meron.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	qtol = 1e-3

	Spec_file_check, snum,/pildet,par_const=['Det_th'],nsc=nsc,lis=lis,_extra=_e
	abs = fildat.scan[lis].nabso
	qvl = fltarr(nsc)
	cst = intarr(nsc)
	rcn = fltarr(2,nsc)
	for i = 0l, nsc - 1 do begin
		qvl[i] = (Scan_column(lis[i],'L',/const,tol=qtol,sta=st))[0]
		cst[i] = st
	endfor
	sord = Lexisort(64-abs,qvl)
	sabs = abs[sord]
	sqvl = qvl[sord]
	scst = cst[sord]
	slis = lis[sord]
	tlis = string(slis,form='(i0)')

	a = where(scst,na,comp=b,ncomp=nb)
	if na eq 0 then message, 'No single qz scans, use PD_XR instead!'
	dat = fltarr(3,na)
	ss = Sorpurge(64-sabs[a],/no_sort,net=nav)
	ssabs = (sabs[a])[ss]

	mfac =  1. + fltarr(nav)
	if Isnum(abf) then begin
		wabf = Cast(abf > 1./abf,4)
		rfac = wabf^(ssabs - ssabs[0])
	endif else rfac = fltarr(nav)

	nofar = (max(fildat.scan[slis].pdfar) eq 0)
	if nofar then begin
		glct = keyword_set(lct)
		wlct = 1
		wcnt = Default(cnt,Scan_PD_center(slis[0]))
	endif

	prfl = keyword_set(prg)
	for i = 0l, na-1 do begin
		if prfl then print, slis[a[i]]
		sdat = $
		Scan_PD_XR(slis[a[i]],ver=ver,sli=sli,back=bag,left=lef,right=rig,/sum,$
		/glob,cen=wcnt,loc=wlct,fre=fre,tol=tol,rcen=scnt,tit=stit,_extra=_e)
		dat[*,i] = sdat
		rcn[*,a[i]] = scnt
		if i eq 0 and nofar then begin
			wcnt = scnt
			wlct = glct
		endif
	endfor

	for j = 0, nav-1 do begin
		jlis = where(sabs[a] eq ssabs[j],jsc)
		next = dat[*,jlis]
		repeat begin
			if jsc eq 1 then break
			check = Dif(next[0,*],/edge)
			lc = (where(check[1:*] le qtol, ndum))[0]
			if ndum gt 0 then begin
				top = (next[1,lc]/next[2,lc])^2 + (next[1,lc+1]/next[2,lc+1])^2
				bot = next[1,lc]/(next[2,lc])^2 + next[1,lc+1]/(next[2,lc+1])^2
				next[1,lc] = top/bot
				next[2,lc] = sqrt(top)/bot
				jsc = jsc - 1
				if lc lt jsc-1 then begin
					next[*,lc+1:jsc-1] = next[*,lc+2:jsc]
					done = 0
				endif else done = 1
				next = next[*,0:jsc-1]
			endif else done = 1
		endrep until done
		if jsc eq 1 then begin
			onext = next
			next = fltarr(3,2)
			next[0,*] = onext[0] + [0,qtol/2]
			next[1,*] = onext[1]
			next[2,*] = onext[2]*sqrt(2)
		endif		
		if j gt 0 then begin
			nr = (size(res))[2] - 1
			rev = abs(res[0,nr] - res[0,nr-1]) gt abs(next[0,1] - next[0,0])
			res = Scan_join(res,next,fact=fac,rev=rev,ext=rfac[j],_extra=_e)
			mfac[j] = fac[1]
		endif else res = next
	endfor

	if nofar then begin
		glct = keyword_set(lct)
		wlct = 1
		wcnt = Default(cnt,Scan_PD_center(slis[0]))
	endif

	for i = 0, nb - 1 do begin
		if prfl then print, slis[b[i]]
		next = $
		Scan_PD_XR(slis[b[i]],ver=ver,sli=sli,back=bag,left=lef,right=rig,$
		/glob,cen=wcnt,loc=wlct,fre=fre,tol=tol,rcen=scnt,_extra=_e)
		rcn[*,b[i]] = scnt
		if i eq 0 and nofar then begin
			wcnt = scnt
			wlct = glct
		endif
		res = Scan_join(res,next,fact=fac,/merge,_extra=_e)
	endfor
	rord = sort(sord)
	rcn = rcn[*,rord]

	nrs = n_elements(res)
	repeat begin
		if nrs eq 1 then break
		check = Dif(res[0,*],/edge)
		lc = (where(check[1:*] le qtol, ndum))[0]
		if ndum gt 0 then begin
			top = (res[1,lc]/res[2,lc])^2 + (res[1,lc+1]/res[2,lc+1])^2
			bot = res[1,lc]/(res[2,lc])^2 + res[1,lc+1]/(res[2,lc+1])^2
			res[1,lc] = top/bot
			res[2,lc] = sqrt(top)/bot
			nrs = nrs - 1
			if lc lt nrs-1 then begin
				res[*,lc+1:nrs-1] = res[*,lc+2:nrs]
				done = 0
			endif else done = 1
			res = res[*,0:nrs-1]
		endif else done = 1
	endrep until done

	case One_of(snm,vnm) of
		0	:	begin
					ndat = Scan_PD_XR($
					snm,ver=ver,slit=sli,back=bag,left=lef,right=rig,$
					/sum,/glob,cen=wcnt,loc=glct,fre=fre,tol=tol,_extra=_e)
					mult = 1/ndat[1]
					nabs = fildat.scan[snm].nabso
					if nabs ne sabs[0] then begin
						if not Isnum(wabf) then wabf = exp($
						(Linfit_mm(ssabs-ssabs[0],alog(mfac),ord=0,fac=1))[0])
						mult = mult*wabf^(sabs[0] - nabs)
					endif
				end
		1	:	if vnm gt 0 then mult = vnm/max(res[1,*]) else mult = 1
		else:	mult = 1./max(res[1,*])
	endcase
	res = Scan_scale(res,mult)

	if nsc eq 1 then tit= stit else tit= fildat.name + ' S# ' + Range_comp(slis)
	Scan_patch_show, res, /ylog, xtit='Q!dz!n', ytit='Reflectivity', $
	tit = tit, _extra= _e

	return
end