Pro PD_XR, snum, verify = ver, last = lst, abs_factor = abf, progress = prg, $
	slit = sli, back_ang = bag, left = lef, right = rig, $
	center = cnt, locate = lct, free = fre, tolerance = tol, $
	qstep = qst, drop = drp, interactive = int, snorm = snm, vnorm = vnm, $
	show_patches = shp, qcrit = qcr, qloc = qlc, rvrange = rvr, $
	raw = raw, hrange = hrn, vrange = vrn, $
	rcenter=rcn, factors=mfac, dlist=dls, result= res, rfresult=rfres, _extra=_e

;+
; NAME:
;		PD_XR
; VERSION:
;		8.48
; PURPOSE:
;		Patches and diplays reflectivity data from PD scans.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		PD_XR, SNUM [, keywords])
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
;	/LAST
;		Switch.  If set, last processed data is reused.
;		
;		Note:	When LAST is set, all the keywords with the exception of QCRIT,
;				RAW, HRANGE and VRANGE (see below) are disabled.
;	ABS_FACTOR
;		Numeric scalar, the absorption factor for a single absorber foil.  If
;		provided and different from 0, this value will be used for the patching
;		of consecutive scans, else the patching is done by matching the 
;		consecutive absorption curves.
;	/PROGRESS
;		Switch.  If set, the evaluation progress is printed to the screen.
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
;	QSTEP
;		Scalar specifying the minimal Qz distance to be kept between consecutive
;		data points.  Excess points are eliminated, with the exception of 
;		"special points", corresponding to locations of high curvature (see the
;		routine SCAN_TRIM for details).  If QSTEP is not provided, no data 
;		trimming takes place.
;	DROP
;		Integer scalar.  If given, then if positive the first DROP frames of 
;		every scan are dropped, else the last DROP frames of each scan are 
;		dropped.
;	/INTERACTIVE
;		Switch.  If set, frame dropping is performed interactively, scan by 
;		scan.  In this mode, if the value provided in response to "Drop how 
;		many?" is > 0 then it is taken as the number of points, counting from
;		the beginning of the scan, to drop.  If it is <= 0 then its absolute
;		value is taken as the number of points, counting from the end of the 
;		scan, to drop.
;	SNORM												|	At most one of these
;		Integer scalar, number of the direct beam scan	|	two may be used.  If
;		for normalization.								|	none is, the refl.
;	VNORM												|	curve is normalized
;		Scalar, the value to which the maximum of the	|	1 at the maximum.
;		reflectivity curve is to be normalized.			|
;
;		Note:	If VNORM is given as 0, no normalization is performed.
;	/SHOW_PATCHES
;		Switch.  If not set explicitly to 0, the patch (overlap) regions are 
;		marked  on the plot in a different color.
;	QCRIT
;		Scalar, the value of Q_critical.  If given, an additional result is 
;		generated, with reflectivity being scaled by the Fresnel reflectivity 
;		(see keyword RFRESULT).  Also, the Fresnel-scaled reflectivity is 
;		displayed to the screen.
;	QLOC
;		2-element vector.  If given, PD_XR will attempt to find a critical Q
;		value, within the range defined by QLOC, best fitting the data.  If
;		QCRIT is also provided the data will be Q-shifted by the difference
;		between QCRIT and the fitted value, prior to the Fresnel scaling, else
;		the fitted value will be used as QCRIT.
;		The fitted value will also be displayed to the screen.
;	RVRANGE
;		Same as IDL YRANGE, but applying only to the plot Fresnel-scaled plot.
;	/RAW
;		Switch.  If set, an additional plot is displayed, showing separately 
;		the "raw" (no background subtraction) reflectivity data and the 
;		background.
;	HRANGE
;		Same as IDL XRANGE, but applying only to the plot displayed by /RAW.
;	VRANGE
;		Same as IDL YRANGE, but applying only to the plot displayed by /RAW.
;	RCENTER
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	DLIST
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
;	RCENTER
;		Returns the locations (in pixels) used for the reflectivity peak, as a
;		[2,NFRAMES] array, where RCENTER[*,i] provides center location for scan
;		#i.
;	FACTORS
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scans #i is multiplied relative to scan #0.  The 0-th value is
;		always 1.
;	DLIST
;		Returns a [2,*] array, where the first column contains a list of the
;		scan numbers and the second one contains the corresponding DROP values
;		(see DROP and INTERACTIVE, above).
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
;		PD_XR_KEEP.  Includes the following:
;			EEXS	- Flag, set when the data in the common block is defined.
;			LRES	- Last result (i.e. processed data).
;			LTIT	- Last title.
;			LBAGFL	- Flag, set to 1 if background data was defined for last
;					  result.
;			LRAWRES	- Last raw (i.e. no background subtraction) data.
;			LBACRES	- Last background data. 
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		The reflectivity values for each scan are evaluated by SCAN_PD_XR, then
;		the scans are patched together using standard techniques.
;		Calls SCAN_COLUMN, SCAN_FIELD_READ, SCAN_JOIN, SCAN_ORDER, SCAN_OFFSET, 
;		SCAN_PATCH_SHOW, SCAN_PD_CENTER, SCAN_PD_XR, SCAN_RFNORM, SCAN_SCALE,
;		SCAN_SHOW, SCAN_TRIM and SPEC_FILE_CHECK.  Calls PD_XR_MATCH.  Calls
;		QC_OPT from SURFACE.  Also calls CAST, DEFAULT, ISNUM, ONE_OF, 
;		RANGE_COMP, STRMATCH_MM, TOLER and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUN-2008 by Mati Meron.
;		Modified 1-AUG-2008 by Mati Meron.  Internal changes.  Added keywords
;		CENTER, TOLERANCE and RCENTER.  Removed keywords ACC_SHIFT and ABSOLUTE.
;		Modified 10-AUG-2008 by Mati Meron.  Replaced keyword RNORM with SNORM
;		and VNORM.
;		Modified 10-NOV-2008 by Mati Meron.  Added keywords LEFT, RIGHT and
;		FREE.  Modified peak location and streamlined operation.
;		Modified 5-MAR-2009 by Mati Meron.  Added keyword PROGRESS.
;		Modified 30-MAR-2009 by Mati Meron.  Added keyword VERIFY.
;		Modified 25-OCT-2009 by Mati Meron.  Internal changes.
;		Modified 15-NOV-2009 by Mati Meron.  Added INTERACTIVE mode, DROP frame
;		capability and DLIST, the list of dropped frames.
;		Modified 5-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 10-APR-2010 by Mati Meron.  Added keyword SHOW_PATCHES.
;		Modified 25-APR-2010 by Mati Meron.  Provided single point drop option
;		through /INTERACTIVE.  Made SHOW_PATCHES the default option.  Provided
;		automatic reversal of patching direction on reversal of point density.
;		Added keyword ABS_FACTOR.
;		Modified 20-FEB-2011 by Mati Meron.  Added far detector support.
;		Modified 10-JUL-2012 by Mati Meron.  Added keywords QSTEP, QCRIT and
;		RFRESULT.
;		Modified 15-AUG-2012 by Mati Meron.  Added the capacity to drop points
;		either from the beginning or the end of each scan.
;		Modified 5-OCT-2012 by Mati Meron.  Added keyword RAW.
;		Modified 10-OCT-2012 by Mati Meron.  Added keywords LAST, HRANGE and
;		VRANGE.  Activated keeping last results for reuse.
;		Modified 30-MAR-2013 by Mati Meron.  Added keyword RVRANGE.
;		Modified 15-JUN-2014 by Mati Meron.  Internal changes.
;		Modified 20-OCT-2014 by Mati Meron.  Internal changes.
;		Modified 25-MAY-2015 by Mati Meron.  Added the option of fitting the
;		data to find QCRIT.
;		Modified 20-FEB-2017 by Mati Meron.  Added keyword QLOC to streamline
;		QCRIT fitting.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common pd_xr_keep, eexs, lres, ltit, lbagfl, lrawres, lbacres
	on_error, 1
	posib = [' ','y','n','q']

	if keyword_set(lst) then begin
		if Isnum(eexs) then begin
			res = lres
			tit = ltit
			if lbagfl then begin
				bagfl = lbagfl
				rawres = lrawres
				bacres = lbacres
			endif
			shp = 0
		endif else message, 'There is no previous data!'
	endif else begin
		Spec_file_check, snum,/pil,par_con=['Det_th'],nsc=nsc,lis=lis,_extra=_e
		sord = Scan_order(lis,col=[2,-1,-2])
		slis = lis[sord]
		tlis = string(slis,form='(i0)')
		nofar = (max(Scan_field_read(slis,'PDfar')) eq 0)
		rfac = fltarr(nsc)
		mfac = rfac + 1
		rcn = fltarr(2,nsc)
	
		if Isnum(abf) then begin
			wabf = Cast(abf > 1./abf,4)
			nab = fildat.scan[slis].nabso
			rfac = wabf^(nab - nab[0])
			abfl = 1
		endif else abfl = 0
	
		if nofar then begin
			glct = keyword_set(lct)
			wlct = 1
			wcnt = Default(cnt,Scan_PD_center(slis[0]))
		endif

		prfl = keyword_set(prg)
		qstfl = keyword_set(qst)	
		intfl = keyword_set(int)
		bagfl = keyword_set(bag)
		if intfl then drp = 0 else drp = Default(drp,0,/dtyp)
		wdrp = replicate(drp,nsc)
		dls = lonarr(3,nsc)
		dls[0,*] = slis
		if nsc gt 1 then begin
			if drp gt 0 then begin
				wdrp[0] = 0
				dls[1,1:-1] = drp
			endif else begin
				wdrp[-1] = 0
				dls[2,0:-2] = -drp
			endelse
		endif
	
		for i = 0l, nsc-1 do begin
			if prfl then print, slis[i]
			next = Scan_PD_XR(slis[i],ver=ver,slit=sli,back=bag,$
			left=lef,right=rig,/glob,cen=wcnt,loc=wlct,fre=fre,tol=tol,$
			drop=wdrp[i],rdat=nraw,bdat=nbac,rcen=scnt,tit=stit,_extra=_e)
			rcn[*,i] = scnt
			if qstfl then next = Scan_trim(next,xstep=qst,/spec,_extra=_e)
			if i eq 0 then begin
				res = next
				if bagfl then begin
					rawres = nraw
					bacres = nbac
				endif
				if nofar then begin
					wcnt = scnt
					wlct = glct
				endif
			endif else begin
				if intfl then begin
					tres = res
					tnext = next
					okfl =  0
					que = ''
					idrop = 0
					repeat begin
						PD_XR_match,tres,tnext,psym =-4,/zoom, tit=fildat.name $
						+ '!c'+ 'First: '+ tlis[i-1]+ ' ; Second: '+ tlis[i]
						wait, 0.001
						read, que, prompt = 'OK (Y/N/Q)?  '
						case Strmatch_mm(que,posib,1) of
							-1	:
							0	:	okfl = 1
							1	:	okfl = 1
							2	:	begin
										read, idrop, prompt = 'Drop how many?  ' 
										if Isnum(idrop) then begin
											if idrop le 0 then begin
												tres = res[*,0:idrop-1]
												dls[2,i-1] = -idrop
											endif else begin
												tnext = next[*,idrop:*]
												dls[1,i] = idrop
											endelse
										endif else message,'Invalid input!',/con
									end
							3	:	begin
										intfl = 0
										okfl = 1
									end
						endcase
					endrep until okfl
					res = tres
					next = tnext
				endif
				nr = (size(res))[2] - 1
				rev = abs(res[0,nr] - res[0,nr-1]) gt abs(next[0,1] - next[0,0])
				res = Scan_join(res,next,$
				force=frc,fact=fac,rev=rev,ext=rfac[i],_extra=_e)
				mfac[i] = fac[1]
				if bagfl then begin
					rawres = Scan_join(rawres,nraw,$
					force=frc,rev=rev,ext=rfac[i],_extra=_e)
					nbac[1,*] = nbac[1,*] > Toler()*nraw[1,*]
					bacres = Scan_join(bacres,nbac,$
					force=frc,rev=rev,ext=rfac[i],_extra=_e)
				endif
			endelse
		endfor
	
		case One_of(snm,vnm) of
			0	:	begin
						drb = Scan_PD_XR($
						snm,slit=sli,back=bag,cen=wcnt,loc=lct,/glob,_extra=_e)
						mult = fildat.scan[snm].ncr[1]/total(drb[1,*])
						if abfl then begin
							nnab = fildat.scan[[snm,slis[0]]].nabso
							mult = mult*wabf^(nnab[1]-nnab[0])
						endif
					end
			1	:	if vnm gt 0 then mult = vnm/max(res[1,*]) else mult = 1
			else:	mult = 1./max(res[1,*])
		endcase
		res = Scan_scale(res,mult)
		if bagfl then begin
			rawres = Scan_scale(rawres,mult)
			bacres = Scan_scale(bacres,mult)
			llen = (size(nraw))[2]
			loff = total(nraw[1,*] - nbac[1,*])*mfac[-1]*mult/llen
			roff = total(rawres[1,-llen:-1] - bacres[1,-llen:-1])/llen
			bacres = Scan_offset(bacres,yoff=roff-loff)
		endif
	
		if nsc eq 1 then tit=stit else tit=fildat.name+ ' S# '+ Range_comp(slis)

		eexs = 1
		lres = res
		ltit = tit
		lbagfl = bagfl
		if lbagfl then begin
			lrawres = rawres
			lbacres = bacres
		endif
	endelse

	Scan_patch_show, res, /ylog, xtit='Q!dz!n', ytit='Reflectivity', $
	tit = tit, _extra= _e

	if Default(shp,1) ne 0 then begin
		resq = reform(res[0,*])
		resd = reform(res[1,*])
		for i = 1, nsc-1 do begin
			loq = Scan_column(slis[i-1],'l')
			hiq = Scan_column(slis[i],'l')
			ovq = [min(hiq),max(loq)]
			dum = where(resq ge ovq[0] and resq le ovq[1], ndum)
			if ndum gt 0 then $
			oplot, resq[dum], resd[dum], col = !pcol.red, _extra = _e
		endfor
	endif 

	if Isnum(qcr) or Isnum(qlc) then begin
		qshf = 0
		if n_elements(qlc) eq 2 then begin
			oqcr = Qc_opt(res,qcr,sta=qsta)
			if qsta then com = '' else com = ' (Unreliable, no convergence)'
			print
			print, '	Fitted value of Qc = ', oqcr, com
			if Isnum(qcr) and qsta then begin
				qshf = qcr - oqcr
				res = Scan_offset(res,xoff=qshf)
				print, '	Data Qc shifted by = ', qshf
				wqcr = qcr
			endif else wqcr = oqcr
			print
		endif else wqcr = qcr
		rfres = Scan_rfnorm(res,wqcr)
		cwin = !d.window
		yrcheck = (Wherinstruct('yran',_e))[0]
		if yrcheck ge 0 then begin
			eyran = _e.(yrcheck)
			_e.(yrcheck) = [min(rfres[1,*],max=max),max]
		endif
		window, (cwin + 1) mod 32
		Scan_patch_show, rfres, xtit='Q!dz!n', ytit='R/RF', tit=tit, $
			yran=rvr, ystyle=Isnum(rvr), _extra= _e
		if Default(shp,1) ne 0 then begin
			resq = reform(rfres[0,*])
			resd = reform(rfres[1,*])
			for i = 1, nsc-1 do begin
				loq = Scan_column(slis[i-1],'l')
				hiq = Scan_column(slis[i],'l')
				ovq = [min(hiq),max(loq)]
				dum = where(resq ge ovq[0] and resq le ovq[1], ndum)
				if ndum gt 0 then $
				oplot, resq[dum], resd[dum], col = !pcol.red, _extra = _e
			endfor
		endif
		if yrcheck ge 0 then _e.(yrcheck) = eyran
		wset, cwin
	endif else rfres = !null

	if bagfl and keyword_set(raw) then begin
		cwin = !d.window
		xrcheck = (Wherinstruct('xran',_e))[0]
		if xrcheck ge 0 then begin
			exran = _e.(xrcheck)
			_e.(xrcheck) = [0,0]
		endif
		yrcheck = (Wherinstruct('yran',_e))[0]
		if yrcheck ge 0 then begin
			eyran = _e.(yrcheck)
			_e.(yrcheck) = [0,0]
		endif
		window, (cwin + 1 + Isnum(wqcr)) mod 32
		Scan_show,rawres,bacres,/ylo,tit=tit,xtit='Q!dz!n',ytit='Reflectivity',$
		xran= hrn, yran= vrn, lcol= [!pcol.red,!pcol.blue],line= [0,2],_extra=_e
		if xrcheck ge 0 then _e.(xrcheck) = exran
		if yrcheck ge 0 then _e.(yrcheck) = eyran
		wset, cwin
	endif

	return
end