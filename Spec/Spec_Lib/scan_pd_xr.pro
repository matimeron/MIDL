Function Scan_PD_XR, snum, fnum, verify = ver, inspect = ins, sum = sum, $
	slit = sli, back_ang = bag, left = lef, right = rig, btweak = btw, $
	center = cnt, locate = lct, free = fre, tolerance = tol, $
	drop = drp, show = sho, rdata = sig, bdata = bdt, $
	rcenter = rct, fcenter = fct, title = tit, _extra = _e

;+
; NAME:
;		SCAN_PD_XR
; VERSION:
;		8.48
; PURPOSE:
;		Extracts reflectivity data from some or all frames within a PD scan.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_XR( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	/INSPECT
;		Switch.  If set, the individual frames are displayed (using PD_XR_VIEW)
;		and the user can remove some from processing, if needed.
;	/SUM
;		Switch.  If set, the selected frames (all, if not selected) are summed
;		prior to the reflectivity evaluation.
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
;	BTWEAK
;		Scalar parameterallowing for "background tweaking".  If given, the 
;		background is multiplied by (1 + BTWEAK) prior to subtraction.
;	CENTER
;		Location of the nominal center of the reflectivity peak, in	pixels.
;		Provided as a 2-element vector ([xy] order).  If not given defaults to
;		the value from the SPEC file (see SCAN_PD_CENTER).
;	/LOCATE
;		Switch,  If set, the center is located automatically.  If the center
;		such located is beyond the approved range around the nominal center (see
;		TOLERANCE), it is rejected and the nominal center is used instead.
;	/FREE
;		Switch.  If set and if LOCATE is set, SCAN_PD_XR is free to use an
;		individual center for each frame, as located.  Default is to use the
;		mean center of the evaluated frames.
;		
;		Note:	In the "far detector" mode both LOCATE and FREE are set
;				automatically.
;	TOLERANCE
;		Numeric scalar, value of the acceptable shift of the reflectivity peak
;		from the nominal center, in either direction, in pixels, when /LOCATE is
;		used.  If not given, defaults to 3 pixels (about 0.5mm).
;	DROP
;		If set, first frame of the scan is dropped.  More general, if given as
;		a positive integer n, first n frames will be dropped.  If given as 
;		negative integer n, last n frames will be dropped.
;	/SHOW
;		Switch.  If set, the resulting reflectivity and background are displayed
;		to the screen.
;		Note:	If /SUM is set, then an image of the summed data with the slit
;				and background regions of interest superimposed are displayed.
;	RDATA
;		Optional output, see below.
;	BDATA
;		Optional output, see below.
;	RCENTER
;		Optional output, see below.
;	FCENTER
;		Optional output, see below.
;	TITLE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Returns (nonnormalized) reflectivity values, optionally corrected for
;		background, in the standard 3-column 1D data format.
; OPTIONAL OUTPUT PARAMETERS:
; 	RDATA
; 		Returns the raw data (no background subtraction) in the standard 
; 		3-column 1D data format.
; 	BDATA
; 		Returns the background data (averaged left-right) in the standard 
; 		3-column 1D data format.
;	RCENTER
;		Returns the location (in pixels) used for the reflectivity peak, as a
;		2-element vector.  If LOCATE with FREE are used, the returned center is
;		the average center for the scan.
;	FCENTER
;		Returns the locations (in pixels) used for the reflectivity peak, frame
;		by frame, as a [2,N] array.  If /FREE is not set, this is just 
;		N-duplication of the return of RCENTER.
;	TITLE
;		Returns the Pilatus data filename to be used as a plot title, for the
;		use of calling routines.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Evaluates the reflectivity values as the sums of counts within the
;		electronic window defined by SLIT around the peak value present in each
;		frame.  Optionally, if BACK_ANGLE is provided, background counts are
;		evaluated by summing counts within same size windows at locations offset
;		horizontally in both directions from the main peak by BACK_ANGLE, and
;		averaging.  Said background is then subtracted from the signal.
;		Calls SCAN_COLUMN, SCAN_LC, SCAN_PD_CENLOC, SCAN_PD_CENTER,
;		SCAN_PD_FRAMES, SCAN_PD_FPREAD, SCAN_PD_SHOW, SCAN_PD_TOL, SCAN_SHOW
;		and SPEC_FILE_CHECK.  Calls PD_XR_VIEW from PD.  Calls ERREST from 
;		SURF_LIB.  Also calls DEFAULT, FPU_FIX, HOW_MANY, ISNUM, RANGE_COMP, 
;		RANGE_PROC, STREQ, STRPARSE_MM and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUN-2008 by Mati Meron.
;		Modified 1-AUG-2008 by Mati Meron.  Internal changes.  Added keywords
;		CENTER, LOCATE, TOLERANCE and RCENTER.  Removed keywords ACC_SHIFT and
;		ABSOLUTE.
;		Modified 20-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 10-NOV-2008 by Mati Meron.  Added keywords LEFT, RIGHT and
;		FREE.  Modified peak location and streamlined operation.
;		Modified 25-OCT-2009 by Mati Meron.  Added keyword TITLE.
;		Modified 10-NOV-2009 by Mati Meron.  Added keyword DROP.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 30-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 5-APR-2010 by Mati Meron.  Internal changes, APEX related.
;		Modified 5-FEB-2011 by Mati Meron.  Internal changes for far detector 
;		support.
;		Modified 15-FEB-2011 by Mati Meron.  Added keyword SUM.
;		Modified 10-MAR-2011 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2011 by Mati Meron.  Updated error calculations for APEX
;		Modified 15-AUG-2011 by Mati Meron.  Internal changes.  Added keyword
;		FCENTER.
;		Modified 30-DEC-2011 by Mati Meron.  Internal changes to streamline
;		operation.  Added keyword INSPECT.
;		Modified 15-AUG-2012 by Mati Meron.  Extended the meaning of DROP (see
;		above).
;		Modified 5-OCT-2012 by Mati Meron.  Added keywords RDATA and BDATA.
;		Modified 1-JUL-2015 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;		Modified 15-FEB-2017 by Mati Meron.  Added keyword BTWEAK.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	wfnum = Scan_PD_frames(snum,fnum,verify=ver,nframes=nfnum,_extra=_e)
	if Isnum(drp,/int) then begin
		if drp gt 0 then wfnum = wfnum[drp:*] else wfnum = wfnum[0:drp-1]
		nfnum = nfnum - abs(drp)
	endif

	cur = fildat.scan[wsnum]
	apfl = (cur.pdstat - 3)/2
	if cur.pdfar then lct = (fre = 1)

	if n_elements(sli) eq 2 then hs = round(sli)/2 $
	else message, 'Slit needs two elements!'
	if Isnum(bag) then begin
		hoff = round([bag*!dtor*cur.pdist/cur.pdpix,0])
		if hoff[0] le 2*hs[0] then begin
			bagm = cur.pdpix*(2*hs[0]+1)/(!dtor*cur.pdist)
			message, 'Minimal offset for current slit is ' + $
			string(bagm,form='(f6.3)') + ' degrees!'
		endif
		bfl = 1
		dum = How_many(fir=lef,sec=rig,whi=whi)
		if dum then begin
			if whi then coef = [1.,-1.,0] else coef = [1.,0,-1.]
		endif else coef = [1.,-0.5,-0.5]
	endif else bfl = 0

	sumfl = keyword_set(sum)
	dum = (Wherinstruct('norm',_e))[0]
	if dum ge 0 then _e.(dum) = _e.(dum) < (1 - sumfl)
	fdat = Scan_PD_fpread(wsnum,wfnum,norm=1-sumfl,nfac=nfc,tit=tit,_extra=_e)

	if keyword_set(lct) then begin
		wtol = Scan_PD_tol(tol,pdstat=cur.pdstat)
		fct = Scan_PD_cenloc($
		wsnum,wfnum,tol=wtol,ran=(wtol+1)/2,/mean,cent=cnt,_extra=_e)
		rct = total(reform(fct,2,nfnum),2)/nfnum
		if not keyword_set(fre) then $
		fct = transpose([[replicate(rct[0],nfnum)],[replicate(rct[1],nfnum)]])
	endif else begin
		rct = Default(cnt,Scan_PD_center(wsnum))
		if n_elements(rct) ne 2 then message, 'Center needs 2 elements!'
		fct = transpose([[replicate(rct[0],nfnum)],[replicate(rct[1],nfnum)]])
	endelse
	fct = round(fct)
	rct = round(rct)

	if keyword_set(ins) then begin
		que = (ique = '')
		PD_XR_view,wsnum,wfnum,sli=sli,back=bag,pcen=fct,_extra=_e
		wait, 0.001
		read, que, prompt = 'OK (Y/N)? '
		if Streq(que,'n',1) then begin
			read, ique, prompt = 'Drop which? '
			if strlen(ique) gt 0 then begin
				bique = byte(ique)
				if bique[0] eq bique[-1] then begin
					if bique[0] eq 34 or bique[0] eq 39 $
					then bique = bique[1:-2]
				endif
				ique = string(bique)
				dlis = Range_proc(ique,/sort,/unique)
				frok = 0*wfnum + 1
				for ii = 0, n_elements(dlis) - 1 do begin
					dum = where(wfnum eq dlis[ii],ndum)
					if ndum gt 0 then frok[dum] = 0
				endfor
				good = where(frok,nfnum)
				wfnum = wfnum[good]
				if nfnum gt 0 then print, 'Using frames ' + Range_comp(wfnum) $
				else message, 'No frames left, aborting!'
				fdat = fdat[good,*,*]
				nfc = nfc[good]
				fct = fct[*,good]
				rct = total(reform(fct,2,nfnum),2)/nfnum
				dum = Strparse_mm(cur.pdfnam,'_',lis)
				zlen = strlen(lis[dum])
				fcod = strcompress('(I0'+string(zlen)+')',/rem)
				gnam = fildat.pdpath + strjoin(lis[0:dum-1],'_') + '_'
				tit= gnam + '(' + Range_comp(wfnum) +')'
			endif
		endif
	endif

	qz = (Scan_column(snum,'L'))[wfnum]
	qtol = 1e-4
	if sumfl then begin
		if (max(qz,min=min) - min) gt qtol $
		then message, 'Warning: Qz not constant', /con 
		qz = total(qz)/nfnum
		nfnum = 1
	endif else begin
		s = sort(qz)
		qz = qz[s]
		wfnum = wfnum[s]
	endelse
	sig = (rbg = (lbg = fltarr(3,nfnum)))
	sig[0,*] = (rbg[0,*] = (lbg[0,*] = qz))

	if sumfl then begin
		dat = total(fdat,1)
		fdat = Errest(fdat,emod=apfl,/squ)
		err = sqrt(total(fdat,1))
		if (Wherinstruct('glob',_e))[0] ge 0 then begin
			ntot = total((Scan_column(wsnum,'monc'))[wfnum])
			dat = dat/ntot
			err = err/ntot
		endif
		ct = rct
		sig[1] = total(dat[ct[0]-hs[0]:ct[0]+hs[0],ct[1]-hs[1]:ct[1]+hs[1]])
		sig[2] = sqrt( $
		total(err[ct[0]-hs[0]:ct[0]+hs[0],ct[1]-hs[1]:ct[1]+hs[1]]^2))
		if bfl then begin
			cl = ct - hoff
			lbg[1]=total(dat[cl[0]-hs[0]:cl[0]+hs[0],cl[1]-hs[1]:cl[1]+hs[1]])
			lbg[2]=sqrt( $
			total(err[cl[0]-hs[0]:cl[0]+hs[0],cl[1]-hs[1]:cl[1]+hs[1]]^2))
			cr = ct + hoff
			rbg[1]=total(dat[cr[0]-hs[0]:cr[0]+hs[0],cr[1]-hs[1]:cr[1]+hs[1]])
			rbg[2]=sqrt( $
			total(err[cr[0]-hs[0]:cr[0]+hs[0],cr[1]-hs[1]:cr[1]+hs[1]]^2))
		endif
	endif else begin
		napfl = not apfl
		for i = 0l, nfnum-1 do begin
			ct = fct[*,i]
			dat = reform(fdat[i,*,*])
			pdat = dat[ct[0]-hs[0]:ct[0]+hs[0],ct[1]-hs[1]:ct[1]+hs[1]]
			sig[1,i] = total(pdat)
			if napfl then sig[2,i] = sqrt(nfc[i]*sig[1,i]) $
			else sig[2,i] = nfc[i]*Errest(pdat/nfc[i],/emod,/tot)
			if bfl then begin
				cl = ct - hoff
				pdat = dat[cl[0]-hs[0]:cl[0]+hs[0],cl[1]-hs[1]:cl[1]+hs[1]]
				lbg[1,i] = total(pdat)
				if napfl then lbg[2,i] = sqrt(nfc[i]*lbg[1,i]) $
				else lbg[2,i] = nfc[i]*Errest(pdat/nfc[i],/emod,/tot)
				cr = ct + hoff
				pdat = dat[cr[0]-hs[0]:cr[0]+hs[0],cr[1]-hs[1]:cr[1]+hs[1]]
				rbg[1,i] = total(pdat)
				if napfl then rbg[2,i] = sqrt(nfc[i]*rbg[1,i]) $
				else rbg[2,i] = nfc[i]*Errest(pdat/nfc[i],/emod,/tot)
			endif
		endfor
	endelse

	if bfl then begin
		if nfnum eq 1 then begin
			sig = reform(sig,3,1)
			lbg = reform(lbg,3,1)
			rbg = reform(rbg,3,1)
		endif
		if Isnum(btw) then begin
			if abs(btw) lt 1 then begin
				lbg[1:2,*] = (1+btw)*lbg[1:2,*]
				rbg[1:2,*] = (1+btw)*rbg[1:2,*]
			endif else message, 'Tweak absolute value must be <1', /con
		endif
		res = Scan_lc(sig,lbg,rbg,coef=coef)
		bdt = Scan_lc(lbg,rbg,coef=-coef[1:2])
	endif else res = sig

	if keyword_set(sho) then begin
		if sumfl then begin
			if Wherinstruct('pix',_e) ge 0 then px = [1,1] else px = [0,0]
			mark = [ct-hs,ct+hs+px]
			chroi = [ct[0]- 2*(hs[0]>1),ct[0]+ 2*(hs[0]>1)]
			cvroi = [ct[1] - 2*(hs[1]>1),ct[1] + 2*(hs[1]>1)]
			if bfl then begin
				mark = [mark,cl-hs,cl+hs+px,cr-hs,cr+hs+px]
				chroi = chroi + hoff[0]*[-1,1]
			endif
			whroi = 0 > Default(hroi,chroi) < (dim[0]-1)
			wvroi = 0 > Default(vroi,cvroi)	< (dim[1]-1)
			Scan_pd_show, dat[whroi[0]:whroi[1],wvroi[0]:wvroi[1]],$
			mark= mark, xoff= whroi[0], yoff= wvroi[0], tit= tit, _extra= _e
		endif else begin
			lcol=[!pcol.red,!pcol.green,!pcol.blue]
			if bfl then begin
				dum = where(coef eq 0, ndum)
				if ndum gt 0 then lcol[dum] = !pcol.white
				Scan_show, sig, lbg, rbg, lcol = lcol, $
				xtit = 'Q!dz!n', ytit = 'Counts', tit = tit, _extra = _e
			endif else Scan_show, sig, lcol=lcol, $
			xtit = 'Q!dz!n', ytit = 'Counts', tit = tit, _extra = _e
		endelse
	endif

	return, FPU_fix(res)
end