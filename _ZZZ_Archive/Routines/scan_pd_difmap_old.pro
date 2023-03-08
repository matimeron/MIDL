Function Scan_PD_difmap_old, snum, fnum, verify = ver, inspect = ins, sum = sum, $
	slit = sli, back_ang = bag, left = lef, right = rig, $
	center = cnt, locate = lct, free = fre, tolerance = tol, bottom = bot, $
	drop = drp, show = sho, rcenter= rct, fcenter= fct, title= tit, _extra= _e

;+
; NAME:
;		SCAN_PD_DIFMAP
; VERSION:
;		8.15
; PURPOSE:
;		Generates a map of specular and diffuse data.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_DIFMAP( SNUM [, FNUM] [, keywords])
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
;		and the user can remove some from processing, if needed.  INSPECT is 
;		only active when SUM (see below) is set, else it is ignored.
;	/SUM
;		Switch.  If set, the selected frames (all, if not selected) are summed
;		prior to the reflectivity evaluation.
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
;		Location of the nominal center of the reflectivity peak, in	pixels.
;		Provided as a 2-element vector ([xy] order).  If not given defaults to
;		the value from the SPEC file (see SCAN_PD_CENTER).
;	/LOCATE
;		Switch,  If set, the center is located automatically.  If the center
;		such located is beyond the approved range around the nominal center (see
;		TOLERANCE), it is rejected and the nominal center is used instead.
;	/FREE
;		Switch.  If set and if LOCATE is set, SCAN_PD_DIFMAP is free to use an
;		individual center for each frame, as located.  Default is to use the
;		mean center of the evaluated frames.
;		
;		Note:	In the "far detector" mode both LOCATE and FREE are set
;				automatically.
;	TOLERANCE
;		Numeric scalar, value of the acceptable shift of the reflectivity peak
;		from the nominal center, in either direction, in pixels, when /LOCATE is
;		used.  If not given, defaults to 3 pixels (about 0.5mm).
;	BOTTOM
;		Numeric scalar, the value of the minimum Beta value to be included in 
;		the map.  If BOTTOM is lower then the minimum Beta value present, the 
;		Beta range is extended downward to reach BOTTOM.
;	DROP
;		If set, first frame of the scan is dropped.  More general, if given as
;		a positive integer n, first n frames will be dropped.
;	/SHOW
;		Switch.  If set, the generated map is displayed to the screen.
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
;		Returns 2D data (in an [Alpha,Beta,Data,Error] format), such that each
;		data column contains masured intensity for a single alpha and range of
;		Beta values.
; OPTIONAL OUTPUT PARAMETERS:
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
;		Integrates detector data over an electronic horizontal slit, same as
;		diffuse scattering data.  Optionally, if BACK_ANGLE is provided, 
;		background counts are evaluated by summing counts within same size 
;		windows at locations offset horizontally in both directions from the 
;		main peak by BACK_ANGLE, and averaging.  Said background is then 
;		subtracted from the signal.
;		Calls IMG_INT, SCAN_COLUMN, SCAN_INTER, SCAN_LC, SCAN_PD_CENLOC, 
;		SCAN_PD_CENTER, SCAN_PD_FRAMES, SCAN_PD_FPREAD, SCAN_PD_LCOO, 
;		SCAN_PD_SHOW, SCAN_Q_ANGS and SPEC_FILE_CHECK.  Calls PD_XR_VIEW from 
;		PD.  Calls ERREST from SURF_LIB.  Also calls DEFAULT, FPU_FIX, HOW_MANY,
;		ISNUM, MAKE_GRID, RANGE_COMP, RANGE_PROC, STREQ, STRPARSE_MM and 
;		WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron as a modification of SCAN_PD_XR.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	wfnum = Scan_PD_frames(snum,fnum,verify=ver,nframes=nfnum,_extra=_e)
	if Isnum(drp,/int) then begin
		wdrp = drp > 0
		wfnum = wfnum(wdrp:*)
		nfnum = nfnum - wdrp
	endif

	cur = fildat.scan[wsnum]
	apfl = (cur.pdstat - 3)/2
	if cur.pdfar then lct = (fre = 1)

	if n_elements(sli) gt 0 then hs = round(sli[0])/2 $
	else message, 'Missing slit size!'
	if Isnum(bag) then begin
		hoff = round([bag*!dtor*cur.pdist/cur.pdpix,0])
		if hoff[0] le 2*hs then begin
			bagm = cur.pdpix*(2*hs+1)/(!dtor*cur.pdist)
			message, 'Minimal offset for current slit is ' + $
			string(bagm,form='(f6.3)') + ' degrees!'
		endif
		bfl = 1
		dum = How_many(fir=lef,sec=rig,whi=whi)
		if dum then begin
			if whi then coef = [1.,-1.,0] else coef = [1.,0,-1.]
		endif else coef = [1.,-0.5,-0.5]
	endif else begin
		message, 'Missing background angle, results unreliable', /con
		bfl = 0
	endelse

	sumfl = keyword_set(sum)
	dum = (Wherinstruct('norm',_e))[0]
	if dum ge 0 then _e.(dum) = 1 - sumfl
	fdat = Scan_PD_fpread(wsnum,wfnum,norm=1-sumfl,nfac=nfc,tit=tit,_extra=_e)
	vsiz = (size(fdat))[3]

	if keyword_set(lct) then begin
		wtol = Default(tol,3*(cur.pdstat - 2))
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

	if keyword_set(ins) and sumfl then begin
		que = (ique = '')
		PD_XR_view,wsnum,wfnum,sli=sli,back=bag,pcen=fct,/diffuse,_extra=_e
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
		nfnum = 1
	endif else wfnum = wfnum[sort(qz)]
	sig = (rbg = (lbg = fltarr(3,vsiz)))
	sig[0,*] = (rbg[0,*] = (lbg[0,*] = findgen(vsiz)))
	res = fltarr(4,nfnum,vsiz)

	if sumfl then begin
		ct = rct
		res[0,0,*] = (Scan_q_angs(wsnum,wfnum[0]))[0]
		res[1,0,*]=Scan_pd_lcoo(wsnum,wfnum[0],dir='ver',cent=ct,/ang,_extra=_e)
		dat = total(fdat,1)
		fdat = Errest(fdat,emod=apfl,/squ)
		err = sqrt(total(fdat,1))
		if (Wherinstruct('glob',_e))[0] ge 0 then begin
			ntot = total((Scan_column(wsnum,'monc'))[wfnum])
			dat = dat/ntot
			err = err/ntot
		endif
		sig[1,*] = total(dat[ct[0]-hs:ct[0]+hs,*],1)
		sig[2] = sqrt(total(err[ct[0]-hs:ct[0]+hs,*]^2,1))
		if bfl then begin
			cl = ct - hoff
			lbg[1,*] = total(dat[cl[0]-hs:cl[0]+hs,*],1)
			lbg[2,*] = sqrt(total(err[cl[0]-hs:cl[0]+hs,*]^2,1))
			cr = ct + hoff
			rbg[1,*] = total(dat[cr[0]-hs:cr[0]+hs,*],1)
			rbg[2,*] = sqrt(total(err[cr[0]-hs:cr[0]+hs,*]^2,1))
			sig = Scan_lc(sig,lbg,rbg,coef=coef)
		endif
		res[2:3,0,*] = sig[1:2,*]
	endif else begin
		alp = (Scan_q_angs(wsnum,wfnum))[0,*]
		avec = Make_grid([min(alp,max=max),max],nfnum)
		for i = 0l, nfnum-1 do begin
			ct = fct[*,i]
			res[0,i,*] = avec[i]
			res[1,i,*] = $
			Scan_pd_lcoo(wsnum,wfnum[i],dir='ver',cent=ct,/ang,_extra=_e)
			dat = reform(fdat[i,*,*])
			sig=Img_int(dat,xy_reg=ct[0]+[-hs,hs],/xy_int,emod=apfl,nfac=nfc[i])
			if bfl then begin
				cl = ct - hoff
				lbg = $
				Img_int(dat,xy_reg=cl[0]+[-hs,hs],/xy_int,emod=apfl,nfac=nfc[i])
				cr = ct + hoff
				rbg = $
				Img_int(dat,xy_reg=cr[0]+[-hs,hs],/xy_int,emod=apfl,nfac=nfc[i])
				sig = Scan_lc(sig,lbg,rbg,coef=coef)
			endif
			res[2:3,i,*] = sig[1:2,*]
		endfor
		bstep = (max(res[1,0,*],min=min) - min)/(vsiz-1)
		bran = [Default(bot,min(res[1,*,*],max=max)),max]
		bsiz = round((bran[1] - bran[0])/bstep)
		bvec = Make_grid(bran,bsiz)
		ores = res
		res = fltarr(4,nfnum,bsiz)
		for i = 0, nfnum-1 do begin
			res[0,i,*] = avec[i]
			res[1,i,*] = bvec
			imin = min(ores[1,i,*],max=imax)
			dum = where(bvec ge imin and bvec le imax)
			res[1:3,i,dum] = Scan_inter(reform(ores[1:3,i,*]),bvec[dum])
		endfor
	endelse
	res[2,*,*] = res[2,*,*] > 0

	if keyword_set(sho) then Scan_PD_show, res,  xtit = 'Alpha', ytit = 'Beta',$
	tit = tit, _extra = _e

	return, FPU_fix(res)
end