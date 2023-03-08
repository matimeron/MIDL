Pro PD_view, snum, fnum, verify = ver, amax = amx, xy_int = xyi, z_int = zi, $
	angles = ang, qvals = qvl, pinhole = pnh, indiv_fr = inf, skew = skw, $
	foff = fof, cmark = cmr, noofset = nof, offset = ofs, wait = wai, psym= psm,$
	tiny = tin, window = win, _extra = _e

;+
; NAME:
;		PD_VIEW
; VERSION:
;		8.475
; PURPOSE:
;		Displays frames from a single area detector scan.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		PD_VIEW, SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Optional list of frame numbers, in any form acceptable by RANGE_PROC.
;		By default, all the frames within SNUM are used.
;
;		Note:	If some of the frame numbers are out of the range present,
;		they're ignored.  If there are no frame numbers within the range
;		present, an error message is issued.
; KEYWORD PARAMETERS:
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	AMAX
;		Scalar value, absolute reference to be used as maximum for all the
;		images displayed.  If not provided, the maximal value of all the frames
;		read is used.  If set at zero, each frame is normalized to its own
;		maximum.
;	/XY_INT															|
;		Switch.  If set, all the frames are integrated horizontally	|
;		and the 1D results displayed in a single window, with 		| Only one
;		offsets.													| of these
;	/Z_INT															| two may
;		Switch.  If set, all the frames are integrated vertically 	| be used.
;		and the 1D results displayed in a single window, with		|
;		offsets.													|
;	/ANGLES
;		Switch.  If set, angle coordinates are used for 1D displays.| Only one
;	/QVALS															| of these
;		Switch.  If set, Q-coordinates are used for 1D diplays.		| two may
;																	| be used.
;		Note 1	:	Unlike most other PD routines, the default coordinates for
;					PD_VIEW are RAW, not QVALS.
;		Note 2	:	2D images are always diplayed as RAW, by PD_VIEW, regardless
;					of the settings of ANGLES and QVALS.
;		Note 3	:	ANGLES and QVALS are ignored for camera.
;	/PINHOLE
;		Switch.  Specifies that horizontal pinhole angle transformations are to
;		be used.  If set, the distance PDSDIST from the structure FILDAT (see 
;		common block) is used in the transformation.  See the routine 
;		SCAN_PD_READ for details.
;	/INDIV_FR
;		Switch.  Specifies that coordinates should be recalculated for each
;		frame.  In the case of camera, INDIV_FR is ignored.
;
;		Note:	/PINHOLE and/or /INDIV_FRAME have effect only if one of /XY_INT 
;				and /Z_INT, as well as one of /ANGLES and /QVALS is set.
;	SKEW
;		Numeric scalar, if given the data is skewed along the horizontal 
;		dimension.  The value of SKEW is proportional to the tangent of the 
;		skew angle.  Values larger than 1 (in abs. value) should not be used.
;	FOFF
;		Numeric array (even number of entries) providing offsets for individual
;		frames.  The offsets apply to HROI and VROI, on readout, and to MARK on
;		display.  Active only for 2D diplay. 
;	/NOOFSET
;		Switch.  If set, 1D spectra are displayed with no offsets between
;		frames.  Has no effect for 2D images.
;
;		Note:	/NOOFSET has been obsoleted 6/30/15.  The new keyword is /OFFSET,
;				see below.
;	/OFFSET
;		Switch.  If set, 1D spectra are displayed with offsets between frames.
;		Replaces the previous keyword /OFFSET, so that the default mode became
;		displaying with no offsets.  Has no effect for 2D images.		
;	/CMARK
;		Switch.  If set, the center of the frame is marked with a cross.  Active
;		only for 2D display.
;	/WAIT
;		Switch.  If set in conjuction with /XY_INT or /Z_INT, the plots are
;		displayed one after another, waiting for the user to hit a key to
;		continue.  Alternatively, if WAIT is given as negative value, the
;		program waits for the time specified by the absolute value of WAIT
;		before displaying the next plot.
;	PSYM
;		Plot symbol number, same as in the PLOT command.
;	/TINY
;		Switch.  If set, the size of the display window is smaller, limited to
;		the capacity of a small laptop.
;	WINDOW
;		Integer scalar, the number of the (first) window to display.  If not
;		given, PD_VIEW starts with window #8, adding windows as needed.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the following keywords,
;		affecting data readout:
;
;			HROI
;				Two element vector defining horizontal region of interest,
;				in *pixels*.
;			VROI
;				Two element vector defining vertical region of interest, in
;				*pixels*.
;			/BAD
;				Switch.  If set, faulty (very high count rate) pixels are
;				removed from the data.
;			/NORM
;				Switch.  If set, the data is normalized to monitor counts.
;
;		See SCAN_PD_READ for more details.
;
;		Note:	Unlike the other PD routines, PD_VIEW doesn't normalize data by
;				default.
; OUTPUTS:
;		Graphics output only, either a set of 2D images or a plot of set of 1D
;		integrated data.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward, Reads the data and displayes it as a set of images or
;		1D curves.  Calls IMG_SCALE, SCAN_PD_CENTER, SCAN_PD_FPREAD, 
;		SCAN_PD_LCOO and SPEC_FILE_CHECK.  Calls CLEAN_NAME, DEFAULT, 
;		DISPLAY_MM, FLTROUND, ISNUM, LABELS, MAKE_GRID, ONE_OF, STREQ, 
;		STRPARSE_MM, WHERINSTRUCT and WIMG_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAY-2008 by Mati Meron.
;		Modified 25-JUL-2008 by Mati Meron.  Added keywords /WAIT and PSYM.
;		Modified 25-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 10-JUN-2009 by Mati Meron.  Internal changes.  Added support
;		for saving output (1D only) as graphics file.
;		Modified 10-AUG-2009 by Mati Meron.  Added keyword /INDIV_FR.
;		Modified 5-NOV-2009 by Mati Meron.  Added keyword /PINHOLE.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 10-APR-2010 by Mati Meron.  Internal changes to accomodate
;		Apex frames.
;		Modified 15-APR-2010 by Mati Meron.  Added keyword SKEW.
;		Modified 10-FEB-2011 by Mati Meron.  Added keyword FOFF.
;		Modified 20-JUL-2011 by Mati Meron.  Added keyword WINDOW.
;		Modified 10-OCT-2011 by Mati Meron.  Internal changes, added camera
;		support.
;		Modified 20-DEC-2011 by Mati Meron.  Internal changes.
;		Modified 5-OCT-2012 by Mati Meron.  Bug fixes.
;		Modified 20-NOV-2013 by Mati Meron.  Added keyword CMARK.
;		Modified 30-JUN-2015 by Mati Meron.  Replaced the keyword NOOFSET with
;		the keyword OFFSET.
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /pil, /sing, list = wsnum, _extra = _e
	dum = (Wherinstruct('new',_e))[0]
	if dum ge 0 then _e.(dum) = 0
	hcheck = (Wherinstruct('hro',_e))[0]
	if hcheck ge 0 then begin
		ehro = _e.(hcheck)
		xof = ehro[0]
	endif else xof = 0
	vcheck = (Wherinstruct('vro',_e))[0]
	if vcheck ge 0 then begin
		evro = _e.(vcheck)
		yof = evro[0]
	endif else yof = 0

	cur = fildat.scan[wsnum]
	buf = Scan_PD_fpread(wsnum,fnum,$
	verify=ver,foff=fof,flist=wfnum,nframes=nfnum,tit=wtit,_extra=_e)
	ftit = 'frame #' + string(wfnum, form = '(i0)')
	sdim = (size(buf))[2:3]
	if Isnum(skw) then begin
		scal = (1 + skw*Make_grid([-1,1],sdim[0]))#replicate(1.,sdim[1])
		for i = 0, nfnum-1 do buf[i,*,*] = Img_scale(reform(buf[i,*,*]),scal)
	endif	

	whi = One_of(xyi,zi,/noz)
	if whi lt 0 then wtit = strmid(wtit,0,strpos(wtit,'_',/reverse_search))
	if whi lt 0 and Isnum(fof) then begin
		nfof = n_elements(fof)
		if nfof mod 2 eq 0 then begin
			fofl = 1
			wfof = (reform(fof,2,nfof/2))[*,lindgen(nfnum)]
		endif else begin
			fofl = 0
			message, 'Bad frame offsets, ignoring', /con
		endelse
	endif else begin
		fofl = 0
		wfof = lonarr(2,nfnum)
	endelse

	apfl = (cur.pdstat - 3)/2
	if whi ge 0 then begin
		lbuf = total(buf,2+whi)
		slbuf = (size(lbuf))[2]
		if apfl eq 2 then begin
			infl = 0
			wha = -1			
		endif else begin
			infl = keyword_set(inf)
			wha = One_of(ang,qvl)			
		endelse

		if wha ge 0 then begin
			if infl then begin
				coo = fltarr(slbuf,nfnum)
				for i = 0l, nfnum-1 do coo[*,i] = Scan_PD_lcoo($
				wsnum,wfnum[i],ang=ang,dir=1-whi,pin=pnh,_extra=_e)
			endif else coo = Scan_PD_lcoo($
			wsnum,ang=ang,dir=1-whi,pin=pnh,_extra=_e)
		endif else begin
			coo = lindgen(slbuf)
			if whi then coo = coo + xof else coo = coo + yof
			if infl then message, $
			'ANGLES or QVALS need to be set for INDIV option', /con
			infl = 0
		endelse
		if not infl then coo = reform(coo)#replicate(1,nfnum)
		off = Fltround(max(lbuf),/ceil)
		ofl = keyword_set(ofs)
		xytit = $
		[['pix.','Dth','Q!dxy!n'],['pix.','Beta','Q!dz!n']]
		xtit = xytit[1+wha,1-whi]
		xran = [min(coo),Fltround(1.05*max(coo),dig=2,/ceil)]
		pcol= [!pcol.dred,!pcol.dgreen,!pcol.dblue,!pcol.purple]
		labs = string(wfnum, form='(i0)')
		xlab = 1.02*max(coo)
		ylab = lbuf[*,slbuf-1]
		ii = lindgen(nfnum)
		psm = Default(psm,0,/dtyp)
		wai = Default(wai,0.,/dtyp)
		if wai gt 0 then print, string([13b,9b,9b]) + $
		'Hit "Q" to exit, any other key to continue'

		if (Wherinstruct('ylo',_e))[0] ge 0 then begin
			if ofl then rat = sqrt(10) else rat = 1
			plot, coo[*,0], coo[*,0],xran=xran,yran=[1,off*rat^nfnum+1],/nodat,$
			xstyle= 1, ystyle= 1, xtit= xtit, ytit= 'a.u', tit= wtit, _extra= _e
			for i = 0l, nfnum-1 do begin
				oplot, coo[*,i], rat^i*(lbuf[i,*]+1),psym=psm,col=pcol[i mod 4]
				if wai gt 0 then begin
					dum = (dum = get_kbrd())
					if Streq(dum,'q',1) then break
				endif else if wai lt 0 then wait, abs(wai)
			endfor
			j = i < (nfnum-1)
			if ofl then $
			Labels, xlab, ((ylab+1)*rat^ii)[0:j], labs[0:j], col= !pcol.cyan
		endif else begin
			if ofl then os = 1 else os = nfnum
			plot, coo[*,0], coo[*,0], xran=xran,yran=[0,off*(nfnum/os)],/nodat,$
			xstyle= 1, ystyle= 1, xtit= xtit, ytit= 'a.u', tit= wtit, _extra= _e
			for i = 0l, nfnum-1 do begin
				oplot, coo[*,i], (i/os)*off+lbuf[i,*],psym=psm,col=pcol[i mod 4]
				if wai gt 0 then begin
					dum = (dum = get_kbrd())
					if Streq(dum,'q',1) then break
				endif else if wai lt 0 then wait, abs(wai)
			endfor
			j = i < (nfnum-1)
			if ofl then $
			Labels, xlab, (ylab + ii*off)[0:j], labs[0:j], col= !pcol.cyan
		endelse
		Wimg_mm, Clean_name(wtit), call = 2, /nodef, /verb, _extra = _e
	endif else begin
		owin = !d.window
		swin = Default(win,8)
		case apfl of
			0:	begin
					xdim = [0,327,229,197]
					ydim = [0,553,309,228]
					xndim = [0,195,97,65]
					yndim = [0,487,243,162]
					bfac = [1,1]
					if keyword_set(tin) then begin
						fmax = 15
						xbin = [0,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3]
						ybin = [0,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3]
						rows = [0,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3]
						cols = [0,1,2,3,2,3,3,4,4,3,4,4,4,5,5,5]
					endif else begin
						fmax = 24
						xbin=[0,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3]
						ybin=[0,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3]
						rows=[0,1,1,1,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4]
						cols=[0,1,2,3,2,3,3,3,3,3,4,4,4,5,5,5,4,5,5,5,5,6,6,6,6]
					endelse
				end
			1:	begin
					xndim = [0,512,256,170]
					yndim = [0,512,256,170]
					xdim = [0,644,388,302]
					ydim = [0,578,322,236]
					bfac = ceil(float((size(buf))[2:3]) /512)
					if keyword_set(tin) then begin
						fmax = 12
						xbin = [0,1,2,2,2,3,3,3,3,3,3,3,3]
						ybin = [0,1,2,2,2,3,3,3,3,3,3,3,3]
						rows = [0,1,1,2,2,2,2,2,2,3,3,3,3]
						cols = [0,1,2,2,2,3,3,4,4,3,4,4,4]
					endif else begin
						fmax = 16
						xbin = [0,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3]
						ybin = [0,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3]
						rows = [0,1,1,1,2,2,2,3,3,3,3,3,3,4,4,4,4]
						cols = [0,1,2,3,2,3,3,3,3,3,4,4,4,4,4,4,4]
					endelse
				end
			2:	begin
					xndim = [0,512,256,170]
					yndim = [0,512,256,170]
					xdim = [0,644,388,302]
					ydim = [0,578,322,236]
					bfac = ceil(float((size(buf))[2:3]) /512)
					if keyword_set(tin) then begin
						fmax = 12
						xbin = [0,1,2,2,2,3,3,3,3,3,3,3,3]
						ybin = [0,1,2,2,2,3,3,3,3,3,3,3,3]
						rows = [0,1,1,2,2,2,2,2,2,3,3,3,3]
						cols = [0,1,2,2,2,3,3,4,4,3,4,4,4]
					endif else begin
						fmax = 16
						xbin = [0,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3]
						ybin = [0,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3]
						rows = [0,1,1,1,2,2,2,3,3,3,3,3,3,4,4,4,4]
						cols = [0,1,2,3,2,3,3,3,3,3,4,4,4,4,4,4,4]
					endelse
				end
			4:	begin
					xndim = [0,521,260,173]
					yndim = [0,521,260,173]
					xdim = [0,653,392,305]
					ydim = [0,587,326,239]
					bfac = ceil(float((size(buf))[2:3]) /522)
					if keyword_set(tin) then begin
						fmax = 12
						xbin = [0,1,2,2,2,3,3,3,3,3,3,3,3]
						ybin = [0,1,2,2,2,3,3,3,3,3,3,3,3]
						rows = [0,1,1,2,2,2,2,2,2,3,3,3,3]
						cols = [0,1,2,2,2,3,3,4,4,3,4,4,4]
					endif else begin
						fmax = 16
						xbin = [0,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3]
						ybin = [0,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3]
						rows = [0,1,1,1,2,2,2,3,3,3,3,3,3,4,4,4,4]
						cols = [0,1,2,3,2,3,3,3,3,3,4,4,4,4,4,4,4]
					endelse
				end
		endcase

		nf = nfnum < fmax
		bin = [xbin[nf],ybin[nf]]
		roc = [rows[nf],cols[nf]]
		xf = xdim[bin[0]]
		yf = ydim[bin[1]]
		csize = 1. - 0.1*max(bin)
		wsize = reverse(roc)*[xf,yf]

		nw = ceil(1.*nfnum/fmax)
		wtit = 'IDL '+ string(indgen(nw)+ swin, form ='(i0)') + ':  ' + wtit
		if nw gt 1 then wtit= wtit+' ;  page '+string(indgen(nw)+1,form='(i0)')

		k = indgen(fmax)
		pof = transpose($
		[[(k mod roc[1])*xf],[(roc[0]- k/roc[1]- 1)*yf]])
		noer = k gt 0
		amx = Default(amx,max(buf))
		if hcheck lt 0 and vcheck lt 0 then auz = 0 $
		else auz = [xndim[bin[0]],yndim[bin[1]]]
		bin = bfac*bin

		if fofl then begin
			mcheck = (Wherinstruct('mar',_e))[0]
			if mcheck ge 0 then begin
				emar = _e.(mcheck)
				mind = 2*lindgen(n_elements(emar)/2)
				mfofl = 1
			endif else mfofl = 0
		endif else mfofl = 0

		rcheck = (Wherinstruct('rot',_e))[0]
		if rcheck ge 0 then begin
			erot = _e.(rcheck)
			_e.(rcheck) = 0
		endif

		if keyword_set(cmr) then begin
			ccen = Scan_PD_center(wsnum)
			cmfl = 1
		endif else cmfl = 0

		for i = nw-1, 0, -1 do begin
			window, swin+i, xsize= wsize[0], ysize= wsize[1], tit = wtit[i]
			nrem = (nfnum - i*fmax) < fmax
			for j = 0, nrem-1 do begin
				k = i*fmax + j
				if mfofl then begin
					kemar = emar
					kemar[mind] = kemar[mind] + wfof[0,k]
					kemar[mind+1] = kemar[mind+1] + wfof[1,k]
					_e.(mcheck) = kemar
				endif
				Display_mm, buf[k,*,*],poff= pof[*,j],noer= noer[j],bin= bin, $
				/ave, amax= amx, tit= ftit[k], charsiz= csize, /shav, auz= auz,$
				xoff= xof+ wfof[0,k], yoff= yof+ wfof[1,k], ppos= pps,ran= ran,$
				_extra= _e
				if cmfl then begin
					plot, ran[[0,2]], ran[[1,3]], pos=pps,/dev,xsty=13,ysty=13,$
					/noerase, /nodata
					plots, ccen,col=!pcol.white,psym=7,symsize=1,thi=2
				endif
			endfor
		endfor
		if mfofl then _e.(mcheck) = emar
		if rcheck ge 0 then _e.(rcheck) = erot
		wset, owin
	endelse

	return
end