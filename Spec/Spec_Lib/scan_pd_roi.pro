Function Scan_PD_roi, snum, fnum, $
	last = lst, verify = ver, orientation = ori, sum = sum, $
	column = col, milimeter = mmt, ivar = ivr, scale = scl, nfact = nfc, $
	hslit = hsl, vslit = vsl, hoffset = hof, voffset = vof, $
	center = cnt, locate = lct, free = fre, tolerance = tol, $
	show = sho, rcenter = rct, title = tit, _extra= _e

;+
; NAME:
;		SCAN_PD_ROI
; VERSION:
;		8.475
; PURPOSE:
;		Extracts data from selected ROI in some or all frames within a PD scan.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_ROI( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		2 possible options:
;		1)	Single scan number.
;		2)	A complete preread image, either in the [M,N] or the [4,M,N] format.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
;		In case of SNUM option #2, FNUM is non-operational. 
; KEYWORD PARAMETERS:
; 	/LAST
; 		Switch.  If set, data from previous call is reused.  See COMMON BLOCKS
; 		for details.
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	ORIENTATION
;		An optional input specifying the detector orientation.  Can be provided
;		as integer input, 0 for horizontal, 1 for vertical.  Alternatively, can
;		also be provided as character input with two possible values, "HOR"
;		(for horizontal) and "VER" (for vertical).  First letter is sufficient.
;		Default is vertical.
;	/SUM
;		Switch.  If set, the selected frames (all, if not selected) are summed
;		prior to the evaluation.  Operational only for option 1.
;	COLUMN
;		Numeric or character scalar specifying the column within the #L bloc
;		of SNUM (in the SPEC file) to be used as the independent variable of
;		the result.  See SCAN_COLUMN for details.
;	/MILIMETER
;		Switch.  If set, the slit sizes (see below) are taken to be specified
;		in milimeters.
;	IVAR
;		Scalar, independent variable value.  Used only for preprocessed data 
;		(SNUM opt. 2).  For opt. 1 the needed value(s) are provided by COLUMN.
;	SCALE
;		Scalar, scaling parameter for conversion from pixels to milimeters. Used
;		only for SNUM opt. 2, for opt.1 the needed value is taken from the data.
;	NFACT
;		Scalar, the multiplicative normalization constant that has been apllied
;		to the data (see SCAN_PD_FPREAD).  Needed for proper calculation of 
;		errors.  Used only for SNUM opt. 2, for opt. 1 the value(s) are 
;		provided by SCAN_PD_FPREAD.
;	HSLIT
;		Numeric scalar specifying horizontal electronic slit dimension, in 
;		pixels unless MILIMETER is set.  Defaults to full detector size
;	VSLIT
;		Same as HSLIT, for the vertical dimension.
;	HOFFSET
;		Horizontal electronic slit offset, in pixels.  Defaults to 0.
;	VOFFSET
;		Vertical electronic slit offset, in pixels.  Defaults to 0.
;
;		Note:	Both offsets are relative to the default position, which is the
;				"center pixel" (see below) for the detector.
;	CENTER
;		Location of the nominal center location of the detector, in pixels.
;		Provided as a 2-element vector ([xy] order).  If not given defaults to
;		the value from the SPEC file (see SCAN_PD_CENTER), for opt. 1.  
;		Mandatory for opt. 2.
;	/LOCATE
;		Switch,  If set, the center is located automatically, within a range
;		given by TOLERANCE (see below) around the provided or default CENTER
;		value.
;	/FREE
;		Switch.  If set and if LOCATE is set, SCAN_PD_ROI is free to use an
;		individual center for each frame, as located.  Default is to use the
;		mean center of the evaluated frames.  Free has no meaning in the case
;		of preprocessed data.
;	TOLERANCE
;		Numeric scalar, value of the center search range from the nominal 
;		center, in either direction, in pixels, when /LOCATE is used.  If not 
;		given, defaults to 3 pixels for a Pilatus detector and 9 pixels for 
;		Apex (about 0.5mm).
;	/SHOW
;		Switch.  If set, the extracted data is shown to the screen.  If the 
;		input data represents a number ( >1) of frames, a plot of the extracted
;		data is generated.  In the case of single frame an image of the data is 
;		displayed with the region of interest being marked.
;	RCENTER
;		Optional output, see below.
;	TITLE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
;
;		Note: 	Of special importance is the keyword /NORM, passed to 
;				SCAN_PD_FPREAD.  By default the data is normalized (to MONC).
;				The normalization can be shut down by setting NORM = 0.
; OUTPUTS:
;		Returns integrated ROI values, in the standard 3-column 1D data format.
;		First column values come from the selected SPEC data column (by default
;		column 0), second column values are the integrated ROI values and the
;		third result column contains the statistical errors of the ROI values.
; OPTIONAL OUTPUT PARAMETERS:
;	RCENTER
;		Returns the location (in pixels) used for the reflectivity peak, as a
;		2-element vector.  If LOCATE with FREE are used, the returned center is
;		the average center for the scan.
;	TITLE
;		Returns the Pilatus data filename to be used as a plot title, for the
;		use of calling routines.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
;		
;		SPD_ROI_KEEP.   Contains:
;			EEXS	:	Status variable, value of 1 signifies that the 
;						following common block values have been defined.
;			LFDAT	:	2D or 3D array, the frame data from last call.
;			LEDAT	:	Data error array from last call, meaningful only when
;						last call used preprocessed daa.
;			LNFNUM	:	The number of frames present in last call.  For 
;						preprocessed data this value is automatically 1.
;			LDIM	:	The dimensions (hor. and ver.) of last data.
;			LWCT	:	The "working center" for the last data set.  Can be in
;						2-element vector format or a [2,LNFNUM] array if /FREE
;						was used in the last call.
;			LRCT	:	The "returned center" from last call, a 2-element vector
;			LIVR	:	Independent variable value(s) from last call.
;			LSCL	:	Scalar, the scaling parameter from last call.
;			LNFC	:	The normalization factor(s) from last call (see NFACT).
;			LTIT	:	The title (generated from data and scan file names) from
;						last call.
;		This data makes it possible to repeat the last call (while changing 
;		center, slits and/or offsets) without having  to reread the data.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Generates output in the standard 1D form (3 columns) where the values
; 		in the first column come from the selected column, while those in the
; 		second column are the sums of counts within the selected ROI of each 
; 		frame.  The third column contains the statistical errors of the second 
; 		one.
; 		Calls SCAN_COLUMN, SCAN_PD_CENLOC, SCAN_PD_CENTER, SCAN_PD_FPREAD, 
; 		SCAN_PD_FRAMES, SCAN_PD_READ, SCAN_PD_SHOW, SCAN_PD_TOL and SCAN_SHOW.
; 		Calls ERREST from SURF_LIB.  Also calls ARREQ, DEFAULT, FPU_FIX and
; 		ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-NOV-2009 by Mati Meron, as a generalization of SCAN_PD_XR.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 30-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 10-APR-2010 by Mati Meron.  Major rewrite, added ability to 
;		deal with preprocessed data and retain previous data.  Added common 
;		block SPD_ROI_KEEP and keywords LAST, IVAR, SCALE and NFACT.
;		Modified  20-FEB-2011 by Mati Meron.  Added keyword SUM and support for
;		far detector.
;		Modified 25-MAR-2011 by Mati Meron.  Updated error calculations for APEX
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common spd_roi_keep, eexs, lfdat, ledat, lnfnum, ldim, lwct, lrct, $
		livr, lscl, lnfc, ltit
	on_error, 1

	if keyword_set(lst) then begin
		if Isnum(eexs) then begin
			fdat = lfdat
			edat = ledat
			erfl = Arreq(fdat,edat,/noval)
			nfnum = lnfnum
			dim = ldim
			wct = lwct
			rct = lrct
			ivr = livr
			scl = lscl
			nfc = lnfc
			tit = ltit
		endif else message, 'There is no previous data!'
	endif else begin
		siz = size(snum)
		if siz[0] le 1 then begin
			dcnt = Scan_PD_center(snum,/sing,ori=ori,dim=dim,stat=sta,_extra=_e)
			if sta then begin
				wsnum = long(snum)
				wfnum = Scan_PD_frames($
				snum,fnum,verify=ver,nframes=nfnum,_extra=_e)
				cur = fildat.scan[wsnum]
				apfl = (cur.pdstat - 3)/2
				if cur.pdfar then lct = (fre = 1)
				if keyword_set(mmt) then scl = 1./cur.pdpix else scl = 1.

				if keyword_set(sum) then begin
					nfnum = 1
					iwfnum = 0
					sumfl = 1
				endif else begin
					iwfnum = wfnum
					sumfl = 0
				endelse

				if keyword_set(lct) then begin
					wtol = Scan_PD_tol(tol,pdstat=cur.pdstat)
					wct = Scan_PD_cenloc(wsnum,wfnum,$
					tol=wtol,ran=(wtol+1)/2,/mean,cent=cnt,ave=sum,_extra=_e)
					rct = total(reform(wct,2,nfnum),2)/nfnum
					if not keyword_set(fre) then $
					wct = transpose($
						[[replicate(rct[0],nfnum)],[replicate(rct[1],nfnum)]])
				endif else begin
					rct = Default(cnt,dcnt)
					if n_elements(rct) ne 2 then $
					message, 'Center needs 2 elements!'
					wct = transpose($
						[[replicate(rct[0],nfnum)],[replicate(rct[1],nfnum)]])
				endelse
				wct = round(wct)
				rct = round(rct)

				if sumfl then begin
					dat = Scan_PD_read(wsnum,wfnum,ori=ori,/norm,/raw,$
						tit=tit,_extra=_e)
					fdat = reform(dat[2,*,*],1,dim[0],dim[1])
					edat = reform(dat[3,*,*],1,dim[0],dim[1])
					erfl = 1
					nfc = 1
				endif else begin
					fdat = Scan_PD_fpread(wsnum,wfnum,ori=ori,/norm,nfa=nfc,$
						tit=tit,_extra=_e)
					edat = (erfl = 0)
				endelse
				ivr = (Scan_column(wsnum,Default(col,0),cnam=vnam))[iwfnum]
			endif else message, 'Bad or missing scan number!'
		endif else begin
			dim = siz[siz[0]-1:siz[0]]
			ivr = Default(ivr,0)
			scl = Default(scl,1.,/dtyp)
			nfc = Default(nfc,1.,/dtyp)
			tit =  ''

			case siz[0] of
				2	:	begin
							fdat = reform(snum,1,dim[0],dim[1])
							edat = reform(sqrt(nfc*snum),1,dim[0],dim[1])
						end
				3	:	begin
							fdat = reform(snum[2,*,*],1,dim[0],dim[1])
							edat = reform(snum[3,*,*],1,dim[0],dim[1])
						end
				else:	message, 'Unknown data format!'
			endcase
			erfl = (nfnum = 1)

			if n_elements(cnt) eq 2 then begin
				if keyword_set(lct) then begin
					big = (max(dim) ge 512)
					wtol = Default(tol,3*(1 + 2*big))
					wct = Scan_PD_cenloc(snum,$
					tol=wtol,ran=(wtol+1)/2,/mean,cent=cnt,_extra=_e)
				endif else wct = cnt
			endif else message, 'A 2-element center is required!'			
			wct = (rct = round(cnt))
		endelse
	endelse

	if Isnum(hsl) then hs = (round(scl*hsl)/2)[0] else hs = dim[0]
	if Isnum(vsl) then vs = (round(scl*vsl)/2)[0] else vs = dim[1]
	sl = [hs,vs]
	off = [Default(hof,0,/dtyp),Default(vof,0,/dtyp)]

	res = fltarr(3,nfnum)
	res[0,*] = ivr
	napfl = not apfl
	for i = 0l, nfnum-1 do begin
		dat = reform(fdat[i,*,*])
		ct = wct[*,i] + off
		hlim = [(ct[0] - sl[0]) > 0, (ct[0] + sl[0]) < (dim[0] - 1)] 
		vlim = [(ct[1] - sl[1]) > 0, (ct[1] + sl[1]) < (dim[1] - 1)]
		pdat = dat[hlim[0]:hlim[1],vlim[0]:vlim[1]]
		res[1,i] = total(pdat)
		if not erfl then begin
			if napfl then res[2,i] = sqrt(nfc[i]*res[1,i]) $
			else res[2,i] = nfc[i]*Errest(pdat/nfc[i],/emod,/tot)
		endif else res[2,i] = $
		sqrt(total(edat[i,hlim[0]:hlim[1],vlim[0]:vlim[1]]^2))
	endfor

	lfdat = fdat
	ledat = edat
	lnfnum = nfnum
	ldim  = dim
	lwct = wct
	lrct = rct
	livr = ivr
	lscl = scl
	lnfc = nfc
	ltit = tit
	eexs = 1

	if keyword_set(sho) then begin
		if nfnum eq 1 then begin
			mark = [hlim[0],vlim[0],hlim[1],vlim[1]]
			scan_pd_show, reform(fdat), title = tit, mark = mark, _extra = _e
		endif else scan_show, res, tit =  tit, _extra = _e
	endif

	return, FPU_fix(res)
end