Pro PD_XR_view, snum, fnum, verify = ver, sum = sum, slit= sli, back_ang= bag,$
	diffuse = dif, kscan = ksc, hroi = hroi, vroi = vroi, $
	center= cnt, locate= lct, free=fre, tolerance=tol, pcenter= pcn, shift=shi,$
	shave = shv, rcenter = ct, fcenter = fct, _extra = _e

;+
; NAME:
;		PD_XR_VIEW
; VERSION:
;		8.475
; PURPOSE:
;		Displays frames from a single Pilatus detector reflectivity scan.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		PD_XR_VIEW, SNUM [, FNUM] [, keywords])
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
;	/SUM
;		Switch.  If set, the sum of the frames is displayed.
;	SLIT
;		A 2 element vector specifying electronic slit dimensions, in pixels, in
;		[horizontal,vertical] order.
;	BACK_ANG
;		Numeric scalar, the "sideways" offset angle used to evaluate background,
;		in degrees.  If not given, no background subtraction is performed.
;	/DIFFUSE
;		Switch.  If set, diffuse scattering slits (horizontal only) are assumed.
;	/KSCAN
;		Switch.  If set, Kscan geometry is assumed.
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;		If not given, HROI is generated internally based on the values of SLIT
;		and BACK_ANG (when given).
;	VROI
;		Same as HROI, for vertical region of interest.
;	CENTER
;		Location of the center of the reflectivity peak, in		| At most one
;		pixels.  Provided as a 2-element vector ([xy] order.	| of these two
;	/LOCATE														| may be used.
;		Switch,  If set, the center is located automatically.	| If neither is,
;		If the center such located is beyond the approved range	| the nominal
;		around the nominal center, it is rejected and the 		| center will
;		nominal center is used instead.							| be used.
;	/FREE
;		Switch.  If set and if LOCATE is set, PD_XR_VIEW is free to use an
;		individual center for each frame, as located.  Default is to use the
;		mean center of the evaluated frames.
;		
;		Note:	In the "far detector" mode both LOCATE and FREE are set
;				automatically.
;	TOLERANCE
;		Numeric scalar, value of the acceptable shift of the reflectivity peak
;		from the nominal center, in either direction, in pixels, when /LOCATE is
;		used.  If not given, defaults to 3 pixels for Pilatus or 9 pixels for
;		Apex (about 0.5mm).
;	PCENTER
;		Previous frame center data, provided in same format as FCENTER.  If 
;		given, it is used in lieu of results calculated SCAN_PD_CENLOC.
;		
;		Warning:	PCENTER is intended to save time by skipping recalculation
;					of frame centers, when same data is presented multiple 
;					times.  However, no checking is done to assure that the
;					provided PCENTER comes from same scan as the one being
;					displayed.  Thus, this keyword should be reserved for 
;					internal, non-interactive use.
;	/SHIFT
;		Switch.  If set and /LOCATE is set, the difference(s) between located
;		and calculated center positions are printed to the screen.  SHIFT is 
;		disabled when PCENTER is used.
;	SHAVE
;		Accepts a value to be "shaved" off each side of the horizontal region
;		of interest, for better display.
;	RCENTER
;		Optional output, see below.
;	FCENTER
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Screen output only.
; OPTIONAL OUTPUT PARAMETERS:
;	RCENTER
;		Returns the location (in pixels) used for the reflectivity peak, as a
;		2-element vector.
;	FCENTER
;		Returns the locations (in pixels) used for the reflectivity peak, frame
;		by frame, as a [2,N] array.  If /FREE is not set, this is just an 
;		N-duplication of the return of RCENTER.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Generates regions of interest large enough to contain the electronic
;		slits for signal and background and displays the frames with the
;		electronic slits superimposed.  Calls PD_VIEW.  Calls SCAN_COLUMN,
;		SCAN_PD_CENLOC, SCAN_PD_CENTER, SCAN_PD_FRAMES, SCAN_PD_SHOW,
;		SCAN_PD_TOL and SPEC_FILE_CHECK.  Also calls DEFAULT, ISNUM and
;		WHERINSTRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 5-AUG-2008 by Mati Meron.
;		Modified 25-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2008 by Mati Meron.  Added keyword KSCAN and internal
;		changes.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.	
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 10-FEB-2011 by Mati Meron.  Internal changes for far detector
;		support.  Added keyword SUM.
;		Modified 15-FEB-2011 by Mati Meron.  Added keyword SHIFT.
;		Modified 20-JUL-2011 by Mati Meron.  Internal changes.
;		Modified 15-AUG-2011 by Mati Meron.  Internal changes.  Added keyword
;		FCENTER.
;		Modified 25-DEC-2011 by Mati Meron.  Added keyword PCENTER.
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	wfnum = Scan_PD_frames(snum,fnum,ver=ver,nframes=nfnum)
	q = (Scan_column(snum,2-keyword_set(ksc)))[wfnum]
	if (max(q,min=min) - min) ge 1e-5 then begin
		s = sort(q)
		wfnum = wfnum[s]
	endif

	cur = fildat.scan[wsnum]
	dct = Scan_PD_center(wsnum,dim=dim)
	if n_elements(sli) gt 0 then begin
		if keyword_set(dif) then sli = [sli[0],2*dim[1]]
		if n_elements(sli) eq 2 then hs = round(sli)/2 $
		else message, 'Slit needs two elements!'
	endif else message, 'Slit not defined!'
	if Isnum(bag) then begin
		hoff = round([bag*!dtor*cur.pdist/cur.pdpix,0])
		if hoff[0] le 2*hs[0] then begin
			bagm = cur.pdpix*(2*hs[0]+1)/(!dtor*cur.pdist)
			message, 'Minimal offset for current slit is ' + $
			string(bagm,form='(f6.3)') + ' degrees!'
		endif
		bfl = 1
	endif else bfl = 0

	sumfl = keyword_set(sum)
	if sumfl then nfnum = 1
	pcnfl = keyword_set(pcn)
	if cur.pdfar or pcnfl then lct = (fre = 1)
	if keyword_set(lct) then begin
		shifl = keyword_set(shi)
		wtol = Scan_PD_tol(tol,pdstat=cur.pdstat)
		if pcnfl then fct = pcn else fct = Scan_PD_cenloc(wsnum,wfnum,$
		tol=wtol,ran=(wtol+1)/2,/mean,cent=cnt,shift=shi,ave=sum,_extra=_e)
		ct = round(total(reform(fct,2,nfnum),2)/nfnum)
		if keyword_set(fre) then begin
			off = fct
			off[0,*] = off[0,*] - ct[0]
			off[1,*] = off[1,*] - ct[1]
		endif
		if shifl and not pcnfl then begin
			if sumfl then tfnum = 'All'  else tfnum = wfnum
			print & tabulate, tfnum, shi[0,*], shi[1,*], form = ('3i4'), $
			tit = 'Center shifts', head = ['Frame','X','Y'] & print
		endif
	endif else begin
		ct = Default(cnt,dct)
		if n_elements(ct) ne 2 then message, 'Center needs 2 elements!'
		fct = transpose([[replicate(ct[0],nfnum)],[replicate(ct[1],nfnum)]])
	endelse

	if Wherinstruct('pix',_e) ge 0 then px = [1,1] else px = [0,0]
	mark = [ct-hs,ct+hs+px]
	chroi = [ct[0]- 2*(hs[0]>1),ct[0]+ 2*(hs[0]>1)]+ Default(shv,0,/dtyp)*[1,-1]
	cvroi = [ct[1] - 2*(hs[1]>1),ct[1] + 2*(hs[1]>1)]
	if bfl then begin
		cl = ct - hoff
		cr = ct + hoff
		mark = [mark,cl-hs,cl+hs+px,cr-hs,cr+hs+px]
		chroi = chroi + hoff[0]*[-1,1]
	endif
	whroi = 0 > Default(hroi,chroi) < (dim[0]-1)
	wvroi = 0 > Default(vroi,cvroi)	< (dim[1]-1)

	dum = (Wherinstruct('xy_int',_e))[0]
	if dum ge 0 then _e.(dum) = 0
	dum = (Wherinstruct('z_int',_e))[0]
	if dum ge 0 then _e.(dum) = 0

	if sumfl then Scan_pd_show, wsnum, wfnum, hroi = whroi, vroi = wvroi, $
	/jimg, /norm, mark = mark, xoff = whroi[0], yoff = wvroi[0], _extra = _e $
	else PD_view, wsnum, wfnum, hroi = whroi, vroi = wvroi, $
	foff = off, mark = mark, _extra = _e

	return
end