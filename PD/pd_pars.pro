Pro PD_pars, snum, range = ran, round = ron, show = sho, _extra = _e

;+
; NAME:
;		PD_PARS
; VERSION:
;		7.15
; PURPOSE:
;		Calculates PD parameters (PDX, PDY, PDDIST) from an ABSCAN.
; CATEGORY:
;		PD specific.
; CALLING SEQUENCE:
;		PD_PARS, SNUM [, keywords]
; INPUTS:
;	SNUM
;		Single scan number (must be an ABSCAN).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RANGE
;		An optional input specifying the integration range for the determination
;		of the peak mean.  Can be given as scalar (in which case same range is
;		used for horizontal and vertical integration) or 2-element vector
;		containing the horizontal and vertical ranges.  The default values 
;		are [5,2].  Note, these are half ranges.
;	/ROUND
;		Switch.  If set, the peak locations found are rounded to the nearest
;		pixel.
;	/SHOW
;		Switch.  If set set, a plot of the difference between measured and
;		fitted peak locations is displayed.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.
; OUTPUTS:
;		Screen output only, prints the calculated X and Y coordinates of the
;		PD center, as well as the input values for SPEC (which may differ due
;		to different orientation).  Also,  if /SHOW  is set, displays a plot
;		of the vertical residuals.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The input scan must be of type ABSCAN.
; PROCEDURE:
;		Straightforward, performs a linear fit of the peak locations to values
;		of TAN (ALPHA - BETA).  Calls SCAN_PD_PEAK, SCAN_Q_ANGS and 
;		SPEC_FILE_CHECK.  Also calls ARREQ, DEFAULT, FLTROUND, LINFIT_MM and
;		POLEVAL, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-JUL-2009 by Mati Meron.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /pil, /single, list = wnum, _extra = _e
	if not Arreq(fildat.scan[wnum].stype,'abscan') then $
	message, 'Inappropriate scan, ABSCAN is required!'
	ploc = Scan_PD_peak(wnum,/mean,ran=Default(ran,[5,2]),title=tit,_extra=_e)
	pix = fildat.scan[wnum].pdpix
	angs = Scan_q_angs(wnum)
	tamb = tan(!dtor*reform(angs[0,*] - angs[1,*]))
	xloc = Linfit_mm(tamb,reform(ploc[0,*]),ord=0,err=xler)
	temp = Linfit_mm(tamb,reform(ploc[1,*]),err=terr)
	yloc = temp[0]
	yler = terr[0]
	pdist = pix*temp[1]
	pderr = pix*terr[1]
	if keyword_set(ron) then begin
		xloc = round(xloc)
		xler = Fltround(sqrt(0.5^2 + xler^2),dig=2)
		yloc = round(yloc)
		yler = Fltround(sqrt(0.5^2 + yler^2),dig=2)
		pdist = Fltround(pdist,dig=5)
		pderr = Fltround(pderr,dig=4)
		frm = ['i3','f3.1']
	endif else frm = ['f5.1','f3.1']

	print
	print, form = '("		X_center	= ",' + frm[0] + '," (",' + frm[1] + $
		',") pix")', xloc, xler
	print, form = '("		Y_center	= ",' + frm[0] + '," (",' + frm[1] + $
		',") pix")', yloc, yler
	print
	print, form = '("	PD-Sample distance	= ",f6.1," (",f4.1,") mm")', $
		pdist, pderr

	if fildat.scan[wnum].pdhv then begin
		xdum = xloc
		xder = xler
		xloc = yloc
		xler = yler
		yloc = fildat.scan[wnum].pddim[1] - 1 - xdum
		yler = xder
	endif

	print
	print
	print, '	In SPEC enter:'
	print, form = '("			SURF> pdx = ",' + frm[0] + ')', xloc
	print, form = '("			SURF> pdy = ",' + frm[0] + ')', yloc
	print, form = '("			SURF> pddist = ",f6.1)', pdist

	if keyword_set(sho) then plot, xtit= '!7b!x (deg.)', ytit= 'yoffset (mm)',$
	angs[1,*], pix*(ploc[1,*] - Poleval(tamb,temp)), _extra = _e

	return
end