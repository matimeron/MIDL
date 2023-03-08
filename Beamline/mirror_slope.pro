Function Mirror_slope, snum, angle = ang, scan_type = stp, slit_loc = slc, $
	mir_loc = mlc, det_loc = dlc, ampfactor = amp, net_err = ner, $
	show = sho, frames = frm, wait = wai, window = win, inspect = ins, $
	shape = shp, volt = vvl, _extra = _e

;+
; NAME:
;		MIRROR_SLOPE
; VERSION:
;		8.18
; PURPOSE:
;		Mapping mirror slopes.
; CATEGORY:
;		Beamline optics, 15IDC specific.
; CALLING SEQUENCE:
;		Result = MIRROR_SLOPE( SNUM, ANGLE = ANG [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ANGLE
;		Mirror angle, in mr.
;	SCAN_TYPE
;		Character scalar, denotes mirror scan type.  The acceptable types are:
;			Joint	-	Both mirrors moving in parallel.  This is the default.
;			Mir1	-	Scanning first mirror, second mirror moving at twice the
;						rate to present same point to the beam.
;			Mir2	-	Scanning second mirror, first mirror stationary
;						presenting same point to the beam.
;			Slit	-	Both mirrors stationary, slit is scanned accross the
;						beam upstream of the mirrors.
;	SLIT_LOC
;		Numeric scalar, location of the pre-mirror slit.  Default value is 31.4.
;	MIR_LOC
;		Numeric scalar or 2-element vector, location of the mirror(s).  If given
;		as 2 element vector, the first value is for mirror_1 and the second for
;		mirror_2.  For Joint and Slit scans, the midpoint of the two is used.
;		Default values are [32.1,32.9].
;	DET_LOC
;		Numeric scalar, the detector location.  Mandatory, no default.
;		
;		Note:	All the above locations are measured in meters, from source.
;	AMPFACTOR
;		Parameter pertaining to calculation of the locations of reflected beams.
;		The location is calculated as the centroid of the fraction of the peak
;		with amplitude >= AMPFACTOR*Maximum.  Default is 0.5.
;	/NET_ERR
;		Switch.  If set, both global slope and global curvature are subtracted
;		from the result.  Default is subtract global slope only.
;	/SHOW
;		Switch.  If set, plots of the mirror's slope and shape errors are
;		displayed.
;	/FRAMES
;		Switch.  If set, plots of the individual frames are displayed, marking
;		the region used for centroid calculation.
;	WAIT
;		Setting the wait time, in seconds, when displaying individual frames.
;		Default is 1sec.  Has no effect when /FRAMES is not set.
;	WINDOW
;		Sets the first window for display purposes.  Default is 0.
;	/INSPECT
;		Switch.  If set, the user is shown the resulting slope and given the
;		option to drop some data points before final result is calculated.
;	SHAPE
;		Optional output, see below.
;	VOLT
;		Optional output, see below.
;	_EXTRA
;		Formal keyword used to pass keywords to imbedded routines.  Not to be 
;		used directly.
; OUTPUTS:
;		Returns the mirror local slopes in a [2,N] format, where the first 
;		column contains mirror locations and the second the corresponding slopes
; OPTIONAL OUTPUT PARAMETERS:
; 	SHAPE
;		Returns the mirror local vertical offsets, in same format as the main
;		output.
;	VOLT
;		For a bimorph mirror, returns the voltages applied to the mirror.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Translates the reflected beam locations on the detector to local mirror
;		slopes.  For details see the "Mirror Scans" writeup.  
;		Calls PEAK_CENT, SCAN_COLUMN, SCAN_FIELD_READ, SCAN_PD_FREAD and 
;		SCAN_SHOW, from SPEC.  Calls DEFAULT, DIF, INTEG, LINFIT_MM, POLEVAL, 
;		RANGE_COMP, RANGE_PROC STREQ, STRMATCH_MM and TOLER, from MIDL. 
; MODIFICATION HISTORY:
;		Created 15-DEC-2011 by Mati Meron.
;		Modified 10-JUL-2012 by Mati Meron.  Slight internal changes.
;		Modified 30-JUL-2012 by Mati Meron.  Added keyword SLIT.
;		Modified 1-OCT-2012 by Mati Meron.  Change of scan type default.
;-

	on_error, 1
	eps = Toler()

	styp = ['joint','mir1','mir2','slit']
	zsgn = [-1,-1,1,1]
	slitloc = 31.4
	mirloc = [32.1,32.9]
	pix = 0.00196
	wang = 1e-3*ang
	wamp = Default(amp,0.5)

	shofl = keyword_set(sho) or keyword_set(frm) or keyword_set(ins)
	if shofl then begin
		cwin = !d.window
		wwin = Default(win,0)
		window, wwin
	endif

	wstp = (Strmatch_mm(stp,styp) + 4) mod 4
	wslc = Default(slc,slitloc)
	wmlc = Default(mlc,mirloc)
	case wstp of
		0	:	wmlc = total(wmlc)/n_elements(wmlc)
		1	:	wmlc = wmlc[0]
		2	:	wmlc = wmlc[1 < n_elements(wmlc) - 1]
		3	:	wmlc = total(wmlc)/n_elements(wmlc)
	endcase
	dis = dlc - wmlc
	hcor = [0,2,2,1+dis/wmlc]
	ymult = [1,1,1,wmlc/wslc]

	dat = total(Scan_pd_fpread(snum,nframes=npo,/ver,_extra=_e),2)
	res = fltarr(2,npo)
	ym = Scan_column(snum,0)
	if wstp eq 3 then ym = (ym + Scan_column(snum,1))/2
	if n_elements(ym) ne npo then message, 'Lost frames!'
	ym = (ym - total(ym)/npo)*ymult[wstp]
	zm = zsgn[wstp]/sin(wang)*ym
	yd = 0*zm

	for i = 0, npo-1 do begin
		tit = 'Frame # ' + string(i,form='(i0)')
		yd[i] = Peak_cent(dat[i,*],amp=wamp,/aug,show=frm,/bran,tit=tit)
		if keyword_set(frm) then wait, Default(wai,1)
	endfor
	yd = pix*yd - hcor[wstp]*ym
	yd = (yd - total(yd)/npo)
	msl = - 1e3*yd/(2*dis)
	if zsgn(wstp) eq - 1 then begin
		zm = reverse(zm)
		msl = reverse(msl)
	endif

	if keyword_set(ins) then begin
		que = (ique = '')
		slp = transpose([[zm],[msl]])
		Scan_show, slp, /num, _extra = _e
		wait, 0.001
		print
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
				plis = lindgen(npo)
				pok = replicate(1,npo)
				for ii = 0, n_elements(dlis) - 1 do begin
					dum = where(plis eq dlis[ii],ndum)
					if ndum gt 0 then pok[dum] = 0
				endfor
				good = where(pok,ngood)
				if ngood gt 0 then print, 'Using points ' + Range_comp(good) $
				else message, 'No points left, aborting!'
				zm = zm[good]
				msl = msl[good]
			endif
		endif
	endif

	coe = Linfit_mm(zm,msl)
	nmsl = msl - Poleval(zm,coe)
	if keyword_set(ner) then msl = nmsl else msl = msl - coe[0]
	if min(abs(Dif(zm,/lin))) eq 0 then begin
		msh = total([0.,(zm[1:-1] - zm[0:-2])*(msl[1:-1] + msl[0:-2])/2],/cum)
		message, 'Bad data in scan # ' + string(snum,form='(i0)'), /con
	endif else msh = 1e-3*Integ(zm,msl)
	slp = transpose([[zm],[msl]])
	shp = transpose([[zm],[msh]])

	if keyword_set(sho) then begin
		if coe[1] eq 0 then rad = 1/eps else rad = 1/coe[1]
		slerr = sqrt(abs(Integ(zm,nmsl^2,/val))/(max(zm,min=mzm) - mzm))
		print
		print, rad, form = '("	Radius		= ",g10.3," km")'
		print, slerr, form = '("	Slope error	= ",g10.3," microrad")'
		print

		stit = ', Scan #' + string(snum, form = '(i0)')
		if keyword_set(ner) then etit = ' (global curvature subtracted)' $
		else etit = ''
		vvl = reform(Scan_field_read(snum,'mirval'))
		nvl = n_elements(vvl)
		tvvl = reform(vvl,nvl/2,2)
		vtit = '!c'+strjoin(string(reform(long(tvvl[*,0])),form='(i0)'),', ')+ $
				'!c'+strjoin(string(reform(long(tvvl[*,1])),form='(i0)'),', ')
		Scan_show, slp, xtit= 'mm', ytit= '!7l!xrad', ymargin = [4,6], $
		tit= 'Mirror slope' + stit + etit + vtit, _extra = _e
		window, wwin+1
		Scan_show, shp, xtit= 'mm', ytit= '!7l!xm', ymargin = [4,6], $
		tit= 'Mirror shape' + stit + etit + vtit, _extra = _e
	endif
	if shofl and cwin ge 0 then wset, cwin
	
	return, slp
end