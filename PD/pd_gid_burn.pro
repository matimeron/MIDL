Pro PD_GID_burn, snum, fnum, angles= ang, range= ran, brange= brn, check= chk,$
	intensity= int, location= loc, width= wid, last= las, result= res, _extra=_e

;+
; NAME:
;		PD_GID_BURN
; VERSION:
;		8.421
; PURPOSE:
;		Analyzes GID "burn test" data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_GID_BURN, SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
; 	/ANGLES
; 		Switch.  If set, data is read as function of the dth angle.  Default is
; 		reading as function of Qxy.
;	RANGE
;		Two element vector, the range of the independent variable (either Qxy or
;		angle, to analyze.
;	BRANGE
;		Scalar or vector setting the range for background subtraction (see
;		PEAK_STRIP for details).  Default value is 3.
;	/CHECK
;		Switch.  If set, the first integrated frame of SNUM is displayed, with
;		RANGE marked in red, and the user is asked to confirm the RANGE 
;		selection or (if not confirming) to enter a new selection.  This 
;		continues till the user enters 'yes' (by typing any character other than
;		'N'.  Note that if RANGE wasn't entered, CHECK has no effect.
;	/INTENSITY													|
;		Switch.  If set, net intensity (counts) within RANGE	|	No more than
;		is evaluated.											|	one of these
;	/LOCATION													|	switches may
;		Switch.  If set, the peak location (relative to initial	|	be set.  If
;		location) within RANGE is evaluated.					|	none is, the
;	/WIDTH														|	default is
;		Switch.  If set, the peak width (equivalent FWHM)		|	INTENSITY.
;		within RANGE is evaluated.								|
;	/LAST
;		Switch.  If set, last read data is reused.
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Screen output only.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the data being displayed (intensity, location or width) in the
;		standard 3 column format.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		SNUM must represent proper burn data, i.e. data taken without changing
;		any of the angles.
; PROCEDURE:
; 		Uses the various PEAK routines to extract net (background subtracted)
; 		intensity, peak center or width from the specified region of interest
; 		within each frame of SNUM and displays (also, optionally, outputs) the
; 		resulting curve as function of time into the measurement.
; 		Calls PEAK_CENT, PEAK_COUNT, PEAK_SIG and PEAK_STRIP, from SPEC.  Calls
; 		SCAN_COLUMN, SCAN_FIELD_READ, SCAN_PD_FPREAD, SCAN_PD_LCOO, SCAN_SHOW
; 		and SPEC_FILE_CHECK, also from SPEC.  CALLS ARREQ, DEFAULT, LEGEND_MM,
; 		LINFIT_MM, ONE_OF, POLEVAL and STREQ, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JUN-2014 by Mati Meron as PD_BURN_TEST.
;		Modified 20-JUL-2014 by Mati Meron.  Internal changes.
;		Renamed to PD_GID_BURN 20-JUN-2015 by Mati Meron.
;		Modified 10-JUL-2015 by Mati Meron.  Added keyword CHECK.
;-

	on_error, 1

	fun = ['Peak_count','Peak_cent','Peak_sig']
	etit = ['Net Counts in Peak','Relative Peak_location','Peak Width']
	ytit = ['Counts','Relative Location','FWHM']
	if keyword_set(ang) then ytit = ytit + ['',' (deg)',' (deg)'] $
	else ytit = ytit + ['',' (A!e-1!n)',' (A!e-1!n)']

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	if Arreq(reform(Scan_field_read(wsnum,'varan')),[0,0,0,0]) then begin
		dat = Scan_PD_fpread(wsnum,fnum,read=1-keyword_set(las),/bad,/norm,$
		flist=wfnum,nfram=nfr,tit=tit)
		idat = total(dat,3)
		coo = Scan_PD_lcoo(wsnum,wfnum[0],dir=0,ang=ang,_extra=_e)
		if keyword_set(ran) then begin
			wran = ran
			chkfl = keyword_set(chk)
			repeat begin
				pran = where(coo ge min(wran) and coo le max(wran),ndum)
				if ndum eq 0 then message, 'No data in range!'
				if chkfl then begin
					chdat = transpose([[coo],[reform(idat[0,*])]])
					Scan_show, chdat, chdat[*,pran], thi= 2, /ynoz, $
					lcol= [!pcol.black,!pcol.red], _extra=_e
					wait = 0.001
					que = ''
					print
					read, que, prompt = 'OK (Y/N)?  '
					if Streq(que,'n',1) then begin
						wran = fltarr(2)
						read, wran, prompt = 'Enter [lo,hi]:  '
					endif else chkfl = 0
				endif
			endrep until chkfl eq 0
		endif else pran = lindgen(n_elements(coo))
		ccen = (coo[pran[-1]] + coo[pran[0]])/2
		cwid = (coo[pran[-1]] - coo[pran[0]])

		whi = One_of(int,loc,wid) > 0
		ifl = whi eq 0
		bran = Default(brn,3)
		res = fltarr(3,nfr)
		tim = Scan_column(wsnum,'seconds')
		res[0,*] = (total(tim,/cum))[wfnum]
		for i = 0, nfr-1 do begin
			pdat = reform(idat[i,*])
			sdat = Peak_strip(coo,pdat,sqrt(pdat),ran=pran,bran=bran)
			if whi ne 0 then begin
				dum = where(sdat[1,*] ge 0,ndum)
				if ndum ge 3 then begin
					sdat = sdat[*,dum]
					okfl = 1
				endif else okfl = 0
			endif else okfl = 1
			if okfl then begin
				res[1,i]= call_function(fun[whi],sdat,err=err,/fwhm)
				res[2,i] = err
			endif else begin
				if whi eq 1 then res[1,i] = ccen else res[1,i] = cwid
				res[2,i] = cwid/2
			endelse
		endfor
		if whi eq 1 then res[1,*] = res[1,*] - res[1,0]
	endif else message, 'Changing angle(s), not a burn test!'

	if whi eq 0 then begin
		coe = Linfit_mm(reform(res[0,*]),alog(reform(res[1,*])),$
			reform(res[1,*]/res[2,*]),err=err)
		fres = res
		fres[1,*] = exp(Poleval(fres[0,*],coe))
		fres[2,*] = 0*fres[2,*]
		tau = -1/coe[1]
		del = tau^2*err[1]
		tdig = 2 > ceil(alog10(abs(tau))) < 6
		ddig = 2 > ceil(alog10(abs(del))) < 6
		form = '("!7s!x = ",' + string(tdig+7,tdig,form='("g",i0,".",i0,",")')+$
			'" (",'+ string(ddig+7,ddig,form='("g",i0,".",i0,",")')+ '") sec.")'
		txt = strcompress(string(tau,del,form=form))
		if tau ge 0 then loc = 'UR' else loc = 'UL'
	endif

	Scan_show, res, fres, thi = 2, /ynoz, xtit = 'Time (sec)', ytit= ytit[whi],$
	tit = tit + '!c' + etit[whi], ymar = [4,4], lcol = [!pcol.blue,!pcol.cyan]
	if whi eq 0 then Legend_mm, text = txt, loc = loc, /nowrap

	return
end