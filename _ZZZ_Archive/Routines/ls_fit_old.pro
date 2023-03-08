Function LS_fit_old, file, cut = cut, check = chk, show = sho, $  
	data = dat, cdata = cdat, error = err, _extra = _e

;+
; NAME:
;		LS_FIT
; VERSION:
;		8.214
; PURPOSE:
;		Fits LS spectra to Lorentzians.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		Result = LS_FIT (FILE [,keywords])
; INPUTS:
;	FILE
;		Name of the input data file.  If not given, will be querried for, 
;		interactively.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	CUT
; 		Scalar or 2-element vector specifying the fraction(s) of the data to be
; 		cut away prior to fitting.  If given as a scalar, the second value is
; 		taken to be 1.  Must have 0 <= CUT[0] < CUT[1] <=1.
; 	/CHECK
; 		Switch.  When set, the cut values can be checked and adjusted
; 		interactively.
;	/SHOW
;		Switch.  If set, the processed data, together with the fit, is 
;		displayed to the screen.
; 	DATA
; 		Optional output, see below.
; 	CDATA
; 		Optional output, see below.
; 	ERROR
; 		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Optional screen output and saved files.
; OPTIONAL OUTPUT PARAMETERS:
;	DATA
;		Returns the raw LS spectrum data, as read.
;	CDATA
;		Returns the processed LS spectrum data, cut to the CUT boundaries and
;		with fitted background subtracted.
;	ERROR
;		Returns the statistical errors of the fit parameters.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
; 		None.
; RESTRICTIONS:
; 		None other than the restrictions on the CUT values.
; PROCEDURE:
; 		Reads the data, converts from DB to linear values and fits it to a
; 		Lorentzian, with quadratic background.  
; 		Calls PEAK_FIT from BLIN and SCAN_SHOW from SPEC.  Also calls DEFAULT, 
; 		FNAMPARSE, FPU_FIX, LEGEND_MM, POLEVAL, RASCII and STREQ, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-NOV-2013 by Mati Meron.
;-

	on_error, 1

	fun = 'mlorentz_fun'
	ext = '.txt'
	rtn = string(13b)
	cols = [!pcol.red,!pcol.red,!pcol.green]
	text = ['full data','cut data','fit']
	lins = [1,0,0]
	cwin = !d.window
	if cwin ne 0 and (keyword_set(chk) or keyword_set(sho)) then window, 0

	dat = Rascii(file,file_name=ufil,filt=ext,call=2)
	fnam = Fnamparse(ufil)
	dat[0,*] = 1e-3*dat[0,*]
	dat[1,*] = 1e5*10.^(dat[1,*]/20.)
	dat[1,*] = dat[1,*] - min(dat[1,*])
	dlen = (size(dat))[2]

	wcut = 0 > ([Default(cut,0),1.])[0:1] < 1
	if wcut[0] ge wcut[1] then begin
		message, 'unacceptable CUT, ignored', /con
		wcut = [0.,1.]
	endif

	repeat begin
		dran = round((dlen-1)*wcut)
		wdat = dat[*,dran[0]:dran[1]]
		par = Peak_fit(wdat,func='lor',back=3,/fixed,error=err,sta=st,_extra=_e)
		if not st then begin
			message, rtn + 'No convergence for ' + fnam, /con
			clen = dran[1] - dran[0] + 1
			repeat begin
				wcut = wcut + 0.01*[1,-1]
				dran = round((dlen-1)*wcut)
				wdat = dat[*,dran[0]:dran[1]]
				par = Peak_fit($
					wdat,funct='lor',back=3,/fixed,error=err,sta=st,_extra=_e)
				if st then break
			endrep until (size(wdat))[2] le clen/2
			if st then message, $
			'Convergence problems for ' + fnam + ' resolved' + rtn, /con
		endif
		cdat = (cfit = wdat)
		cdat[1,*] = cdat[1,*] - Poleval(cdat[0,*],par[0:2])
		cfit[1,*] = call_function(fun,[0,0,0,par[3:*]],cfit[0,*])

		if keyword_set(chk) then begin
			ddat = dat
			ddat[1,*] = ddat[1,*] - Poleval(ddat[0,*],par[0:2])
			Scan_show, ddat, cdat, cfit, lcol = cols,line = lins, $
			thi=1.5, xthi=2, ythi=2, charthi=1.5,charsiz=1.5,xtit='f(kHz)',$
			tit= fnam + ', full, background subtracted',_extra=_e
			Legend_mm, text= text, col= cols, line= lins, thi= 2, $
			charsiz= 1.5, charthi= 1.5

			que = ''
			clo = (chi = 0.)
			prompt = string(wcut,form = $
			'("Current cut is [",f4.2,", ",f4.2,"], OK (Y/N)? ")')
			wait,0.001
			read, que, prompt = prompt
			if Streq(que,'n',1) then begin
				repeat begin
					read, clo, chi, prompt = 'Enter 2 values:'
					ok = clo ge 0 and clo lt chi and chi le 1
				endrep until ok
				wcut = [clo,chi]
				idone = 0
			endif else idone = 1
			print
		endif else idone = 1
	endrep until idone

	if keyword_set(sho) then begin
		Scan_show, cdat, cfit, lcol = cols[1:*],line = lins[1:*], $
		thi= 1.5, xthi= 2, ythi= 2, charthi= 1.5, charsiz= 1.5, $
		tit= fnam + ', background subtracted', xtit='f(kHz)',_extra=_e
		Legend_mm, text= text[1:*], col= cols[1:*], line= lins[1:*], $
		thi= 2, charsiz= 1.5, charthi= 1.5
	endif

	if cwin ne -1 then wset, cwin
	return, FPU_fix(par)
end