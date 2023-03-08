Pro Detfit, dat, columns= cols, time= tim, range= ran, pileup= plp, tau= tau, $
	weight = wei, verify = ver, threshold = tre, conservative = con, $
	psym = psm, title = tit, output = oup, params = par, errors = err, $
	datres = sdat, fitres = ssdat, _extra=_e

;+
; NAME:
;		DETFIT
; VERSION:
;		8.0
; PURPOSE:
;		Verifies detector's linear response.
; CATEGORY:
;		X_ray specific.
; CALLING SEQUENCE:
;		DETFIT [,DAT] [keywords]
; INPUTS:
;	DAT
;		3 possibilities:
;			1)	Numeric scalar.  Taken as a scan number in the currently defined
;				SPEC file, from which the detector data is read (if no SPEC file
;				is defined, the userwill be asked to select one, interactively.
;			2)	A data array.  Used as is.
;			3)	Nothing.  In this case, RASCII is called to read a file.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COLUMNS
;		A 3-element vector, determines which of the data array colums are to
;		serve as "absorber number", "raw counts" and "normalization",
;		respectively.  Default is [1,5,4].  Ignored for SPEC file input.
;	TIME
;		Scalar, the counting time (in seconds).  Default is 30.
;		Ignored for SPEC file input.
;	RANGE
;		A one or two element numeric input, specifies the count range (in
;		cnt/sec) to be used.  Data out of the range is ignored.  If a single
;		number is given, it is taken as the upper limit with 0 used for the
;		lower one.  If not given at all, defaults to "full range".
;	/PILEUP
;		Switch,  If set, pileup correction is performed.
;	TAU
;		The pileup time constant.  This is only used for special testing
;		purposes (for rescaling errors), on data which has *already* been
;		corected for pileup.  Not for general users.
;	/WEIGHT
;		Switch.  If set, a weighted fitting is performed.  Default is unweighted.
;	/VERIFY
;		Switch.  If set, DETFIT checks for the possibility of missing or adeed
;		foils and corrects the data for such if their presence is indicated.
;	THRESHOLD
;		Numeric constant, sets the threshold for minimal number of misplaced
;		foils.  Default is 0.5 foil.
;	/CONSERVATIVE
;		Switch.  See ABS_VERIFY for explanation.
;	PSYM
;		Plot, symbol, default is 4
;	TITLE
;		Character string, serving as a title for the plot.  If non is given and
;		DAT is read internally, using RASCII, the data file name is used as
;		title, else there is no title.
;	/OUTPUT
;		Switch.  If set, a plot of (Input/fit - 1) is sent to the printer.
;	PARAMS
;		Optional output, see below.
;	ERRORS
;		Optional output, see below.
;	DATRES
;		Optional output, see below.
;	FITRES
;		Optional output, see below.
;	_EXTRA
;		Formal keyword, used to transfer plotting (and, optionally, fitting
;		routine) keywords.  Not to be used directly.
; OUTPUTS:
;		None other than screen output.
; OPTIONAL OUTPUT PARAMETERS:
;	PARAMS
;		Returns the fit parameters as a numeric vector with the components:
;		1)	Natural log of the full (no absorbers) intensity.
;		2)	Natural log of the pser absorber decrease (note that this value is
;			always negative.
;		3)	The time width of the pulse (only if pileup is set).
;	ERRORS
;		Returns the statistical errors of the values in PARAMS.
;	DATRES
;		Returns the actual data shown in the top plot (count rate versus
;		absorber number) in a 3 column format (counts, data, errors)
;	FITRES
;		Returns the actual data shown in the bottom plot in a 3 column format
;		(count rate, count/fit - 1, errors)
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Reads and scales the data, then proceeds with linear fit of the count
;		rate to the number of absorbers.  If /PILEUP is set, follows with a
;		nonlinear fit to find the pileup correction.  Calls ABS_VERIFY.  Calls
;		SCAN_READ, SCAN_SCALE and SCAN_SHOW from SPEC.  Calls DEFAULT, FGH_EXT,
;		ISNUM, JOIN_XY, LINFIT_MM, LEGEND_MM, OUTPUT, PLVAR_KEEP, POLEVAL, 
;		RASCII, SPLIT_XY and TOLER from MIDL.
; MODIFICATION HISTORY:
;		Created 20-FEB-2004 by Mati Meron.
;		Modified 20-APR-2004 by Mati Meron.  Data format change.
;		Corrected 15-JUN-2004 by Mati Meron.
;		Upgraded 30-OCT-2004 by Mati Meron, to read data directly from SPEC
;		files.
;		Upgraded 30-OCT-2004 by Mati Meron.  Added keywords /VERIFY and
;		THRESHOLD.
;		Modified 25-JUL-2005 by Mati Meron.  Added keyword /WEIGHT.
;		Modified 15-DEC-2005 by Mati Meron.  Display changes.
;		Modified 15-MAR-2007 by Mati Meron.  Added keywords DATRES and FITRES.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 25-AUG-2010 by Mati Meron.  Replaced LEGEND with LEGEND_MM.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	fnam = 'det_fun'
	mult = 1e6

	psm = Default(psm,4)
	plfl = keyword_set(plp)

	if n_elements(dat) eq 1 then begin
		ddat = Scan_read(dat,col=[0,-1],time=tim,_extra=_e)
		ndat = Scan_read(dat,col=[0,-2])
		ddat[2,*] = ndat[1,*]
		mint = min(tim,max=maxt)
		atim = 0.5*(mint + maxt)
		if (maxt - mint) le Toler()*atim then tim = atim $
		else message, 'Time per point should be constant!'
		wtit = fildat.name + '    scan # ' + strcompress(string(dat),/rem)
		if n_elements(tit) ne 0 then wtit = wtit + '!c' + tit
	endif else begin
		if n_elements(dat) eq 0 then begin
			dat = Rascii(file=fil)
			wtit = fil
			if n_elements(tit) ne 0 then wtit = wtit + '!c' + tit
		endif else wtit = Default(tit,'')
		tim = Default(tim,30.,/dtyp)
		cols = Default(cols,[1,5,4])
		if n_elements(cols) eq 3 then ddat = dat[cols,*] $
		else message, '3 columns required!'
	endelse

	npo = Split_xy(ddat,x=abn,y=cnt,z=nrm)
	fac = total(nrm)/(npo*tim*nrm)
	ser = fac*sqrt(cnt)
	cnt = fac*cnt
	if Isnum(tau) then ser = ser*exp(2*tau*cnt)/(1 - 2*tau*cnt)

	if Isnum(ran) then begin
		if n_elements(ran) eq 1 then wran = [0,ran] else wran = ran
		dum = where(cnt ge wran[0] and cnt le wran[1], npo)
		if npo lt 3 then message, 'Not enough points left!'
		abn = abn[dum]
		cnt = cnt[dum]
		ser = ser[dum]
	endif

	if keyword_set(ver) then begin
		vfl = Abs_verify(abn,cnt,thre=tre,con=con,thi=thi,corr=cabn)
		abn = cabn
		if not vfl then begin
			nthi = n_elements(thi)
			print
			print, '		Corrected thicknesses are:'
			print
			print, thi, form = strcompress('(' + string(nthi) + 'f10.2)',/rem)
			wtit = wtit + '    *Corrected*'
		endif else begin
			print
			print, '		No thickness corrections'
		endelse
	endif

	sdat = Join_xy(abn,cnt,ser)

	lcnt = alog(cnt/mult)
	if keyword_set(wei) then wei = (cnt/ser) else wei = replicate(1.,npo)
	par = Linfit_mm(abn,lcnt,wei,err=err,resid=chi)
	if plfl then begin
		par = FGH_ext(fnam,x_ini=[par,0],par=abn,/min,/sca,$
		/sum,yvals=lcnt,wei=wei^2,chi=chi,err=err,cov=cov,_extra=_e)
		fit = mult*exp(call_function(fnam,par,abn))
		par[2] = par[2]/mult
		err[2] = err[2]/mult
	endif else fit = mult*exp(Poleval(abn,par))
	par[0] = par[0] + alog(mult)

	pnum = 2
	fpar = [exp(par[0]),exp(-par[1])]
	epar = fpar*(err[0:1] < (machar()).xmax/fpar)
	if plfl then begin
		pnum = 3
		fpar = [fpar,1e6*par[2]]
		epar = [epar,1e6*(err[2] < 1e-6*(machar()).xmax)]
	endif
	pnam = ['I_source (phot/sec)','Factor per foil','Pulse time width (mics)']
	spnam = ['I_source (ph/s)','Factor per foil  ','Time wid. (mics)']
	print
	for i = 0, pnum-1 do print, pnam[i],fpar[i],epar[i], $
	form = "('	',a24,' = ',g14.6,'  (',g14.6,' )')"
	print
	print, 'Chi^2', chi, form = "('	',a24,' = ',g14.6)"
	print

	Plvar_keep, act = 'sav'
	!p.multi = [0,1,2]
	Scan_show, sdat, /ylog, title = wtit, psym = abs(psm), _extra = _e
	oplot, abn, fit
	lin = strarr(pnum)
	for i = 0, pnum-1 do lin[i] = spnam[i] + ' = ' + $
	strcompress(string(fpar[i],form='(g12.4)'))
	Legend_mm, loc='ll',text=lin, /now, _extra = _e
	ssdat = Scan_scale(sdat,1/fit)
	ssdat[0,*] = cnt
	ssdat[1,*] = ssdat[1,*] - 1
	Scan_show, ssdat, psym = -abs(psm), _extra = _e

	if keyword_set(oup) then begin
		comar = ['Scan_show,sdat,/ylog,/nof,title=wtit,psym=abs(psm),_extra=_e',$
				'oplot,abn,fit',$
				'Legend_mm,loc="ll",text=lin,/now,_extra=_e',$
				'Scan_show,ssdat,/nof,psym =-abs(psm),_extra=_e']
				com = strjoin(comar,'&')
		Output, com, sub = 'sdat,wtit,psm,abn,fit,lin,ssdat,_e', $
		sdat, wtit, psm, abn, fit, lin, ssdat, Default(_e,{zzzz:0}), _extra = _e
	endif
	Plvar_keep, act = 'res'

	return
end