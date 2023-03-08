Pro Reffit, ref, range = ran, $
	qcrit = qcr, kmu = kmu, qoff = qof, rough = rof, norm_mult = nrm, $
	cor_rough = cor, cor_norm = con, no_shift = nsh, wnew = wnw, $
	result = res, parameters = fpar, _extra = _e

;+
; NAME:
;		REFFIT
; VERSION:
;		8.33
; PURPOSE:
;		Fits reflectivity data to Fresnel reflectivity
;		(plus roughness, normalization and q-offset).
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		REFFIT, REF, keywords
; INPUTS:
;	REF
;		Reflectivity data, in the standard [3,*] array format.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RANGE
;		An optional 2 element vector setting lower and upper limit for Q-range
;		for evaluation.  If only one value is given, it is taken as the lower
;		limit, with the upper limit given by the data.
;	QCRIT
;		Optional value for the critical momentum transfer,	|	If any of these
;		Q_crit used in the fitting.							|	values is given,
;	KMU														|	the
;		Optional value for k*mu value used in the fitting.	|   corresponding
;	QOFF													|	fitting
;		Optional value for the Qz offset, used in the 		|	parameter is
;		fitting.											|	"frozen" at the
;	ROUGH													|	value provided.
;		Optional value for the roughness value used in the	|
;		fitting.											|
;	NORM_MULT												|
;		Optional value for normalization coefficient used 	|
;		in the fitting.										|
;	/CORR_ROUGH
;		Switch.  If set, the optional output RESULT is corrected for roughness.
;		See RESULT for more details
;	/CORR_NORM
;		Switch.  If set, the optional output RESULT is corrected for 
;		normalization.  See RESULT for more details
;	/NO_SHIFT
;		Switch.  If set, the optional output RESULT is *not* corrected for
;		Qz shift.  See RESULT for more details.
;	/WNEW
;		Switch.  If set, REFFIT opens a new plot window instead of plotting 
;		over the previous one.  Up to 4 windows can be used.
;	RESULT
;		Optional output, see below.
;	PARAMETERS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to SCAN_SHOW.
;		Not to be used directly.
; OUTPUTS:
;		None, other than screen output.  This one is in the form of a panel
;		containing:
;		
;		Top left	:	Plot of data scaled by the fit results.
;		Top right	:	The fit parameters.
;		Bottom left	:	Plot of raw data and fit.  Note: the raw data is 
;						q-shifted.
;		Bottom right:	Plot of the processed data returned in RESULT.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the input data scaled by fitted Fresnel reflectivity in the 
;		standard [3,*] form:
;
;		Column 0	:	QZ + QZ_offset.
;		Column 1	:	Reflectivity divided by fitted reflectivity.
;		Column 2	:	Scaled errors.
;
;		The result can be modified using keywords, as follows:
;		
;		If CORR_ROUGH is set, Result is also scaled (divided) by the roughness
;		factor.
;		If CORR_NORM is set, the result is renormalized by the calculated
;		normalization factor.
;		If NO_SHIFT is set, column 0 above contains the original Qz values, 
;		with no shift.
;	PARAMETERS
;		Vector of length 5, containing the calculated fit parameters, in order:
;			0	:	Qc
;			1	:	k*mu (proportional to the imaginary part of the refractive
;					index).
;			2	:	Qz offset, the calculated offset to be applied to data Qz.
;			3	:	Roughness (in Angstrem).
;			4	:	Calculated normalization coefficient (to multiply the data
;					with for proper normalization).
; COMMON BLOCKS:
;		REFFIT_KEEP, contains the last window number.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Nonlinear fit to Fresnel reflectivity, including roughness and possible
;		Qz offset and normalization error.  Calls CAST, DEFAULT, DTOF, FGH_EXT,
;		HOW_MANY, JOIN_XY, LABELS, LEGEND_MM, PLVAR_KEEP, REFFIT_FUN, SPLIT_XY,
;		TOLER and WHERINSTRUCT, from MIDL.  Also calls PEAK_INT, SCAN_OFFSET, 
;		SCAN_PATCH_SHOW and SCAN_SCALE, from SPEC.
; MODIFICATION HISTORY:
;		Created 15-AUG-2003 by Mati Meron.
;		Completely rewritten 15-JAN-2004 by Mati Meron.
;		Modified 20-APR-2004 by Mati Meron.  Changed internal data format.
;		Modified 25-JUN-2004 by Mati Meron.  Added keyword PLAIN.
;		Modified 20-JUL-2011 by Mati Meron.  Internal changes.
;		Modified 15-OCT-2011 by Mati Meron.  Partially rewritten, changed 
;		display format.  Removed keywords SHOW_FIT, PLAIN and CORRECT_OFF which
;		were not needed.  Added keywords COR_ROUGH, COR_NORM, NO_SHIFT and WNEW.
;		Modified 10-FEB-2012 by Mati Meron.  Plotting changes.
;		Modified 20-OCT-2014 by Mati Meron.  Internal changes.
;-

	common reffit_keep, lwin

	on_error, 1
	fnam = 'reffit_fun'

	nmin = (pnum = 5)
	eps = Toler()
	n = Split_xy(ref,x=q,y=r,z=ser,inpz=inz)
	if n gt 0 then begin
		if not inz then begin
			ser = fltarr(n)
			tref = Join_xy(q,r,ser)
		endif else tref = ref
		qran = ([Default(ran,0.,/dtyp),(machar()).xmax])[0:1]
		bad = where(r le ser, nbad, comp = good, ncomp = ngood)
		if ngood gt 0 then begin
			dum = where(q[good] ge qran[0] and q[good] le qran[1], ndum)
			if ndum ge nmin then wref = tref[*,good[dum]] $
			else message, 'Not enough data points to proceed!'
		endif else message, 'No valid data points!'

		wn = Split_xy(wref,x=wq,y=wr,z=wser)
		wdum = where(wser eq 0,nwdum,comp=cdum,ncomp=ncdum)
		if nwdum gt 0 then begin
			if ncdum gt 0 then wmin = min(wser[cdum]) else wmin = 1
			wser[wdum] = wmin
		endif
		yvl = alog(wr)
		wei=(wr/wser)^2
		feps = wn*eps

		cvl = dblarr(pnum)
		mfl = How_many(fir=qcr,sec=kmu,thi=qof,fou=rof,fif=nrm,whi=whi) gt 0
		if mfl then begin
			cnam=['qcr','kmu','qof','rof','nrm']
			mmsk = whi/(2l^indgen(pnum)) and 1
			cvl[4] = 1
			for i = 0l, pnum-1 do $
			if mmsk[i] then edum = execute('cvl[i] = ' + cnam[i])
			cvl = [cvl[0]^2,4*cvl[1],cvl[2],cvl[3]^2,alog(1/cvl[4])]
			mmsk = 1 - mmsk
		endif else mmsk = replicate(1l,pnum)

		prgfl = (Wherinstruct('prog',_e))[0] ge 0
		ipar = cvl
		if mmsk[0] then ipar[0] = (Peak_int(wref)/max(wr) + wq[0])^2
		msk = mmsk and [1,1,0,1,0]
		rpar = FGH_ext(fnam,feps,x_ini=ipar,par=wq,/min,/sca,mask=msk,val=cvl,$
		/sum,yvals=yvl,wei=wei,error=err,stat=sta,_extra=_e)
		if prgfl then print
		msk = mmsk and [1,1,0,1,1]
		rpar = FGH_ext(fnam,feps,x_ini=rpar,par=wq,/min,/sca,mask=msk,val=cvl,$
		/sum,yvals=yvl,wei=wei,error=err,stat=sta,_extra=_e)
		if prgfl then print
		msk = mmsk
		rpar = FGH_ext(fnam,feps,x_ini=rpar,par=wq,/min,/sca,mask=msk,val=cvl,$
		/sum,yvals=yvl,wei=wei,chi=chi,error=err,stat=sta,_extra=_e)

		rpar = Cast(DtoF(rpar),4,4,/fix)
		err = Cast(DtoF(err),4,4,/fix)
		rpar[1] = abs(rpar[1])
		rpar[3] = rpar[3] > 0
		fpar = [sqrt(rpar[0]>0),rpar[1]/4,rpar[2],sqrt(rpar[3]),exp(-rpar[4])]
		epar= err*[1/(2*fpar[0] > eps),1./4,1,1/(2*fpar[3] > eps),fpar[4]]
	endif else message, 'Not a scan!'
	if keyword_set(nsh) then qof = 0. else qof = fpar[2]

	owin = !d.window
	bwnum = 20l
	if keyword_set(wnw) then begin
		lwin = Default(lwin,bwnum-1)
		wnum = bwnum + ((lwin - bwnum + 1) mod 4)
	endif else wnum = Default(lwin,bwnum)
	lwin = wnum
	window, wnum, xsize = 768, ysize = 768, $
	tit = strcompress('IDL ' + string(wnum) + ' : REFFIT Results')

	Plvar_keep, act = 'sav'
	!p.multi = [0,2,2]
	!p.charsize = 1.2

	fit = exp(call_function(fnam,rpar,q))
	ofref = Scan_offset(tref,xof=qof)
	sres = Scan_scale(ofref,1/fit)
	Scan_patch_show, sres, xtit = 'Q!dz!n', tit = 'Fully scaled data', _extra = _e
	oplot, wq+qof, wr/fit[dum], thi=2

	lab = strarr(7)
	pnam = ['Qc','k*mu','Qz offset','Roughness','Norm coef']
	for i = 0, pnum-1 do lab[i] = string(pnam[i],fpar[i],epar[i], $
		form = "(a9,' = ',g12.4,'  (',g12.4,' )')")
	lab[pnum+1] = string('Chi^2', chi, form = "('  ',a9,' = ',g12.4)")
	plot, [0,1], [0,1] ,ymargin=[4,4], xstyle = 20, ysty = 20, /nodata
	Labels, -0.2, (8. - findgen(7))/10., lab

	Scan_patch_show, ofref, /ylog, thi=2, lcol=!pcol.red, xtit = 'Q!dz!n', $
	style = 0, tit = 'Raw data and fit',_extra = _e
	oplot, q+qof, fit, thi = 2, col = !pcol.green
	Legend_mm, text=['raw data','fit'],col=[!pcol.red,!pcol.green],lin=0, wid=0.5

	wpar = [1,1,1 - keyword_set(nsh),keyword_set(cor),keyword_set(con)]*rpar
	wfit = exp(call_function(fnam,wpar,q))
	res = Scan_scale(ofref,1/wfit)
	Scan_patch_show, res, thi=2, xtit = 'Q!dz!n', tit= 'Scaled data', _extra= _e

	Plvar_keep, act = 'res'
	if owin ge 0 then wset, owin

	return
end