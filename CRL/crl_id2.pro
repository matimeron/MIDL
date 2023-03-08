Pro CRL_ID2, erange = ern, step = stp, rsig= rsg, asig= asg, und_length= udl,$
	elem = ele, radius = rad, thick = thi, aperture = apr, roughness = rof, $
	selem= sele, sradius= srad, sthick= sthi, saperture= sapr, sroughness=srof,$
	sorloc = sol, optloc = opl, samloc = sal,  mirloc = mil, slope_err = sle, $
	sbounce= sbo, collimate= clm, hor_opt= hop, ver_only= vrt, limited= lim, $
	fwhm = fwhm, focloc = fcl, tranvals = etr, hfocvals = hfv, vfocvals = vfv, $
	hsize = hsi, vsize = vsi, _extra = _e

;+
; NAME:
;		CRL_SHOW
; VERSION:
;		8.72
; PURPOSE:
;		Evaluates the performance of CRL lenses.
; CATEGORY:
;		Optics, ChemMat Specific.
; CALLING SEQUENCE:
;		CRL_SHOW, ERANGE = ENE [, Keywords]
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ERANGE
;		Two element vector specifying the energy range (in keV).  Mandatory.
;	STEP
;		Scalar, the step for the energy grid used in the evaluation, in keV.
;		If not given, defaults to 0.01 (i.e 10 eV)
;	RSIG
;		Gaussian sigma of the spatial beam distribution at source, in mm.  If
;		given as scalar, it is taken as generic, if given as 2-element vector,
;		the components are taken as X and Y sigma, respectively.  Optional, if
;		not given the parameters are taken from the system variable !BLPAR.
;	ASIG
;		Same as RSIG, for the angular distribution in miliradians.
;
;		Note:	If RSIG and ASIG are provided, they must be both scalars or
;				both 2-element vectors.
;	UND_LENGTH
;		Undulator length, in m.  If not given, default value is read from !BLPAR
;	ELEM
;		The physical element from which the lens is constructed.  Currently
;		only Al Be and C (diamond) are acceptable, default is Al.
;	RADIUS
;		The radius of curvature at the tip of the lens parabola, in mm.
;		Default is 0.2mm.
;	THICK
;		The minimal (center) thickness of the lens, in mm.  Default is 0.1mm
;		for Be, 0.02mm for Al and diamond.
;	APERTURE
;		The lens aperture, in mm.  Default is 0.894mm.
;	ROUGHNESS
;		The lens roughness, in Angstrem.  Default is 3000 Angstrem.
;	SORLOC
;		The source distance from the reference point (center of straight
;		section).  Default is 1.25m.
;	OPTLOC
;		The optics (lens) distance from the reference point, in m.  Default is
;		46.7m (changed from the previous 34.2m).
;	SAMLOC
;		Same as OPTLOC, for the sample.  Default is 56.m.
;	MIRLOC
;		Location of the reflecting mirror(s) from the reference point, in m.
;		Used only when SLOPE_ERR (see below) is provided).  Default is 32.5m.
;		The value is always forced to be <= OPTLOC.
;		Alternatively, a negative value may be given, and it is interpreted as
;		the distance from the sample location.  In this case the absolute value
;		is forced to be <= (SAMLOC - OPTLOC).
;	SLOPE_ERR
;		The RMS value of the mirror slope error, in microradians.  Default is 0.
;		If provided, the broadening effect of slope errors is added to the
;		evaluation.
;	/COLLIMATE
;		Switch.  If set, the lens is optimized for collimation.  Default is
;		focusing.
;	/HOR_OPT
;		Switch.  if set, horizontal focus is optimized.  Default is vertical
;		focus.
;	/VER_ONLY
;		Switch.  If set, a 1D CRL, focusing only in the vertical, is assumed.
;	/LIMITED
;		Switch. If set, the number of lenses used is limited by a preset
;		maximal value.  Currently this value is 95.  If LIMITED is provided with
;		an integer value > 1, this value replaces the preset value.
;	/FWHM
;		Switch.  If set, results for beam FWHM (spatial and angular) are
;		displayed.  Default is Sigma.
;	FOCLOC
;		Optional output, see below.
;	TRANVALS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Screen output only.  Displays plots of lens transmission, horizontal
;		and vertical sizes (and angular sizes) and other parametes.  Self
;		explanatory.
; OPTIONAL OUTPUT PARAMETERS:
;	FOCLOC
;		Returns the focus location, relative to sample position, packed together
;		with the energy values used, in a [2,N] format.
;	TRANVALS
;		Returns the (continuous approximation) transmission values, packed
;		together with the energy values used, in a [2,N] format.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Calculates using data from JJ and formalism from "Focusing with
;		attenuation".  Calls BL_DEFAULTS, DIELECT and UND_BEAMSIZE,from SRUFF.
;		Calls PQR_PROP from OPTICS.  Calls ARREQ, DEFAULT, ERRORF_MM, HOW_MANY,
;		IMAGINARY_MM, ISNUM, LABELS, LEGEND_MM, MAKE_GRID, NULLIFY, PLVAR_KEEP,
;		REAL_MM, STREQ and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAR-2010 by Mati Meron.
;		Updated 20-SEP-2011 by Mati Meron.  Added keyword VER_ONLY. 
;		Modified 20-SEP-2015 by Mati Meron.  Added keyword ALTERNATE.
;		Updated 5-NOV-2015 by Mati Meron.  Default parameters values update.
;		Modified 20-DEC-2015 by Mati Meron.  Added keyword UND_LEN.  Removed
;		keyword LAM which was never used.  Added evaluation and display of total
;		and vertical flux density gain.
;		Modified 20-MAY-2016 by Mati Meron.  Internal changes.
;		Modified 20-JUN-2016 by Mati Meron.  Adapting the PQR algorithm.
;		Modified 5-AUG-2016 by Mati Meron.  Added keywords MIRLOC and SLOPE_ERR.
;		Modified 25-SEP-2016 by Mati Meron.  Internal changes.
;		Modified 15-MAR-2017 by Mati Meron.  Internal changes.
;		Modifed 5-MAR-2018 by Mati Meron.  Internal changes.
;		Modified 20-APR-2018 by Mati Meron.  Internal changes.
;		Modified 25-APR-2018 by Mati Meron.  Eliminated keyword ALTERNATE.
;		Added keyword FOCLOC.
;		Modified 30-SEP-2018 by Mati Meron  Added diamond as possible lens
;		element.  Added keyword TRANVALS.
;		Modified 10-JAN-2019 by Mati Meron.  Changed default sample location
;		from 56.5 to 56.0m.  Added keyword SORLOC.
;		Modified 25-APR-2019 by Mati Meron.  Included diffractive effects.
;		Modified 25-JAN-2020 by Mati Meron.  Internal changes.
;		Modified 20-DEC-2020 by Mati Meron.  Internal changes.
;-

	on_error, 1
	lenmax = 95
	eps = Toler()

	ffl = keyword_set(fwhm)
	styp = [' (rms)',' (fwhm)']
	if ffl then mult = 1e3*sqrt(alog(256)) else mult = 1e3

	if n_elements(ern) eq 2 then begin
		ene = Make_grid(ern,Default(stp,0.01,/dtyp),/step,dim=nen)
		k = 2*!pi/float(!srcon.conv)*ene
		nen = nen[0]
	endif else message, 'Energy range needs 2 elements!'

	dum = How_many(fir=rsg,sec=asg,thi=udl,whi=whi)
	BL_defaults, radial=rsg, angular=asg, dev_length=udl, _extra=_e

	tfl = 1
	check = [n_elements(rsg),n_elements(asg)]
	if Arreq(check,[1,1]) then tfl = 0 $
	else if not Arreq(check,[2,2]) then message, 'Bad source input!'
	vfl = keyword_set(vrt) and tfl
	hfl = keyword_set(hop) and (not vfl)

	cfl = keyword_set(clm)
	ncfl = 1 - cfl
	sol = Default(sol,1.25,/dtyp)
	opl = Default(opl,34.,/dtyp) > sol
	if cfl then sal = Default(sal,opl,/dtyp) $
	else sal = Default(sal,42.9,/dtyp) > opl
	l0 = opl - sol
	l1 = sal - opl
	if keyword_set(sbo) then l0 = l0 + SB_length_cor(ene)

	rsgsq = (Und_beamsize(ene,$
	rsi=rsg,asi=asg,und=udl,/def,ang=casg,_extra=_e))[0:tfl,*]^2
	asgsq = casg[0:tfl,*]^2

	orsg = sqrt(rsgsq + (l0+l1)^2*asgsq)
	oasg = sqrt(asgsq)

	psqi = rsgsq*asgsq
	qsqi = rsgsq
	rvli = 0*asgsq
	pqri = [psqi,qsqi,rvli]

	if n_elements(sle) gt 0 and not keyword_set(sbo) then begin
		wsle = 1e-3*sle
		if tfl then wsle = [0,wsle]
		rmil = (mil = Default(mil,32.5,/dtyp))
		if mil gt 0 then begin
			rmil = mil < opl
			lm = rmil - sol
			mifl = 1
		endif else begin
			rmil = (sal + mil) > opl
			lm = rmil - opl
			mifl = -1
		endelse
	endif else mifl = 0

	if mifl eq 1 then begin
		pqrj = PQR_prop(pqri,l=lm)
		pqr0 = PQR_prop(pqrj,s=wsle,l=l0-lm,rpsq=psq,rqsq=qsq,rr=rvl)
	endif else pqr0 = PQR_prop(pqri,l=l0,rpsq=psq,rqsq=qsq,rr=rvl)

	ele = Default(ele,'Be',/dtyp)
	rad = Default(rad,0.3,/dtyp)
	if Streq(ele,'Be') then thi = Default(thi,0.1) else thi = Default(thi,0.02)
	if Streq(ele,'C') then dfac = 2.068 else dfac = 1.
	apr = Default(apr,2*sqrt(rad),/dtyp)
	rof = Default(rof,3000.,/dtyp)

	if not Isnum(srad) then begin
		srad = rad
		message, 'No second radius', /cont
	endif

	if srad ge rad then begin
		srad = Cast(srad,4)
		sele = Default(sele,ele,/dtyp)
		sthi = Default(sthi,thi,/dtyp)
		sapr = Default(sapr,2*sqrt(srad),/dtyp)
		srof = Default(srof,rof,/dtyp)
	endif else message, 'Second radius must be larger than first!'

	die = Dielect(ene,elem=ele,dfac=dfac)
	alp = Real_mm(die)
	mu = -1e7*k*Imaginary_mm(die)
	sfoc = 1e-3*rad/alp

	if tfl then begin
		if hfl then hvi = 0 else hvi = 1
		roqsq = rvl[hvi,*]/qsq[hvi,*]
	endif else roqsq = rvl/qsq
	ofoc = reform(1./(ncfl/(l1 > eps) + roqsq))

	clen = sfoc/ofoc
	wfoc = ofoc
	if not cfl then $
	cfloc = reform(1./(1/wfoc - roqsq)) + opl

	dval = (k*rof*alp/2)^2
	mug = clen*(mu*thi + dval)
	wsq = rad^2/(2*clen*(mu*rad + dval))
	if tfl then begin
		wfoc = transpose([[wfoc],[wfoc]])
		wsq = transpose([[wsq],[wsq]])
	endif
	sfac = sqrt(1 + qsq/wsq)
	ptran = Errorf_mm(apr*sfac/sqrt(8*qsq))/sfac
	if vfl then ptran[0,*] = 1
	if tfl then ctran = exp(-mug)*product(ptran,1,/pre) $
	else ctran = exp(-mug)*ptran
	etr = transpose([[ene],[ctran]])

	if mifl eq -1 then begin
		pqrj = PQR_prop(pqr0,w=sqrt(wsq),f=wfoc,l=lm,ene=ene)
		pqr1 = PQR_prop(pqrj,s=wsle,l=l1-lm)
	endif else pqr1 = PQR_prop(pqr0,w=sqrt(wsq),f=wfoc,l=l1,ene=ene)

	crsg = sqrt(pqr1[1+tfl:1+2*tfl,*])
	casg = sqrt(pqr1[0:tfl,*] + pqr1[2+2*tfl:2+3*tfl,*]^2)/crsg

	if vfl then begin
		crsg[0,*] = orsg[0,*]
		casg[0,*] = oasg[0,*]
	endif
	crsg = mult*crsg
	casg = mult*casg

	CRL_adjust, clen, rad/srad, wlen = tlen, awlen = atlen

	if Isnum(lim) then begin
		if lim gt 1 then lenmax = lim
		thr = where(floor(tlen) gt lenmax, thnum)
		thfl = thnum gt 0
		tlen = tlen < lenmax + floor(srad/rad)*rad/srad
	endif else thfl = 0
	wfoc = sfoc/tlen
	if not cfl then $
	tfloc = reform(1./(1/wfoc - roqsq)) + opl
	if arg_present(fcl) then fcl = transpose([[ene],[tfloc-sal]])

	mug = atlen*(mu*thi + dval)
	wsq = rad^2/(2*tlen*(mu*rad + dval))
	if tfl then begin
		wfoc = transpose([[wfoc],[wfoc]])
		wsq = transpose([[wsq],[wsq]])
	endif
	sfac = sqrt(1 + qsq/wsq)
	ptran = Errorf_mm(apr*sfac/sqrt(8*qsq))/sfac
	if vfl then ptran[0,*] = 1
	if tfl then ttran = exp(-mug)*product(ptran,1,/pre) $
	else ttran = exp(-mug)*ptran

	if mifl eq -1 then begin
		pqrj = PQR_prop(pqr0,w=sqrt(wsq),f=wfoc,l=lm,ene=ene)
		pqr1 = PQR_prop(pqrj,s=wsle,l=l1-lm)
	endif else pqr1 = PQR_prop(pqr0,w=sqrt(wsq),f=wfoc,l=l1,ene=ene)

	trsg = sqrt(pqr1[1+tfl:1+2*tfl,*])
	tasg = sqrt(pqr1[0:tfl,*] + pqr1[2+2*tfl:2+3*tfl,*]^2)/trsg

	if vfl then begin
		trsg[0,*] = orsg[0,*]
		tasg[0,*] = oasg[0,*]
	endif
	trsg = mult*trsg
	tasg = mult*tasg
	if tfl then hfv = transpose([[ene],[reform(trsg[0,*])]])
	vfv = transpose([[ene],[reform(trsg[tfl,*])]])

	orsg = mult*orsg
	oasg = mult*oasg
	cvgain = ctran
	tvgain = ttran
	if cfl then begin
		if tfl then cvgain = cvgain*oasg[1,*]/casg[1,*]
		ctgain = cvgain*oasg[0,*]/casg[0,*]
		if tfl then tvgain = tvgain*oasg[1,*]/tasg[1,*]
		ttgain = tvgain*oasg[0,*]/tasg[0,*]
	endif else begin
		if tfl then cvgain = cvgain*orsg[1,*]/crsg[1,*]
		ctgain = cvgain*orsg[0,*]/crsg[0,*]
		if tfl then tvgain = tvgain*orsg[1,*]/trsg[1,*]
		ttgain = tvgain*orsg[0,*]/trsg[0,*]
	endelse

	bsiz = 64
	ccol = !pcol.green
	tcol= !pcol.red
	hcol = !pcol.purple
	xtit = 'E (keV)'
	pele = ['Be','Al','C']
	dele = ['Be','Al','Dia.']
	shele = dele[where(Streq(ele,pele))]
	sorloc = string(sol,form='(f5.2,"m")')
	optloc = string(opl,form='(f5.2,"m")')
	samloc = string(sal,form='(f5.2,"m")')
	undlen = string(udl,form='(f4.2,"m")')
	ltext = ['Continuous approx.','Discrete lenses']
	if whi mod 4 eq 0 then mtit = '' else mtit = ' (mod.)'
	gtit = [!blpar.desc + mtit + ', ' + undlen + ' undulator at ' + sorloc,$
	shele + ' lenses at ' + optloc]
	if cfl then gtit[1] = gtit[1] + ', collimating' $
	else gtit[1] = gtit[1] + ', focusing to ' + samloc
	if tfl then begin
		if vfl then gtit[1] = '1D ' + gtit[1] else gtit[1] = '2D ' + gtit[1]
		if hfl then gtit[1] = gtit[1] + ', hor. optimized' $
		else gtit[1] = gtit[1] + ', ver. optimized'
	endif
	if mifl then begin
		mirloc = string(rmil,form='(f5.2,"m")')
		slerr = string(sle,form='(f5.2,"A (rms) slope error")')
		gtit = [gtit,'HR mirror at ' + mirloc+ ', ' + slerr]
		phi = 0.90
		xlab = [0.5,0.5,0.5]
		ylab = [0.98,0.95,0.92]
	endif else begin
		phi = 0.93
		xlab = [0.5,0.5]
		ylab = [0.98,0.95]
	endelse

	dwin = !d.window
	Plvar_keep, act = 'sav'

	window, 0, xsi = 16*bsiz, ysi = 13*bsiz

	!p.region = [0,phi/2,0.5,phi]
	plot,  ene, tlen, /nodata, xtit= xtit, ytit= '# of lenses', $
	tit= 'Lenses required', _extra = _e
	oplot, ene, clen, col = ccol, thi = 2
	oplot, ene, tlen, col = tcol, thi = 2
	if thfl then begin
		oplot, ene[thr], tlen[thr], col = !pcol.white, thi = 2
		oplot, ene[thr], tlen[thr], col = hcol, thi = 2
	endif
	Legend_mm, loc = 'lr', line = [0,0], text = ltext, col = [ccol,tcol], thi= 2

	!p.region = [0.5,phi/2,1,phi]
	plot,  ene, ttran, tit='Transmission', xtit=xtit,/nodata,/noerase, _extra=_e
	oplot, ene, ctran, col = ccol, thi = 2
	oplot, ene, ttran, col = tcol, thi = 2
	if thfl then begin
		oplot, ene[thr], ttran[thr], col = !pcol.white, thi = 2
		oplot, ene[thr], ttran[thr], col = hcol, thi = 2
	endif
	Legend_mm, loc = 'lr', line = [0,0], text = ltext, col = [ccol,tcol], thi= 2

	!p.region = [0,0,0.5,phi/2]
	if cfl then tit = 'Angular flux density gain' else tit = 'Flux density gain'
	plot, ene, ttgain, tit = tit, xtit = xtit, /nodata, /noerase, _extra = _e
	oplot, ene, ctgain, col = ccol, thi = 2
	oplot, ene, ttgain, col = tcol, thi = 2
	if thfl then begin
		oplot, ene[thr], ttgain[thr], col = !pcol.white, thi = 2
		oplot, ene[thr], ttgain[thr], col = hcol, thi = 2
	endif
	Legend_mm, loc = 'lr', line = [0,0], text = ltext, col = [ccol,tcol], thi= 2

	if tfl then begin
		!p.region = [0.5,0,1,phi/2]
		if cfl then tit = 'Vertical angular flux density gain' $
		else tit = 'Vertical flux density gain'
		plot, ene, tvgain, tit = tit, xtit = xtit, /nodata, /noerase, _extra =_e
		oplot, ene, cvgain, col = ccol, thi = 2
		oplot, ene, tvgain, col = tcol, thi = 2
		if thfl then begin
			oplot, ene[thr], tvgain[thr], col = !pcol.white, thi = 2
			oplot, ene[thr], tvgain[thr], col = hcol, thi = 2
		endif
		Legend_mm, loc= 'lr', line= [0,0], text= ltext, col= [ccol,tcol], thi= 2
	endif

	Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.3

	if tfl then begin
		window, 1, xsi = 8*bsiz, ysi = 13*bsiz

		!p.region = [0,phi/2,1,phi]
		plot,  ene, trsg[0,*], /nodata, xtit= xtit, ytit= '!7l!xm' + styp[ffl],$
		tit = 'Horizontal size at ' + samloc, _extra = _e
		oplot, ene, crsg[0,*], col = ccol, thi = 2
		oplot, ene, trsg[0,*], col = tcol, thi = 2
		hsi = transpose([[ene],[reform(trsg[0,*])]])
		if thfl then begin
			oplot, ene[thr], trsg[0,thr], col = !pcol.white, thi = 2
			oplot, ene[thr], trsg[0,thr], col = hcol, thi = 2
		endif
		Legend_mm, loc= 'lr', line= [0,0], text= ltext, col= [ccol,tcol], thi= 2

		!p.region = [0,0,1,phi/2]
		plot,  ene, tasg[0,*], /nodata, xtit=xtit, ytit= '!7l!xrad'+ styp[ffl],$
		tit = 'Horizontal ang. size at ' + samloc, /noerase, _extra = _e
		oplot, ene, casg[0,*], col = ccol, thi = 2
		oplot, ene, tasg[0,*], col = tcol, thi = 2
		if thfl then begin
			oplot, ene[thr], tasg[0,thr], col = !pcol.white, thi = 2
			oplot, ene[thr], tasg[0,thr], col = hcol, thi = 2
		endif
		Legend_mm, loc= 'lr', line= [0,0], text= ltext, col= [ccol,tcol], thi= 2

		Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.3
	endif

	window, 1 + tfl, xsi = 8*bsiz, ysi = 13*bsiz

	!p.region = [0,phi/2,1,phi]
	if tfl then pref = 'Vertical ' else pref = ''
	plot,  ene, trsg[tfl,*], /nodata, xtit= xtit, ytit= '!7l!xm' + styp[ffl],$
	tit = pref + 'size at ' + samloc, _extra = _e
	oplot, ene, crsg[tfl,*], col = ccol, thi = 2
	oplot, ene, trsg[tfl,*], col = tcol, thi = 2
	vsi = transpose([[ene],[reform(trsg[tfl,*])]])
	if thfl then begin
		oplot, ene[thr], trsg[tfl,thr], col = !pcol.white, thi = 2
		oplot, ene[thr], trsg[tfl,thr], col = hcol, thi = 2
	endif
	Legend_mm, loc = 'lr', line = [0,0], text = ltext, col = [ccol,tcol], thi= 2

	!p.region = [0,0,1,phi/2]
	plot,  ene, tasg[tfl,*], /nodata, xtit= xtit, ytit= '!7l!xrad' + styp[ffl],$
	tit = pref + 'ang. size at ' + samloc, /noerase, _extra = _e
	oplot, ene, casg[tfl,*], col = ccol, thi = 2
	oplot, ene, tasg[tfl,*], col = tcol, thi = 2
	if thfl then begin
		oplot, ene[thr], tasg[tfl,thr], col = !pcol.white, thi = 2
		oplot, ene[thr], tasg[tfl,thr], col = hcol, thi = 2
	endif
	Legend_mm, loc = 'lr', line = [0,0], text = ltext, col = [ccol,tcol], thi= 2

	Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.3

	Plvar_keep, act = 'res'
	wset, dwin > 0
	Nullify, whi, fir=rsg,sec=asg,thi=udl

	return
end