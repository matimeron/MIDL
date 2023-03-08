Pro NCRL_lencheck, energy = ene, rsig = rsg, asig = asg, und_length = udl, $
	elem = ele, radius = rad, thick = thi, aperture = apr, roughness = rof, $
	sorloc = sol, optloc = opl, samloc = sal, mirloc = mil, slope_err = sle, $
	hor_opt= hop, ver_only= vrt, lrange= lrn, fwhm= fwhm, emin= emi, _extra=_e

;+
; NAME:
;		CRL_LENCHECK
; VERSION:
;		8.72
; PURPOSE:
;		Evaluates near-optimum focusing of CRL lenses.
; CATEGORY:
;		Optics, ChemMat Specific.
; CALLING SEQUENCE:
;		CRL_LENCHECK, ENERGY = ENE [, Keywords]
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENERGY
;		Numeric scalar, the beam energy in keV.
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
;		The physical element from which the lens is constructed.  Acceptable
;		inputs are Al, Be and C (which stands for diamond).  Default is Al.
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
;		Same as OPTLOC, for the sample.  Default is 56.0m.
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
;	/HOR_OPT
;		Switch.  if set, horizontal focus is optimized.  Default is vertical
;		focus.
;	/VER_ONLY
;		Switch.  If set, a 1D CRL, focusing only in the vertical, is assumed.
;	LRANGE
;		Specifies the lens-number offsets from the optimal number.  Given an
;		optimal number (for the provided energy) OPLEN, actual evaluation will
;		be performed for lens-number range of:
;			1) LRANGE scalar	-	[OLEN - LRANGE, OLEN + LRANGE]
;			2) LRANGE 2-elem. vector -	[OLEN + LRANGE[0], OLEN + LRANGE[1]]
;		Other LRANGE input formats are not allowed.  If not provided, default
;		value for LRANGE is 1.
;	/FWHM
;		Switch.  If set, results for beam FWHM (spatial and angular) are
;		reported.  Default is Sigma.
;	EMIN
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Screen output only.  Prints to the screen lens transmission, horizontal
;		and vertical sizes (and angular sizes) and other parametes.  Self
;		explanatory.
; OPTIONAL OUTPUT PARAMETERS:
;	EMIN
;		Returns the energy expected to yield minimal beam size.  By default the
;		minimum is for vertical size, unless the keyword HOR_OPT is set.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Calculates using data from JJ and formalism from "Focusing with
;		attenuation".  Calls BL_DEFAULTS and DIELECT, from SRUFF.  Calls
;		PQR_PROP from OPTICS. Calls ARREQ, DEFAULT, HOW_MANY, IMAGINARY_MM,
;		LINFIT_MM, MAKE_GRID, MAKE_RANGE, NULLIFY, POLEVAL, REAL_MM, STREQ and
;		TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-DEC-2018 by Mati Meron, through surgery on CRL_EVAL.
;		Modified 10-JAN-2019 by Mati Meron.  Changed default sample location
;		from 56.5 to 56.0m.  Added keyword SORLOC.
;		Modified 20-DEC-2020 by Mati Meron.  Internal changes.
;-

	on_error, 1
	eps = Toler()

	ffl = keyword_set(fwhm)
	if ffl then mult = 1e3*sqrt(alog(256)) else mult = 1e3

	if n_elements(ene) eq 1 then k = 2*!pi*ene/float(!srcon.conv) $
	else message, 'Scalar energy input required'

	dum = How_many(fir=rsg,sec=asg,thi=udl,whi=whi)
	BL_defaults, radial=rsg, angular=asg, dev_length=udl, _extra=_e

	tfl = 1
	check = [n_elements(rsg),n_elements(asg)]
	if Arreq(check,[1,1]) then tfl = 0 $
	else if not Arreq(check,[2,2]) then message, 'Bad source input!'
	vfl = keyword_set(vrt) and tfl
	hfl = keyword_set(hop) and (not vfl)
	if vfl then hop = 0

	sol = Default(sol,1.25,/dtyp)
	opl = Default(opl,46.7,/dtyp) > sol
	sal = Default(sal,56.0,/dtyp) > opl
	l0 = opl - sol
	l1 = sal - opl

	rsgsq = $
	(Und_beamsize(ene,rsi=rsg,asi=asg,und=udl,/def,ang=casg,_extra=_e))[0:tfl]^2
	asgsq = casg[0:tfl]^2

	ele = Default(ele,'Al',/dtyp)
	rad = Default(rad,0.2,/dtyp)
	if Streq(ele,'Be') then thi = Default(thi,0.1) else thi = Default(thi,0.02)
	apr = Default(apr,2*sqrt(rad),/dtyp)
	rof = Default(rof,3000.,/dtyp)

	orsgsq = rsgsq + (l0+l1)^2*asgsq
	oasgsq = asgsq

	psqi = rsgsq*asgsq
	qsqi = rsgsq
	rvli = 0*asgsq
	pqri = [psqi,qsqi,rvli]

	if n_elements(sle) gt 0 then begin
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

	ele = Default(ele,'Al',/dtyp)
	rad = Default(rad,0.2,/dtyp)
	if Streq(ele,'Be') then thi = Default(thi,0.1) else thi = Default(thi,0.02)
	if Streq(ele,'C') then dfac = 2.068 else dfac = 1.
	apr = Default(apr,2*sqrt(rad),/dtyp)
	rof = Default(rof,3000.,/dtyp)

	die = Dielect(ene,elem=ele,dfac=dfac)
	alp = Real_mm(die)
	mu = -1e7*k*Imaginary_mm(die)
	sfoc = 1e-3*rad/alp
	ofoc = 1./(1/(l1 > eps) + rvl/qsq)
	rat = sfoc/ofoc
	if keyword_set(hop) then rat = rat[0] else rat = rat[tfl]
	lens = round(rat)

	wlrn = Default(lrn,1,/dtyp)
	case n_elements(wlrn) of
		1	:	wlrn = abs(wlrn)*[-1,1]
		2	:	wlrn = [min(wlrn,max=max),max]
		else:	message, 'Range needs at most 2 elements!'
	endcase
	wlens = lens + Make_range(wlrn,ner=nl)
	wrsgsq = (wasgsq = fltarr(1+tfl,nl))

	for i = 0, nl-1 do begin
		wfoc = sfoc/wlens[i]	
		dval = (k*rof*alp/2)^2
		mug = wlens[i]*(mu*thi + dval)
		wsq = rad^2/(2*wlens[i]*(mu*rad + dval))	

		wape = sqrt(wsq)
		if vfl then begin
			wape = [4/eps,wape]
			wfoc = [4/eps,wfoc]
		endif
	
		if mifl eq -1 then begin
			pqrj = PQR_prop(pqr0,w=wape,f=wfoc,l=lm)
			pqr1 = PQR_prop(pqrj,s=wsle,l=l1-lm)
		endif else pqr1 = PQR_prop(pqr0,w=wape,f=wfoc,l=l1)
	
		wrsgsq[*,i] = pqr1[1+tfl:1+2*tfl]
		wasgsq[*,i] =(pqr1[0:tfl] + pqr1[2+2*tfl:2+3*tfl]^2)/pqr1[1+tfl:1+2*tfl]

		if vfl then begin
			wrsgsq[0,i] = orsgsq[0]
			wasgsq[0,i] = oasgsq[0]
		endif
	endfor

	ffl = keyword_set(fwhm)
	styp = [' (rms)',' (fwhm)']
	if ffl then mult = 1e3*sqrt(alog(256)) else mult = 1e3
	wrsg = mult*sqrt(wrsgsq)
	wasg = mult*sqrt(wasgsq)

	xtit = '# of lenses'
	ytit = '!7l!xm' + styp[ffl]
	if tfl then pref = ['Hor. ', 'Ver. '] else pref = 'Beam '
	pele = ['Be','Al','C']
	dele = ['Be','Al','Dia.']
	shele = dele[where(Streq(ele,pele))]
	sorloc = string(sol,form='(f4.2,"m")')
	optloc = string(opl,form='(f5.2,"m")')
	samloc = string(sal,form='(f5.2,"m")')
	undlen = string(udl,form='(f4.2,"m")')
	energy = string(ene,form='(f6.3,"keV")')
	if whi mod 4 eq 0 then mtit = '' else mtit = ' (mod.)'

	if vfl then lenstr = '1D ' else lenstr = '2D '
	lenstr = lenstr + shele + ' lens. at ' + optloc
	gtit = ['size at ' + samloc + ', ' + lenstr + ', ' + energy, $
		 !blpar.desc + mtit + ', ' + undlen + ' undulator at ' + sorloc]
	if mifl then begin
		mirloc = string(rmil,form='(f5.2,"m")')
		slerr = string(sle,form='(f5.2,"A (rms) slope error")')
		gtit = [gtit,'HR mirror at ' + mirloc+ ', ' + slerr]
		ymar = [5,6]
	endif else ymar = [5,4]
	xmar = [8,3]

	owin = !d.window
	lmi = []
	x = Make_grid([min(wlens,max=max),max],.01,/step)
	for i = 0, tfl do begin
		window, i
		wlens = double(wlens)
		wsize = double(reform(wrsg[i,*]))
		coe = Linfit_mm(wlens,wsize^2,ord=2)
		y = sqrt(Poleval(x,coe) > 0)
		lcen = -coe[1]/(2*coe[2])
		lmi = [lmi,lcen]
		stit = 'Minimum at ' + string(lcen,form='(f6.3)')
		tit = pref[i] + strjoin(gtit,'!c')
		plot, x, y, xmar= xmar, ymar= ymar, line= 2, thi= 2, $
		xtit= xtit, ytit= ytit, tit= tit, subtit= stit, _extra=_e
		oplot, wlens, wsize, psym=8, symsize=1.5, col=!pcol.red
	endfor
	wset, owin > 0

	if keyword_set(hop) then lmi = float(lmi[0]) else lmi = float(lmi[tfl])
	Crl_transfeq, fene=ene,floc=l1,flen=lmi,sene=emi,sloc=l1,slen=lens,get='ene'
	emi = [lens,emi]

	Nullify, whi, fir=rsg,sec=asg,thi=udl

	return
end