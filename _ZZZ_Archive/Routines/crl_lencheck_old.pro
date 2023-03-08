Pro CRL_lencheck_old, energy = ene, rsig = rsg, asig = asg, und_length = udl, $
	elem = ele, radius = rad, thick = thi, aperture = apr, roughness = rof, $
	optloc = opl, samloc = sal, mirloc = mil, slope_err = sle, $
	hor_opt= hop, ver_only= vrt, lrange= lrn, fwhm= fwhm, emin= emi, _extra=_e

;+
; NAME:
;		CRL_LENCHECK
; VERSION:
;		8.712
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
;	OPTLOC
;		The optics (lens) distance from the source, in m.  Default is 46.7m
;		(changed from the previous 34.2m).
;	SAMLOC
;		Same as OPTLOC, for the sample.  Default is 56.m
;	MIRLOC
;		Location of the reflecting mirror(s) from the source, in m.  Used only
;		when SLOPE_ERR (see below) is provided).  Default is 32.5m.  The value
;		is always forced to be <= OPTLOC.
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
;		PQR_PROP from OPTICS. Calls ARREQ, DEFAULT, ERRORF_MM, HOW_MANY,
;		IMAGINARY_MM, LINFIT_MM, MAKE_GRID, MAKE_RANGE, NULLIFY, POLEVAL,
;		REAL_MM, STREQ and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-DEC-2018 by Mati Meron, through surgery on CRL_EVAL.
;		Modified 10-JAN-2019 by Mati Meron.  Changed default sample location
;		from 56.5 to 56.0m.
;-

	on_error, 1

	eps = Toler()
	fcon = float(!srcon.conv)
	if n_elements(ene) eq 1 then k = 2*!pi*ene/fcon $
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

	ulen = udl/(2*!pi)
	rcor = [ulen,1/ulen]/(2e4*k)
	rsgsq = rsg^2 + rcor[0]
	asgsq = asg^2 + rcor[1]

	ele = Default(ele,'Al',/dtyp)
	rad = Default(rad,0.2,/dtyp)
	if Streq(ele,'Be') then thi = Default(thi,0.1) else thi = Default(thi,0.02)
	apr = Default(apr,2*sqrt(rad),/dtyp)
	rof = Default(rof,3000.,/dtyp)

	opl = Default(opl,46.7,/dtyp)
	sal = Default(sal,56.,/dtyp) > opl
	l0 = opl
	l1 = sal - opl

	orsgsq = rsgsq + (l0+l1)^2*asgsq
	oasgsq = asgsq

	psqi = rsgsq*asgsq
	qsqi = rsgsq
	rvli = 0*asgsq
	pqri = [psqi,qsqi,rvli]

	if n_elements(sle) gt 0 then begin
		wsle = 1e-3*sle
		if tfl then wsle = [0,wsle]
		lm = (mil = (-l1) > Default(mil,32.5,/dtyp) < l0)
		if lm le 0 then begin
			lm = l1 + lm
			mifl = -1
		endif else mifl = 1
	endif else mifl = 0

	if mifl eq 1 then begin
		pqrj = PQR_prop(pqri,l=lm)
		pqr0 = PQR_prop(pqrj,s=wsle,l=l0-lm,rpsq=psq,rqsq=qsq,rr=rvl)
	endif else pqr0 = PQR_prop(pqri,l=l0,rpsq=psq,rqsq=qsq,rr=rvl)

	if Streq(ele,'C') then die = Dielect(ene,elem=ele,den=3.5156) $
	else die = Dielect(ene,elem=ele)
	alp = Real_mm(die)
	mu = -1e7*k*Imaginary_mm(die)

	sfoc = 1e-3*rad/alp
	ofoc = 1./(1./(l1 > eps) + rvl/qsq)
	rat = sfoc/ofoc
	lens = round(rat)
	if keyword_set(hop) then lens = lens[0] else lens = lens[tfl]

	wlrn = Default(lrn,1,/dtyp)
	case n_elements(wlrn) of
		1	:	wlrn = abs(wlrn)*[-1,1]
		2	:	wlrn = [min(wlrn,max=max),max]
		else:	message, 'Range needs at most 2 elements!'
	endcase
	wlens = lens + Make_range(wlrn,ner=nl)
	wrsg = (wasg = fltarr(1+tfl,nl))

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
	
		wrsg[*,i] = pqr1[1+tfl:1+2*tfl]
		wasg[*,i] = (pqr1[0:tfl] + pqr1[2+2*tfl:2+3*tfl]^2)/pqr1[1+tfl:1+2*tfl]

		if vfl then begin
			wrsg[0,i] = orsgsq[0]
			wasg[0,i] = oasgsq[0]
		endif
	endfor

	ffl = keyword_set(fwhm)
	styp = [' (rms)',' (fwhm)']
	if ffl then mult = 1e3*sqrt(alog(256)) else mult = 1e3
	wrsg = mult*sqrt(wrsg)
	wasg = mult*sqrt(wasg)

	xtit = '# of lenses'
	ytit = '!7l!xm' + styp[ffl]
	if tfl then pref = ['Hor. ', 'Ver. '] else pref = 'Beam '
	pele = ['Be','Al','C']
	dele = ['Be','Al','Dia.']
	shele = dele[where(Streq(ele,pele))]
	optloc = string(opl,form='(f5.2,"m")')
	samloc = string(sal,form='(f5.2,"m")')
	undlen = string(udl,form='(f4.2,"m")')
	energy = string(ene,form='(f6.3,"keV")')
	if whi mod 4 eq 0 then mtit = '' else mtit = ' (mod.)'

	if vfl then lenstr = '1D ' else lenstr = '2D '
	lenstr = lenstr + shele + ' lens. at ' + optloc
	gtit = ['size at ' + samloc + ', ' + lenstr + ', ' + energy, $
		 !blpar.desc + mtit + ', undulator length ' + undlen]
	if mifl then begin
		mirloc = string(lm + (1-mifl)/2*l0,form='(f5.2,"m")')
		slerr = string(sle,form='(f5.2,"A (rms) slope error")')
		gtit = [gtit,'HR mirror at ' + mirloc+ ', ' + slerr]
		ymar = [5,6]
	endif else ymar = [5,4]
	xmar = [8,3]

	owin = !d.window
	emi = []
	x = Make_grid([min(wlens,max=max),max],.01,/step)
	for i = 0, tfl do begin
		window, i
		wlens = double(wlens)
		wsize = double(reform(wrsg[i,*]))
		coe = Linfit_mm(wlens,wsize^2,ord=2)
		y = sqrt(Poleval(x,coe) > 0)
		lcen = -coe[1]/(2*coe[2])
		emi = [emi,lcen]
		stit = 'Minimum at ' + string(lcen,form='(f6.3)')
		tit = pref[i] + strjoin(gtit,'!c')
		plot, x, y, xmar= xmar, ymar= ymar, line= 2, thi= 2, $
		xtit= xtit, ytit= ytit, tit= tit, subtit= stit, _extra=_e
		oplot, wlens, wsize, psym=8, symsize=1.5, col=!pcol.red
	endfor
	if keyword_set(hop) then emi = float(emi[0]) else emi = float(emi[tfl])
	wset, owin > 0

	Nullify, whi, fir=rsg,sec=asg,thi=udl

	return
end