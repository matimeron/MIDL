Pro DCRL_eval_old, energy = ene, rsig = rsg, asig = asg, und_length = udl, $
	elem = ele, radius = rad, thick = thi, aperture = apr, roughness = rof, $
	optloc = opl, samloc = sal, mirloc = mil, slope_err = sle, collimate = clm,$
	hor_opt= hop, ver_only= vrt, flux_opt = fop, size_opt = sop, reduce = red, $
	fwhm = fwhm, _extra = _e

;+
; NAME:
;		DCRL_EVAL
; VERSION:
;		8.61
; PURPOSE:
;		Evaluates the performance of CRL lenses.
; CATEGORY:
;		Optics, ChemMat Specific.
; CALLING SEQUENCE:
;		DCRL_EVAL, ENERGY = ENE [, Keywords]
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
;		The physical element from which the lens is constructed.  Currently
;		only Al and Be are acceptable, default is Al.
;	RADIUS
;		The radius of curvature at the tip of the lens parabola, in mm.
;		Default is 0.2mm.
;	THICK
;		The minimal (center) thickness of the lens, in mm.  Default is 0.1mm
;		for Be, 0.02mm for Al.
;	APERTURE
;		The lens aperture, in mm.  Default is 0.894mm.
;	ROUGHNESS
;		The lens roughness, in Angstrem.  Default is 3000 Angstrem.
;	OPTLOC
;		The optics (lens) distances from the source, in m.  If given, must be
;		a 2-element vector.  Default is [34.2, 46.7]m.
;	SAMLOC
;		The sample distance from the source.  Default is 56.m
;	MIRLOC
;		Location of the reflecting mirror(s) from the source, in m.  Used only
;		when SLOPE_ERR (see below) is provided).  Default is 32.5m.  The value
;		is always forced to be <= OPTLOC[0].
;		Alternatively, a negative value may be given, and it is interpreted as
;		the distance from the sample location.  In this case the absolute value
;		is forced to be <= (SAMLOC - OPTLOC[1]).
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
;	/FLUX_OPT
;		Switch.  If set, the optimization consists of focusing		|
;		while maximizing flux.  In this case the first (upstream)	|	At most
;		lens system is primarily used, and the downstream one is	|	one of
;		involved only when the primary is not sufficient.			|	these
;	/SIZE_OPT														|	two may
;		Switch.  If set, the optimization consists of focusing		|	be used.
;		while minimizing beam size.  In this case the second 		|	Default
;		(downstream) lens system is primarily used, and the 		|	is
;		upstream one is involved only when the primary is not 		|	SIZE_OPT
;		sufficient.													|
;	REDUCE
;		Positive integer scalar. If given, the number of lenses in the primary
;		system is reduced by the REDUCE value, with the number of lenses in the
;		secondary adjusted appropriately. 
;	/FWHM
;		Switch.  If set, results for beam FWHM (spatial and angular) are
;		reported.  Default is Sigma.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Screen output only.  Prints to the screen lens transmission, horizontal
;		and vertical sizes (and angular sizes) and other parametes.  Self
;		explanatory.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
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
;		IMAGINARY_MM, NULLIFY, ONE_OF, REAL_MM, STREQ and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUN-2016 by Mati Meron, as a development of CRL_EVAL.
;		Modified 5-AUG-2016 by Mati Meron.  Added keywords MIRLOC and SLOPE_ERR.
;		Modified 25-SEP-2016 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2018 by Mati Meron.  Internal changes.
;		Modified 20-APR-2018 by Mati Meron.  Internal changes.
;-

	on_error, 1
	lenmax = 63
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
	head = ['Generic Focus','Horizontal Focus','Vertical Focus']
	vfl = keyword_set(vrt) and tfl
	if vfl then begin
		head[1] = 'Horizontal Parameters'
		hop = 0
	endif

	ulen = udl/(2*!pi)
	rcor = [ulen,1/ulen]/(2e4*k)
	rsgsq = rsg^2 + rcor[0]
	asgsq = asg^2 + rcor[1]

	ele = Default(ele,'Al',/dtyp)
	rad = Default(rad,0.2,/dtyp)
	if Streq(ele,'Be') then thi = Default(thi,0.1) else thi = Default(thi,0.02)
	apr = Default(apr,2*sqrt(rad),/dtyp)
	rof = Default(rof,3000.,/dtyp)

	cfl = keyword_set(clm)
	ncfl = 1 - cfl
	opl = Default(opl,[34.2,46.7],/dtyp)
	if n_elements(opl) eq 2 then opl = opl[sort(opl)] $
	else message, '2 optics locations required!'
	if cfl then sal= Default(sal,opl[1],/dtyp) else sal= Default(sal,56.,/dtyp)
	l0 = opl[0]
	lens = lonarr(2)
	f = (w = fltarr(2))
	l = [opl[1],sal] - opl

	orsg = sqrt(rsgsq + sal^2*asgsq)
	oasg = sqrt(asgsq)

	psqi = rsgsq*asgsq
	qsqi = rsgsq
	rvli = 0*asgsq
	pqri = [psqi,qsqi,rvli]

	if n_elements(sle) gt 0 then begin
		wsle = 1e-3*sle
		lm = (mil = (-l[1]) > Default(mil,32.5,/dtyp) < l0)
		if lm le 0 then begin
			lm = l[1] + lm
			mifl = -1
		endif else mifl = 1
	endif else mifl = 0

	if mifl eq 1 then begin
		pqrj = PQR_prop(pqri,l=lm)
		pqr0 = PQR_prop(pqrj,s=[0,wsle],l=l0-lm,rpsq=psq0,rqsq=qsq0,rr=rvl0)
	endif else pqr0 = PQR_prop(pqri,l=l0,rpsq=psq0,rqsq=qsq0,rr=rvl0)

	die = Dielect(ene,elem=ele)
	mu = -1e7*k*Imaginary_mm(die)
	alp = Real_mm(die)
	lfoc = 1e-3*rad/alp
	dval = (k*rof*alp/2)^2

	wred = Default(red,0l,/dtyp) > 0
	rfl = wred gt 0
	if keyword_set(hop) then uind = 0 else uind = tfl

	wha = abs(One_of(fop,sop))
	case wha of
		0	:	begin
					ffoc = 1./(rvl0/qsq0 + ncfl/(l[0]+l[1]))
					hvlen = round(lfoc/ffoc)
					lens[0] = (hvlen[uind] - wred) > 0
					if lens[0] gt lenmax then begin
						lens[0] = lenmax
						rfl = 1
					endif
					f[0] = lfoc/(lens[0] > eps)
					w[0] = rad/sqrt(2*(lens[0] > eps)*(mu*rad + dval))
					pqr1 = PQR_prop(pqr0,w=w[0],f=f[0],l=l[0])
					qsq1 = pqr1[1+tfl:1+2*tfl,*]
					rvl1 = pqr1[2+2*tfl:2+3*tfl,*]
					if rfl then begin
						sfoc = 1./(rvl1/qsq1 + ncfl/(l[1] > eps))
						hvlen = round(lfoc/sfoc)
						lens[1] = hvlen[uind] > 0
						if lens[1] gt lenmax then begin
							lens[1] = lenmax
							message, 'Exceeding optimal focusing range!', /con
						endif
						f[1] = lfoc/(lens[1] > eps)
						w[1] = rad/sqrt(2*(lens[1] > eps)*(mu*rad + dval))
						if mifl eq -1 then begin
							pqrj = PQR_prop(pqr1,w=w[1],f=f[1],l=lm)
							pqr2 = PQR_prop(pqrj,s=[0,wsle],l=l[1]-lm)
						endif else pqr2 = PQR_prop(pqr1,w=w[1],f=f[1],l=l[1])
						if ncfl then floc = 1./(1/f[1] - rvl1/qsq1) + l0 + l[0]
					endif else begin
						if mifl eq -1 then begin
							pqrj = PQR_prop(pqr1,l=lm)
							pqr2 = PQR_prop(pqrj,s=[0,wsle],l=l[1]-lm)
						endif else pqr2 = PQR_prop(pqr1,l=l[1])
						if ncfl then floc = 1./(1/f[0] - rvl0/qsq0) + l0
					endelse
				end
		1	:	begin
					pqr1 = PQR_prop(pqr0,l=l[0])
					qsq1 = pqr1[1+tfl:1+2*tfl,*]
					rvl1 = pqr1[2+2*tfl:2+3*tfl,*]
					sfoc = 1./(rvl1/qsq1 + ncfl/(l[1] > eps))
					hvlen = round(lfoc/sfoc)
					lens[1] = (hvlen[uind] - wred) > 0
					if lens[1] gt lenmax then begin
						lens[1] = lenmax
						rfl = 1
					endif
					f[1] = lfoc/(lens[1] > eps)
					w[1] = rad/sqrt(2*(lens[1] > eps)*(mu*rad + dval))
					if rfl then begin
						ll = 1./(1/l[0] + ncfl/(l[1] > eps) - 1/f[1])
						chisq = qsq0/l[0]^2 + psq0/w[1]^2
						ffoc = 1./(rvl0/qsq0 + 1/l[0] - ll*chisq/qsq0)
						hvlen = round(lfoc/ffoc)
						lens[0] = hvlen[uind] > 0
						if lens[0] gt lenmax then begin
							lens[0] = lenmax
							message, 'Exceeding optimal focusing range!', /con
						endif
						f[0] = lfoc/(lens[0] > eps)
						w[0] = rad/sqrt(2*(lens[0] > eps)*(mu*rad + dval))
						pqr1 = PQR_prop(pqr0,w=w[0],f=f[0],l=l[0])
						qsq1 = pqr1[1+tfl:1+2*tfl,*]
						rvl1 = pqr1[2+2*tfl:2+3*tfl,*]
					endif
					if mifl eq -1 then begin
						pqrj = PQR_prop(pqr1,w=w[1],f=f[1],l=lm)
						pqr2 = PQR_prop(pqrj,s=[0,wsle],l=l[1]-lm)
					endif else pqr2 = PQR_prop(pqr1,w=w[1],f=f[1],l=l[1])
					if ncfl then floc = 1./(1/f[1]- rvl1/qsq1) + l0 + l[0]
				end
	endcase

	tran = product(exp(-lens*(mu*thi + dval)))
	if lens[0] gt 0 then begin
		sfac = sqrt(1 + qsq0/w[0]^2)
		afac = Errorf_mm(apr*sfac/sqrt(8*qsq0))/sfac
		if vfl then afac[0] = 1
		tran = tran*product(afac)
	endif
	if lens[1] gt 0 then begin
		sfac = sqrt(1 + qsq1/w[1]^2)
		afac = Errorf_mm(apr*sfac/sqrt(8*qsq1))/sfac
		if vfl then afac[0] = 1
		tran = tran*product(afac)
	endif

	trsg = sqrt(pqr2[1+tfl:1+2*tfl,*])
	tasg = sqrt(pqr2[0:tfl,*] + pqr2[2+2*tfl:2+3*tfl,*]^2)/trsg

	if vfl then begin
		trsg[0] = orsg[0]
		tasg[0] = oasg[0]
	endif

	if cfl then begin
		vgain = (tgain = tran*oasg[tfl]/tasg[tfl])
		if tfl then tgain = tgain*oasg[0]/tasg[0]
	endif else begin
		vgain = (tgain = tran*orsg[tfl]/trsg[tfl])
		if tfl then tgain = tgain*orsg[0]/trsg[0]
	endelse

	slen = replicate(' lenses',2)
	dum = where(lens eq 1, ndum)
	if ndum gt 0 then slen[dum] = ' lens'

	ffl = keyword_set(fwhm)
	styp = [' (rms)',' (fwhm)']
	if ffl then mult = 1e3*sqrt(alog(256)) else mult = 1e3

	sour = '	' + !blpar.desc
	if whi mod 4 ne 0 then sour = sour + ' (modified)'
	print
	print, sour
	if cfl then stit = 'Collimating' else stit = 'Focusing'
	if tfl then begin
		if vfl then stit = 'Vertical ' + stit $
		else if keyword_set(hop) then stit = stit + ', Hor. Optimized' $ 
		else stit = stit + ', Ver. Optimized'
		stit = '	' + stit
	endif else stit = '		' + stit
	print, stit
	if wha then sstit = 'Size priority' else sstit = 'Flux priority'
	sstit = '	' + sstit
	print, sstit 
	print
	print, 'Energy		- ', ene, form = '(t9,a,f7.3," keV")'
	print, 'First optic at	- ', opl[0], lens[0], slen[0], $
	form = '(t9,a,f7.3," m ;	",i2,a)'
	print, 'Second optic at	- ', opl[1], lens[1], slen[1], $
	form = '(t9,a,f7.3," m ;	",i2,a)'
	print, 'Sample at	- ', sal, form = '(t9,a,f7.3," m")'
	if mifl then begin
		print, 'HR mirror at	- ', lm+ (1-mifl)/2*(l0+l[0]), $
		form='(t9,a,f7.3," m")'
		print, 'HR slope error	-', sle, form = '(t9,a,f8.3," microrad (rms)")'
	endif
	print
	print, 'Transmission	=  ', tran, form = '(t9,a,f5.3)'
	print
	if cfl then print,'		Angular Flux Gain' else print,'		Flux Gain'
	print
	print, 'Vertical	- ', vgain, form = '(t9,a,f7.2)'
	print, 'Total		- ', tgain, form = '(t9,a,f7.2)'
	print

	for i = 0, tfl do begin
		print, '		', head[i + tfl]
		print
		if not (tfl and vfl and (i eq 0)) then begin
			if lens[0] gt 0 then $
			print, 'First focal length	= ', f[0],form='(t9,a,f7.3," m")'
			if lens[1] gt 0 then $
			print, 'Second focal length	= ', f[1], form = '(t9,a,f7.3," m")'
			if ncfl then $
			print, 'Actual focus at		= ', floc[i], form = '(t9,a,f7.3," m")'
		endif
		print, 'Spatial size at sample	= ', mult*trsg[i], styp(ffl), $
			form = '(t9,a,f7.2," micron",a)'
		print, 'Angular size at sample	= ', mult*tasg[i], styp(ffl), $
			form = '(t9,a,f7.2," microrad",a)'
		print
	endfor

	Nullify, whi, fir=rsg,sec=asg,thi=udl

	return
end