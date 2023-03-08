Pro DCRL_show, erange = ern, step = stp, rsig= rsg, asig= asg, und_length= udl,$
	elem = ele, radius = rad, thick = thi, aperture = apr, roughness = rof, $
	sorloc = sol, optloc = opl, samloc = sal, mirloc = mil, slope_err = sle, $
	collimate = clm, hor_opt= hop, ver_only= vrt, flux_opt= fop, size_opt= sop,$
	reduce = red, fwhm = fwhm, _extra = _e

;+
; NAME:
;		DCRL_SHOW
; VERSION:
;		8.712
; PURPOSE:
;		Evaluates the performance of CRL lenses.
; CATEGORY:
;		Optics, ChemMat Specific.
; CALLING SEQUENCE:
;		DCRL_SHOW, ERANGE = ENE [, Keywords]
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
;		The value is always forced to be <= OPTLOC[0].
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
;		displayed.  Default is Sigma.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Screen output only.  Displays plots of lens transmission, horizontal
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
;		attenuation".  Calls BL_DEFAULTS and DIELECT from SRUFF.  Calls PQR_PROP
;		from OPTICS.  Calls ARREQ, DEFAULT, ERRORF_MM, HOW_MANY, IMAGINARY_MM,
;		LABELS, LEGEND_MM, MAKE_GRID, NULLIFY, ONE_OF, PLVAR_KEEP, REAL_MM,
;		STREQ and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUN-2016 by Mati Meron, as a development of CRL_SHOW.
;		Modified 5-AUG-2016 by Mati Meron.  Added keywords MIRLOC and SLOPE_ERR.
;		Modified 25-SEP-2016 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2018 by Mati Meron.  Internal changes.
;		Modified 10-JAN-2019 by Mati Meron.  Changed default sample location
;		from 56.5 to 56.0m.  Added keyword SORLOC.
;-

	on_error, 1
	lenmax = 63
	eps = Toler()

	ffl = keyword_set(fwhm)
	styp = [' (rms)',' (fwhm)']
	if ffl then mult = 1e3*sqrt(alog(256)) else mult = 1e3

	fcon = float(!srcon.conv)
	if n_elements(ern) eq 2 then begin
		ene = Make_grid(ern,Default(stp,0.01,/dtyp),/step,dim=nen)
		nen = nen[0]
	endif else message, 'Energy range needs 2 elements!'
	k = 2*!pi*ene/fcon

	dum = How_many(fir=rsg,sec=asg,thi=udl,whi=whi)
	BL_defaults, radial=rsg, angular=asg, dev_length=udl, _extra=_e

	tfl = 1
	check = [n_elements(rsg),n_elements(asg)]
	if Arreq(check,[1,1]) then tfl = 0 $
	else if not Arreq(check,[2,2]) then message, 'Bad source input!'
	vfl = keyword_set(vrt) and tfl
	hfl = keyword_set(hop) and (not vfl)

	ulen = udl/(2*!pi)
	rsgsq = (asgsq = fltarr(1+tfl,nen))
	kk = 2e4*k
	rcor = transpose([[ulen/kk],[1/ulen/kk]])
	rsgsq[0,*] = rsg[0]^2 + rcor[0,*]
	asgsq[0,*] = asg[0]^2 + rcor[1,*]
	if tfl then begin
		rsgsq[1,*] = rsg[1]^2 + rcor[0,*]
		asgsq[1,*] = asg[1]^2 + rcor[1,*]
	endif

	ele = Default(ele,'Al',/dtyp)
	rad = Default(rad,0.2,/dtyp)
	if Streq(ele,'Be') then thi = Default(thi,0.1) else thi = Default(thi,0.02)
	apr = Default(apr,2*sqrt(rad),/dtyp)
	rof = Default(rof,3000.,/dtyp)

	cfl = keyword_set(clm)
	ncfl = 1 - cfl
	sol = Default(sol,1.25,/dtyp)
	opl = Default(opl,[34.2,46.7],/dtyp)
	if n_elements(opl) eq 2 then opl = opl[sort(opl)] $
	else message, '2 optics locations required!'
	if cfl then sal= Default(sal,opl[1],/dtyp) else sal= Default(sal,56.,/dtyp)
	l0 = opl[0] - sol
	lens = lonarr(2,nen)
	f = (w = fltarr(2,nen))
	l = [opl[1],sal] - opl

	orsg = sqrt(rsgsq + sal^2*asgsq)
	oasg = sqrt(asgsq)

	psqi = rsgsq*asgsq
	qsqi = rsgsq
	rvli = 0*asgsq
	pqri = [psqi,qsqi,rvli]

	if n_elements(sle) gt 0 then begin
		wsle = 1e-3*sle
		if tfl then wsle = [0,wsle]
		rmil = (mil = Default(mil,32.5,/dtyp))
		if mil gt 0 then begin
			rmil = mil < opl[0]
			lm = rmil - sol
			mifl = 1
		endif else begin
			rmil = (sal + mil) > opl[1]
			lm = rmil - opl[1]
			mifl = -1
		endelse
	endif else mifl = 0

	if mifl eq 1 then begin
		pqrj = PQR_prop(pqri,l=lm)
		pqr0 = PQR_prop(pqrj,s=wsle,l=l0-lm,rpsq=psq0,rqsq=qsq0,rr=rvl0)
	endif else pqr0 = PQR_prop(pqri,l=l0,rpsq=psq0,rqsq=qsq0,rr=rvl0)

	die = Dielect(ene,elem=ele)
	mu = -1e7*k*Imaginary_mm(die)
	alp = Real_mm(die)
	lfoc = transpose(1e-3*rad/alp)
	if tfl then lfoc = [lfoc,lfoc]
	dval = (k*rof*alp/2)^2
	mur = transpose(mu*rad + dval)
	mud = transpose(mu*thi + dval)
	mud = [mud,mud]

	wred = Default(red,0l,/dtyp) > 0
	rfl = wred gt 0
	if keyword_set(hop) then uind = 0 else uind = tfl

	wha = abs(One_of(fop,sop))
	case wha of
		0	:	begin
					ffoc = 1./(rvl0/qsq0 + ncfl/(l[0]+l[1]))
					hvlen = round(lfoc/ffoc)
					lens[0,*] = (hvlen[uind,*] - wred) > 0
					fovr = where(lens[0,*] gt lenmax, nfovr)
					if nfovr gt 0 then begin
						lens[0,fovr] = lenmax
						rfl = 1
					endif
					f[0,*] = lfoc[0,*]/(lens[0,*] > eps)
					w[0,*] = rad/sqrt(2*(lens[0,*] > eps)*mur)
					ff0 = f[0,*]
					ww0 = w[0,*]
					if tfl then begin
						ff0 = [ff0,ff0]
						ww0 = [ww0,ww0]
					endif
					if mifl eq -1 then begin
						pqrj = PQR_prop(pqr0,w=ww0,f=ff0,l=l[0]+lm)
						pqr2 = PQR_prop(pqrj,sle=wsle,l=l[1]-lm)
					endif else pqr2 = PQR_prop(pqr0,w=ww0,f=ff0,l=l[0]+l[1])
					if ncfl then floc = 1./(1/ff0 - rvl0/qsq0) + l0
					if rfl then begin
						pqr1=PQR_prop(pqr0,w=ww0,f=ff0,l=l[0])
						qsq1 = pqr1[1+tfl:1+2*tfl,*]
						rvl1 = pqr1[2+2*tfl:2+3*tfl,*]
						sfoc = 1./(rvl1/qsq1 + ncfl/(l[1] > eps))
						hvlen = round(lfoc/sfoc)
						lens[1,fovr] = hvlen[uind,fovr] > 0
						sovr = where(lens[1,*] gt lenmax, nsovr)
						if nsovr gt 0 then begin
							lens[1,sovr] = lenmax
							message, 'Exceeding optimal focusing range!', /con
						endif
						f[1,fovr]= lfoc[uind,fovr]/(lens[1,fovr] > eps)
						w[1,fovr] = rad/sqrt(2*(lens[1,fovr] > eps)*mur[fovr])
						ff1 = f[1,*]
						ww1 = w[1,*]
						if tfl then begin
							ff1 = [ff1,ff1]
							ww1 = [ww1,ww1]
						endif
						if mifl eq -1 then begin
							pqrj = PQR_prop( $
							pqr1[*,fovr],w=ww1[*,fovr],f=ff1[*,fovr],l=lm)
							pqr2[*,fovr] = PQR_prop(pqrj,s=wsle,l=l[1]-lm)
						endif else pqr2[*,fovr] = PQR_prop( $
						pqr1[*,fovr],w=ww1[*,fovr],f=ff1[*,fovr],l=l[1])
						if ncfl then floc[*,fovr] = $
						(1./(1/ff1[*,fovr]- rvl1[*,fovr]/qsq1[*,fovr]))+ l0+l[0]
					endif else begin
						sovr = -1l
						nsovr = 0l
					endelse
				end
		1	:	begin
					pqr1 = PQR_prop(pqr0,l=l[0])
					qsq1 = pqr1[1+tfl:1+2*tfl,*]
					rvl1 = pqr1[2+2*tfl:2+3*tfl,*]
					sfoc = 1./(rvl1/qsq1 + ncfl/(l[1] > eps))
					hvlen = round(lfoc/sfoc)
					lens[1,*] = (hvlen[uind,*] - wred) > 0
					sovr = where(lens[1,*] gt lenmax, nsovr)
					if nsovr gt 0 then begin
						lens[1,sovr] = lenmax
						rfl = 1
					endif
					f[1,*] = lfoc[uind,*]/(lens[1,*] > eps)
					w[1,*] = rad/sqrt(2*(lens[1,*] > eps)*mur)
					ff1 = f[1,*]
					ww1 = w[1,*]
					if tfl then begin
						ff1 = [ff1,ff1]
						ww1 = [ww1,ww1]
					endif
					if rfl then begin
						ll = 1./(1/l[0] + ncfl/(l[1] > eps) - 1/ff1)
						chisq = qsq0/l[0]^2 + psq0/ww1^2
						ffoc = 1./(rvl0/qsq0 + 1/l[0] - ll*chisq/qsq0)
						hvlen = round(lfoc/ffoc)
						lens[0,sovr] = hvlen[uind,sovr] > 0
						fovr = where(lens[0,*] gt lenmax, nfovr)
						if nfovr gt 0 then begin
							lens[0,fovr] = lenmax
							message, 'Exceeding optimal focusing range!', /con
						endif
						f[0,sovr]= lfoc[0,sovr]/(lens[0,sovr] > eps)
						w[0,sovr] = rad/sqrt(2*(lens[0,sovr] > eps)*mur[sovr])
						ff0 = f[0,*]
						ww0 = w[0,*]
						if tfl then begin
							ff0 = [ff0,ff0]
							ww0 = [ww0,ww0]
						endif
						pqr1[*,sovr] = PQR_prop( $
						pqr0[*,sovr],w=ww0[*,sovr],f=ff0[*,sovr],l=l[0])
						qsq1 = pqr1[1+tfl:1+2*tfl,*]
						rvl1 = pqr1[2+2*tfl:2+3*tfl,*]
					endif else begin
						fovr = -1l
						nfovr = 0l
					endelse
					if mifl eq -1 then begin
						pqrj = PQR_prop(pqr1,w=ww1,f=ff1,l=lm)
						pqr2 = PQR_prop(pqrj,s=wsle,l=l[1]-lm)
					endif else pqr2 = PQR_prop(pqr1,w=ww1,f=ff1,l=l[1])
					if ncfl then floc = 1./(1/ff1- rvl1/qsq1) + l0 + l[0]
				end
	endcase

	tran = product(exp(-lens*mud),1,/pre)
	dum = where(lens[0,*] ne 0, ndum)
	if ndum gt 0 then begin
		sfac = sqrt(1 + qsq0[*,dum]/ww0[*,dum]^2)
		afac = replicate(1.,1+tfl,nen)
		afac[*,dum] = Errorf_mm(apr*sfac/sqrt(8*qsq0[*,dum]))/sfac
		if vfl then afac[0,*] = 1
		tran = tran*product(afac,1,/pre)
	endif
	dum = where(lens[1,*] ne 0, ndum)	
	if ndum gt 0 then begin
		sfac = sqrt(1 + qsq1[*,dum]/ww1[*,dum]^2)
		afac = replicate(1.,1+tfl,nen)
		afac[*,dum] = Errorf_mm(apr*sfac/sqrt(8*qsq1[*,dum]))/sfac
		if vfl then afac[0,*] = 1
		tran = tran*product(afac,1,/pre)
	endif

	trsg = sqrt(pqr2[1+tfl:1+2*tfl,*])
	tasg = sqrt(pqr2[0:tfl,*] + pqr2[2+2*tfl:2+3*tfl,*]^2)/trsg
	if vfl then begin
		trsg[0,*] = orsg[0,*]
		tasg[0,*] = oasg[0,*]
	endif
	trsg = mult*trsg
	tasg = mult*tasg

	orsg = mult*orsg
	oasg = mult*oasg
	tvgain = tran
	if cfl then begin
		if tfl then tvgain = tvgain*oasg[1,*]/tasg[1,*]
		ttgain = tvgain*oasg[0,*]/tasg[0,*]
	endif else begin
		if tfl then tvgain = tvgain*orsg[1,*]/trsg[1,*]
		ttgain = tvgain*orsg[0,*]/trsg[0,*]
	endelse

	if nfovr ge nsovr then begin
		nsec = nfovr
		sec = fovr
		nthi = nsovr
		thi = sovr
	endif else begin
		nsec = nsovr
		sec = sovr
		nthi = nfovr
		thi = fovr
	endelse
	
	bsiz = 64
	lencol = [!pcol.green,!pcol.dgreen]
	pcol = [!pcol.red,!pcol.dred,!pcol.purple]
	xtit = 'E (keV)'
	sorloc = string(sol,form='(f4.2,"m")')
	optloc = string(opl,form='(f5.2,"m and ",f5.2,"m")')
	samloc = string(sal,form='(f5.2,"m")')
	undlen = string(udl,form='(f4.2,"m")')
	ltext = ['One set', 'Both sets', 'Excessive']
	lline = [0,0,0]
	prior = ['Flux priority','Size priority']
	if whi mod 4 eq 0 then mtit = '' else mtit = ' (mod.)'
	gtit = [!blpar.desc + mtit + ', ' + undlen + ' undulator at ' + sorloc,$
		ele + ' lenses at ' + optloc,prior[wha]]
	if cfl then gtit[1] = gtit[1] + ', collimating' $
	else gtit[1] = gtit[1] + ', focusing to ' + samloc
	if tfl then begin
		if vfl then gtit[1] = '1D ' + gtit[1] else gtit[1] = '2D ' + gtit[1]
		if hfl then gtit[2] = gtit[2] + ', hor. optimized' $
		else gtit[2] = gtit[2] + ', ver. optimized'
	endif
	if mifl then begin
		mirloc = string(rmil,form='(f5.2,"m")')
		slerr = string(sle,form='(f5.2,"A (rms) slope error")')
		gtit = [gtit[0:1],'HR mirror at ' + mirloc+ ', ' + slerr,gtit[2]]
		phi = 0.87
		xlab = [0.5,0.5,0.5,0.5]
		ylab = [0.98,0.95,0.92,0.89]
	endif else begin
		phi = 0.90
		xlab = [0.5,0.5,0.5]
		ylab = [0.98,0.95,0.92]
	endelse

	dwin = !d.window
	Plvar_keep, act = 'sav'

	window, 0, xsi = 16*bsiz, ysi = 13*bsiz

	!p.region = [0,phi/2,0.5,phi]
	hlen = max(lens[1,*]) gt max(lens[0,*])
	plot,  ene, lens[hlen,*], /nodata, xtit= xtit, ytit= '# of lenses', $
		tit= 'Lenses required', _extra = _e
	oplot, ene, lens[0,*], col = lencol[0], thi = 2
	oplot, ene, lens[1,*], col = lencol[1], thi = 2
	Legend_mm, loc = 'lr', line = [0,0], col = lencol, thi= 2, $
	text = ['Upstream lenses','Downstream lenses']

	!p.region = [0.5,phi/2,1,phi]
	plot,  ene, tran, tit='Transmission', xtit=xtit,/nodata,/noerase, _extra=_e
	oplot, ene, tran, col = pcol[0], thi = 2
	if nsec gt 0 then begin
		oplot, ene[sec], tran[sec], col = pcol[1], thi = 2
		if nthi gt 0 then begin
			oplot, ene[thi], tran[thi], col = pcol[2], thi = 2
			lind = 2
		endif else lind = 1
	endif else lind = 0
	Legend_mm, loc='lr', line=lline[0:lind], thi=2, text=ltext[0:lind], $
	col=pcol[0:lind]

	!p.region = [0,0,0.5,phi/2]
	if cfl then tit = 'Angular flux density gain' else tit = 'Flux density gain'
	plot, ene, ttgain, tit = tit, xtit = xtit, /nodata, /noerase, _extra = _e
	oplot, ene, ttgain, col = pcol[0], thi = 2
	if nsec gt 0 then begin
		oplot, ene[sec], ttgain[sec], col = pcol[1], thi = 2
		if nthi gt 0 then begin
			oplot, ene[thi], ttgain[thi], col = pcol[2], thi = 2
			lind = 2
		endif else lind = 1
	endif else lind = 0
	Legend_mm, loc='lr', line=lline[0:lind], thi=2, text=ltext[0:lind], $
	col=pcol[0:lind]

	if tfl then begin
		!p.region = [0.5,0,1,phi/2]
		if cfl then tit = 'Vertical angular flux density gain' $
		else tit = 'Vertical flux density gain'
		plot, ene, tvgain, tit = tit, xtit = xtit, /nodata, /noerase, _extra =_e
		oplot, ene, tvgain, col = pcol[0], thi = 2
		if nsec gt 0 then begin
			oplot, ene[sec], tvgain[sec], col = pcol[1], thi = 2
			if nthi gt 0 then begin
				oplot, ene[thi], tvgain[thi], col = pcol[2], thi = 2
				lind = 2
			endif else lind = 1
		endif else lind = 0
		Legend_mm, loc='lr', line=lline[0:lind], thi=2, text=ltext[0:lind], $
		col=pcol[0:lind]
	endif

	Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.3

	if tfl then begin
		window, 1, xsi = 8*bsiz, ysi = 13*bsiz

		!p.region = [0,phi/2,1,phi]
		plot,  ene, trsg[0,*], /nodata, xtit= xtit, ytit= '!7l!xm' + styp[ffl],$
			tit = 'Horizontal size at ' + samloc, _extra = _e
		oplot, ene, trsg[0,*], col = pcol[0], thi = 2
		if nsec gt 0 then begin
			oplot, ene[sec], trsg[0,sec], col = pcol[1], thi = 2
			if nthi gt 0 then begin
				oplot, ene[thi], trsg[0,thi], col = pcol[2], thi = 2
				lind = 2
			endif else lind = 1
		endif else lind = 0
		Legend_mm, loc='lr', line=lline[0:lind], thi=2, text=ltext[0:lind], $
		col=pcol[0:lind]

		!p.region = [0,0,1,phi/2]
		plot,  ene, tasg[0,*], /nodata, xtit=xtit, ytit= '!7l!xrad'+ styp[ffl],$
			tit = 'Horizontal ang. size at ' + samloc, /noerase, _extra = _e
		oplot, ene, tasg[0,*], col = pcol[0], thi = 2
		if nsec gt 0 then begin
			oplot, ene[sec], tasg[0,sec], col = pcol[1], thi = 2
			if nthi gt 0 then begin
				oplot, ene[thi], tasg[0,thi], col = pcol[2], thi = 2
				lind = 2
			endif else lind = 1
		endif else lind = 0
		Legend_mm, loc='lr', line=lline[0:lind], thi=2, text=ltext[0:lind], $
		col=pcol[0:lind]

		Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.3
	endif

	window, 1 + tfl, xsi = 8*bsiz, ysi = 13*bsiz

	!p.region = [0,phi/2,1,phi]
	if tfl then pref = 'Vertical ' else pref = ''
	plot,  ene, trsg[tfl,*], /nodata, xtit= xtit, ytit= '!7l!xm' + styp[ffl],$
		tit = pref + 'size at ' + samloc, _extra = _e
	oplot, ene, trsg[tfl,*], col = pcol[0], thi = 2
	if nsec gt 0 then begin
		oplot, ene[sec], trsg[tfl,sec], col = pcol[1], thi = 2
		if nthi gt 0 then begin
			oplot, ene[thi], trsg[tfl,thi], col = pcol[2], thi = 2
			lind = 2
		endif else lind = 1
	endif else lind = 0
	Legend_mm, loc='lr', line=lline[0:lind], thi=2, text=ltext[0:lind], $
	col=pcol[0:lind]

	!p.region = [0,0,1,phi/2]
	plot,  ene, tasg[tfl,*], /nodata, xtit= xtit, ytit= '!7l!xrad'+ styp[ffl],$
		tit = pref + 'ang. size at ' + samloc, /noerase, _extra = _e
	oplot, ene, tasg[tfl,*], col = pcol[0], thi = 2
	if nsec gt 0 then begin
		oplot, ene[sec], tasg[tfl,sec], col = pcol[1], thi = 2
		if nthi gt 0 then begin
			oplot, ene[thi], tasg[tfl,thi], col = pcol[2], thi = 2
			lind = 2
		endif else lind = 1
	endif else lind = 0
	Legend_mm, loc='lr', line=lline[0:lind], thi=2, text=ltext[0:lind], $
	col=pcol[0:lind]

	Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.3

	Plvar_keep, act = 'res'
	wset, dwin > 0
	Nullify, whi, fir=rsg,sec=asg,thi=udl

	return
end