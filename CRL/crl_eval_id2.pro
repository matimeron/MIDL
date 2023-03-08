Pro CRL_eval_ID2, energy = ene, rsig = rsg, asig = asg, und_length = udl, $
	elem = ele, radius = rad, thick = thi, aperture = apr, roughness = rof, $
	sorloc = sol, optloc = opl, samloc = sal, mirloc = mil, slope_err = sle, $
	collimate = clm, hor_opt = hop, ver_only = vrt, fwhm= fwhm, $
	down_shift = dws, up_shift = ups, range = ran, _extra = _e

;+
; NAME:
;		CRL_EVAL
; VERSION:
;		8.72
; PURPOSE:
;		Evaluates the performance of CRL lenses.
; CATEGORY:
;		Optics, ChemMat Specific.
; CALLING SEQUENCE:
;		CRL_EVAL, ENERGY = ENE [, Keywords]
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
;	/COLLIMATE
;		Switch.  If set, the lens is optimized for collimation.  Default is
;		focusing.
;	/HOR_OPT
;		Switch.  if set, horizontal focus is optimized.  Default is vertical
;		focus.
;	/VER_ONLY
;		Switch.  If set, a 1D CRL, focusing only in the vertical, is assumed.
;	/FWHM
;		Switch.  If set, results for beam FWHM (spatial and angular) are
;		reported.  Default is Sigma.
;	DOWN														|
;		If given, the calculated optimal number of lenses is	|	These two
;		shifted down by DOWN (but not below 1).					|	keywords are
;	UP															|	mutually
;		If given, the calculated optimal number of lenses is	|	exclusive.
;		shifted up by UP.										|
;	RANGE
;		If provided, plots of horizontal and vertical beam sizes in the vicinity
;		of the sample location are generated.  If given as a scalar, the plots
;		are within [SAMLOC - RANGE, SAMLOC + RANGE].  If given as a 2-element
;		vector, plot ranges are [SAMLOC +  RANGE[0], SAMLOC + RANGE[1]].
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
;		attenuation".  Calls BL_DEFAULTS, DIELECT and UND_BEAMSIZE, from SRUFF.
;		Calls PQR_PROP from OPTICS. Calls ARREQ, CAST, DEFAULT, ERRORF_MM,
;		HOW_MANY, IMAGINARY_MM, ISNUM, LABELS, MAKE_GRID, NULLIFY, ONE_OF,
;		PLVAR_KEEP, REAL_MM, STREQ, TOLER and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAR-2010 by Mati Meron.
;		Updated 20-SEP-2011 by Mati Meron.  Added keyword VER_ONLY.
;		Modified 20-SEP-2015 by Mati Meron.  Added keyword ALTERNATE.
;		Updated 5-NOV-2015 by Mati Meron.  Default parameters values update.
;		Modified 15-DEC-2015 by Mati Meron.  Added keyword UND_LEN and display
;		of source name to screen.  Also added evaluation of total and vertical
;		flux gain.  Eliminated keyword LAM which was never used.
;		Modified 20-MAY-2016 by Mati Meron.  Internal changes.
;		Modified 20-JUN-2016 by Mati Meron.  Adapting the PQR algorithm.
;		Modified 5-AUG-2016 by Mati Meron.  Added keywords MIRLOC and SLOPE_ERR.
;		Modified 25-SEP-2016 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2018 by Mati Meron.  Internal changes.
;		Modified 30-MAR-2018 by Mati Meron.  Added keywords UP, DOWN and RANGE.
;		Added optional beam size plotting.
;		Modified 20-APR-2018 by Mati Meron.  Internal changes.
;		Modified 25-APR-2018 by Mati Meron.  Eliminated keyword ALTERNATE.
;		Modified 30-SEP-2018 by Mati Meron  Added diamond as possible lens
;		element.
;		Modified 20-NOV-2018 by Mati Meron.  Enabled UP and DOWN shifts by more
;		than one lens.
;		Modified 10-JAN-2019 by Mati Meron.  Changed default sample location
;		from 56.5 to 56.0m.  Added keyword SORLOC.
;		Modified 25-APR-2019 by Mati Meron.  Included diffractive effects.
;		Modified 25-JAN-2020 by Mati Meron.  Internal changes.
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

	cfl = keyword_set(clm)
	ncfl = 1 - cfl
	sol = Default(sol,1.25,/dtyp)
	opl = Default(opl,34.2,/dtyp) > sol
	if cfl then sal = Default(sal,opl,/dtyp) > opl $
	else sal = Default(sal,42.9,/dtyp) > opl
	l0 = opl - sol
	l1 = sal - opl

	rsgsq = $
	(Und_beamsize(ene,rsi=rsg,asi=asg,und=udl,/def,ang=casg,_extra=_e))[0:tfl]^2
	asgsq = casg[0:tfl]^2

	orsg = sqrt(rsgsq + (l0+l1)^2*asgsq)
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

	die = Dielect(ene,elem=ele,dfac=dfac)
	alp = Real_mm(die)
	mu = -1e7*k*Imaginary_mm(die)
	sfoc = 1e-3*rad/alp
	ofoc = 1./(ncfl/(l1 > eps) + rvl/qsq)
	rat = sfoc/ofoc
	if keyword_set(hop) then rat = rat[0] else rat = rat[tfl]
	lens = round(rat)

	case One_of(dws,ups) of
		0	:	begin
					wdws = lens - ((lens - abs(dws)) > 1)
					if wdws gt 0 then begin
						lens = lens - wdws
						adjstr=string(wdws,form='(" (down shifted by ",i0,")")')
					endif else adjstr = ''
				end
		1	:	begin
					wups = abs(ups)
					lens = lens + wups
					adjstr = string(wups,form='(" (up shifted by ",i0,")")')
				end
		else:	adjstr = string(rat,form='(" (",f7.3," calculated)")')
	endcase

	wfoc = sfoc/lens
	if not cfl then floc = 1/(1/wfoc - rvl/qsq) + opl
	if lens eq 1 then slen = ' lens' else slen = ' lenses'

	dval = (k*rof*alp/2)^2
	mug = lens*(mu*thi + dval)
	wsq = rad^2/(2*lens*(mu*rad + dval))
	sfac = sqrt(1 + qsq/wsq)
	afac = Errorf_mm(apr*sfac/sqrt(8*qsq))/sfac
	if vfl then afac[0] = 1
	tran = exp(-mug)*product(afac)

	wape = sqrt(wsq)
	if vfl then begin
		wape = [4/eps,wape]
		wfoc = [4/eps,wfoc]
	endif

	if mifl eq -1 then begin
		pqrj = PQR_prop(pqr0,w=wape,f=wfoc,l=lm,ene=ene)
		pqr1 = PQR_prop(pqrj,s=wsle,l=l1-lm)
	endif else pqr1 = PQR_prop(pqr0,w=wape,f=wfoc,l=l1,ene=ene)

	trsg = sqrt(pqr1[1+tfl:1+2*tfl])
	tasg = sqrt(pqr1[0:tfl] + pqr1[2+2*tfl:2+3*tfl]^2)/trsg

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

	styp = [' (rms)',' (fwhm)']
	head = ['Generic Focus','Horizontal Focus','Vertical Focus']
	if vfl then head[1] = 'Horizontal Parameters'
	pele = ['Be','Al','C']
	dele = ['Be','Al','Dia.']
	shele = dele[where(Streq(ele,pele))]

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
	print
	print, 'Source at	- ', sol, form = '(t9,a,f7.3," m")'
	print, 'Optic at	- ', opl, form = '(t9,a,f7.3," m")'
	print, 'Sample at	- ', sal, form = '(t9,a,f7.3," m")'
	if mifl then begin
		print, 'HR mirror at	- ', rmil, form='(t9,a,f7.3," m")'
		print, 'HR slope error	-', sle, form = '(t9,a,f8.3," microrad (rms)")' 
	endif
	print, ene,lens,shele, slen,adjstr,form='(t9,f5.2," keV, ",8x,i0," ",a,a,a)'
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
			print, 'Optimal focal length	= ', ofoc[i],form='(t9,a,f7.3," m")'
			print, 'Working focal length	= ',wfoc[-1],form='(t9,a,f7.3," m")'
			if ncfl then $
			print, 'Actual focus at		= ', floc[i], form = '(t9,a,f7.3," m")'
		endif
		print, 'Spatial size at sample	= ', mult*trsg[i], styp(ffl), $
			form = '(t9,a,f7.2," micron",a)'
		print, 'Angular size at sample	= ', mult*tasg[i], styp(ffl), $
			form = '(t9,a,f7.2," microrad",a)'
		print
	endfor

	if Isnum(ran) then begin
		bsiz = 64
		case n_elements(ran) of
			1	:	wran = abs(ran)*[-1.,1.]
			2	:	wran = Cast([min(ran,max=max),max],4)
			else:	message, 'Range needs at most 2 elements!'
		endcase
		if mifl eq -1 then lcheck = l1 - lm else lcheck = l1
		if -wran[0] gt lcheck then message, 'Excessive range!'
		rran = Make_grid(wran,256 > 64*ceil(wran[1]-wran[0]) < 512,dim=len)
		rl1 = l1 + rran
		rpqr = make_array(n_elements(pqr1),len,typ=Type(pqr1))
		if mifl eq -1 then begin
			pqrk = PQR_prop(pqrj,s=wsle)
			rl1 =  rl1 - lm
		endif else pqrk = PQR_prop(pqr0,w=wape,f=wfoc,ene=ene)
		for i = 0, len-1 do rpqr[*,i] = PQR_prop(pqrk,l=rl1[i])
		rrsg = mult*sqrt(rpqr[1+tfl:1+2*tfl,*])

		sorloc = string(sol,form='(f4.2,"m")')
		optloc = string(opl,form='(f5.2,"m")')
		samloc = string(sal,form='(f5.2,"m")')
		undlen = string(udl,form='(f4.2,"m")')
		if whi mod 4 eq 0 then mtit = '' else mtit = ' (modified)'
		gtit = [!blpar.desc + mtit + ', ' + undlen + ' undulator at ' + sorloc,$
		ele + ' lenses at ' + optloc]
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
		endif
		xtit = 'Location relative to sample (m)'
		ytit= '!7l!xm' + styp[ffl]

		Plvar_keep, act = 'sav'
		if tfl then begin
			if mifl then begin
				phi = 0.90
				xlab = [0.5,0.5,0.5]
				ylab = [0.98,0.95,0.92]
			endif else begin
				phi = 0.93
				xlab = [0.5,0.5]
				ylab = [0.98,0.95]
			endelse
			window, 0, xsi = 8*bsiz, ysi = 13*bsiz
			!p.region = [0,phi/2,1,phi]
			plot, rran, rrsg[0,*], tit= 'Horizontal size', $
			xtit= xtit, ytit= ytit, _extra= _e
			!p.region = [0,0,1,phi/2]
			plot, rran, rrsg[1,*], /noerase, tit= 'Vertical size', $
			xtit= xtit, ytit= ytit, _extra= _e
			Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.3
		endif else begin
			if mifl then begin
				phi = 0.83
				xlab = [0.5,0.5,0.5]
				ylab = [0.96,0.91,0.86]
			endif else begin
				phi = 0.87
				xlab = [0.5,0.5]
				ylab = [0.96,0.91]
			endelse
			window, 1, xsi = 8*bsiz, ysi = 7*bsiz
			!p.region = [0,0,1,phi]
			plot, rran, rrsg[0,*], tit= 'Beam size', $
				xtit= xtit, ytit= ytit, _extra= _e
			Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.3
		endelse
		Plvar_keep, act = 'res'
	endif

	Nullify, whi, fir=rsg,sec=asg,thi=udl

	return
end