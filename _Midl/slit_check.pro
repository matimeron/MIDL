Pro Slit_check, loc= loc, step= stp, size= siz, horizontal= hor, vertical= ver,$
	energy= ene, focloc= fol, slope_err= sle, samloc= sal, fwhm= fwhm, _extra=_e

	on_error, 1
	eps = Toler()
	udl = 2.4
	mil = 32.5
	dsle = 0.8

	btyp = ['Horizontal ', 'Vertical ']
	styp = [' (rms)',' (fwhm)']
	ffl = keyword_set(fwhm)
	if ffl then mult = 1e3*sqrt(alog(256)) else mult = 1e3

	fcon = float(!srcon.conv)
	if n_elements(ene) eq 1 then k = 2*!pi*ene/fcon $
	else message, 'Scalar energy input required'

	BL_defaults, radial=rsg, angular=asg, dev_length=udl, _extra=_e

	ulen = udl/(2*!pi)
	rcor = [ulen,1/ulen]/(2e4*k)
	rsgsq = rsg^2 + rcor[0]
	asgsq = asg^2 + rcor[1]

	whi = One_of(hor,ver)
	if whi ge 0 then begin
		rsgsq = rsgsq[whi]
		asgsq = asgsq[whi]
	endif else message, 'Have to specify, horizontal or vertical!'

	if n_elements(loc) eq 2 then begin
		loc = loc[sort(loc)]
		if loc[0] lt mil or loc[1] gt sal $
		then message, 'Slit must be between mirror and sample!' $
		else wloc = Make_grid(loc,Default(stp,0.5,/dtyp),/step,dim=n)
		n = n[0]
	endif else message, 'LOC needs two elements!'
	w = siz/sqrt(2*!pi)

	psqi = rsgsq*asgsq
	qsqi = rsgsq
	rvli = 0*asgsq
	pqri = [psqi,qsqi,rvli]

	if whi then begin
		wsle = Default(sle,dsle,/dtyp)*1e-3
		pqrj = PQR_prop(pqri,l=mil)
		pqrk = PQR_sler(pqrj,sle=wsle)
		f = 1/(1./(fol-mil) + pqrk[2]/pqrk[1])
		pqr0 = PQR_prop(pqrk,f=f)
	endif else pqr0 = PQR_prop(pqri,l=mil)

	glob = fltarr(3,n)
	fir = wloc - mil
	sec = sal - wloc
	for i = 0, n-1 do begin
		pqrs = PQR_prop(pqr0,l=fir[i])
		pqrf = PQR_prop(pqrs,w=w,l=sec[i])
		glob[*,i] = pqrf
	endfor

	rsiz = mult*sqrt(glob[1,*])
	asiz = mult*sqrt((glob[0,*]+glob[2,*]^2)/glob[1,*])

	bsiz = 64
	phi = 0.93
	xlab = [0.5,0.5]
	ylab = [0.98,0.95]

	dwin = !d.window
	Plvar_keep, act = 'sav'

	gtit = ['Beam_size at ' + string(sal,form='(f5.2,"m")'), $
		string(1e3*siz,form='(i0,"!7l!x slit")')]
	if whi then $
	gtit[1] = gtit[1] +' , Mirror focusing to '+string(fol,form='(f5.2,"m")')
	
	window, (dwin + 1), xsi = 8*bsiz, ysi = 13*bsiz

	!p.region = [0,phi/2,1,phi]
	plot, wloc, rsiz, tit = 'Spatial size at ' + string(sal,form='(f5.2,"m")'), $
	xtit = 'Slit location (m)', ytit = 'beam size (!7l!x) ' + styp[ffl], $
	xstyle=1, _extra = _e
	
	!p.region = [0,0,1,phi/2]
	plot, wloc, asiz, tit = 'Angular size at ' + string(sal,form='(f5.2,"m")'),$
	xtit = 'Slit location (m)', ytit = 'Ang. beam size (!7l!xr) '+ styp[ffl], $
	xstyle=1, /noerase, _extra = _e

	Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.3

	Plvar_keep, act = 'res'

	return
end