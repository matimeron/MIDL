Pro Mirror_sim, fir, sec, theta= tet, energy= ene, mir_loc= mlc, det_loc= dlc,$
	rsig = rsg, asig = asg, pmax = pmx, mask= msk, slit= slt, prof_only= prf, $
	_extra = _e

	on_error, 1
	owin = -1
	udl = 2.4

	mlc = Default(mlc,[32.2,32.9])
	rsg = Default(rsg,!blpar.rsig[1])
	asg = Default(asg,!blpar.asig[1])

	lam = float(1e-10*!srcon.conv/ene)
	ursg = 1e3*sqrt(2*lam*udl)/(4*!pi)
	uasg = 1e3*sqrt(lam/(2*udl))

	wrsg = sqrt(rsg^2 + ursg^2)
	wasg = sqrt(asg^2 + uasg^2)

	dis = dlc - mlc
	zav = total(mlc)/2
	wcsg = sqrt(wrsg^2  + zav^2*wasg^2)
	tsg = wrsg*wasg/wcsg

	stet = sin(1e-3*tet)
	top = min([max(fir[0,*]),max(sec[0,*])])
	bot = max([min(fir[0,*]),min(sec[0,*])])
	!null  = where(fir[0,*] ge bot and fir[0,*] le top, nf)
	!null  = where(sec[0,*] ge bot and sec[0,*] le top, ns)
	np = (2*round((top - bot)*stet/(dis[0]*tsg)) + 1) > (nf > ns)
	zm = Make_grid([bot,top],np)
	nif = 1e3*Splin_eval(zm,Splin_coeffs(fir[0,*],fir[1,*]))
	nis = 1e3*Splin_eval(zm,Splin_coeffs(sec[0,*],sec[1,*]))
	ym  = zm*stet

	tru = lonarr(np)
	whi = One_of(msk,slt,val=lim)
	if whi ge 0 then begin
		if n_elements(lim) eq 2 then begin
			lim = [min(lim,max=max),max]
			if whi then tloc  = where(zm ge lim[0] and zm le lim[1],ntloc) $
			else tloc  = where(zm le lim[0] or zm ge lim[1],ntloc)
			if ntloc gt 0 then begin
				tru[tloc] = 1
				tzm = zm[tloc]
				tnif = nif[tloc]
				tnis = nis[tloc]
			endif
			dum = (Wherinstruct('norm',_e))[0]
			if dum ge 0 then _e.(dum) = 0
		endif else message, 'Slit or mask need two elements', /con 
	endif
	tfl = max(tru)

	acen = zav*ym*(wasg/wcsg)^2
	pmx = Default(pmx,100l)
	p = round(pmx*exp(-(ym/wcsg)^2/2))
	dimg = (mdimg = [])
	seed = systime(1)
	for  i = 0l, np-1 do begin
		if p[i] gt 0 then begin
			del = tsg*randomn(seed,p[i]) + acen[i]
			add = ym[i] + (del - 2*nif[i])*dis[0] - 2*nis[i]*dis[1]
			dimg = [dimg,add]
			if tru[i] then mdimg = [mdimg,add]
		endif
	endfor

	plvar_keep, act='sav'
	tcol =  !pcol.green
	if not keyword_set(prf) then begin
		bsiz = 32l
		xsi = 16*bsiz
		ysi = 24*bsiz
		wnum = 20l
		owin = !d.window
		window, wnum, xsize = xsi, ysize = ysi
		!p.multi = [0,0,2]
		mnif = 1e3*nif
		mnis = 1e3*nis
		plot, zm, mnif, tit='Mirror slope', xtit='mm', ytit='!7l!xrad', /nodata
		if tfl then begin
			if whi then begin
				oplot, zm, mnif, thi=2
				oplot, zm, mnif + dis[1]/dis[0]*mnis
				dum = where(tru)
				oplot, zm[dum], mnif[dum], thi=2, col=tcol
				oplot, zm[dum], (mnif + dis[1]/dis[0]*mnis)[dum], col=tcol
			endif else begin
				oplot, zm, mnif, thi=2, col=tcol
				oplot, zm, mnif + dis[1]/dis[0]*mnis, col=tcol
				dum = where(tru eq 0, ndum)
				if ndum gt 0 then begin
					oplot, zm[dum], mnif[dum], thi=2
					oplot, zm[dum], (mnif + dis[1]/dis[0]*mnis)[dum]
				endif
			endelse
		endif else begin
			oplot, zm, mnif, thi=2, col=tcol
			oplot, zm, mnif + dis[1]/dis[0]*mnis, col=tcol
		endelse
	endif

	dimg = 1e3*dimg[sort(dimg)]
	dens = ADC(dimg,n_chan=np/4,val=vloc,_extra=_e)	
	plot, vloc, dens, thi = 2, xran = max(abs(vloc))*[-1,1], $
	xtit = '!7l!xm', ytit = 'a.u.', tit = 'Vertical profile', _extra = _e
	if tfl  then begin
		mdimg = 1e3*mdimg[sort(mdimg)]
		mdens = ADC(mdimg,n_chan=np/4,val=mvloc,_extra=_e)
		oplot, mvloc, mdens, thi = 2, col = tcol, _extra = _e
	endif
	Plvar_keep, act = 'res'

	if owin ge 0 then wset, owin
	return
end