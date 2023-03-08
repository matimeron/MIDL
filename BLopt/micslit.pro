Pro Micslit, ene, npoints = npo, extend = ext, win = win, _extra=_e

	common mic_stuff, ytit, gtit
	on_error, 1

	wnpo = Default(npo,100,/dtyp)
	wext = Default(ext,0) > 0
	wwin = Default(win,1) > 0
	dum = Micbeam(ene,foc_len=fol,_extra=_e)
	ifol = 1./fol
	fran = [min(ifol,max=max),max]
	dran = fran[1] - fran[0]
	wran = fran + dran*(1+2*wext)/4.*[-1,1]
	wran = [Fltround(wran[0],dig=3,/floor),Fltround(wran[1],dig=3,/ceil)]

	dum = (Wherinstruct('det',_e))[0]
	if dum ge 0 then _e.(dum) = 0

	iflen = [Make_grid(wran,wnpo),ifol]
	iflen = iflen[Sorpurge(iflen,net=n)]
	idflen = iflen - ifol[0]

	size = (flux = fltarr(n))
	for i = 0, n-1 do begin
		sres = Micbeam(ene,flux=fres,idel=idflen[i],_extra=_e)
		size[i] = sres
		flux[i] = fres
	endfor

	bsiz = 66
	phi = 0.9
	xlab = [0.5,0.5,0.5]
	ylab = [0.98,0.95,0.92]
	xtit = 'Inverse Focal length (m!e-1!n)'
	sloc = $
	[where(iflen eq ifol[0]),where(iflen eq ifol[1]),where(iflen eq ifol[2])]
	scol = [!pcol.lblue,!pcol.green,!pcol.red]

	Plvar_keep, act = 'sav'
	Psyms, 5

	window, wwin, xsi = 8*bsiz, ysi = 13*bsiz

	!p.region = [0,phi/2,1,phi]
	plot,  iflen, 1e3*size, xtit= xtit, ytit = ytit, /ynoz, $
	tit = 'Beam size at sample', _extra = _e
	for i = 0, 2 do plots, iflen[sloc[i]], 1e3*size[sloc[i]], col=scol[i], $
	psym = 8
	Legend_mm, loc='ul', text=['Minimal size','No-slit focus','Maximal flux'], $
	sym=8, col=scol, /nowrap, charsize=1.2

	!p.region = [0,0,1,phi/2]
	plot,  iflen, flux, xtit=xtit, /ylog, /noerase, $
	tit = 'Relative flux at sample', _extra = _e
	for i = 0, 2 do plots, iflen[sloc[i]], flux[sloc[i]], col=scol[i], $
	psym = 8
	Legend_mm, loc='ul', text=['Minimal size','No-slit focus','Maximal flux'], $
	sym=8, col=scol, /nowrap, charsize=1.2

	Labels, xlab, ylab, gtit, align = 0.5, /normal, charsize=1.5, charthi=1.5

	Psyms
	Plvar_keep, act = 'res'

	return
end