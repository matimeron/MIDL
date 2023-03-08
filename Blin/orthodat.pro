Function Orthodat, file, swap = swap, refine = ref, offrad = ofr, show = sho, $
	_extra = _e

	on_error, 1

	res = Read_gdf(File_get(file),swap=swap)
	resx = reform(res[0,*])
	resy = reform(res[1,*])
	s = sort(resx)
	x = (wx = resx[s])
	y = (wy = resy[s])

	yrn = (max(y,min=min) - min)/4
	count = 10
	repeat begin
		count = count - 1
		np = n_elements(wy)
		yp = shift(wy,1)
		yp[0] = wy[0]
		yn = shift(wy,-1)
		yn[np-1] = wy[np-1]
		comp = wy - (yp + yn)/2
		ok = where(comp lt yrn and comp gt -yrn, iok, comp = bad, ncomp = ibad)
		if iok gt 0 then begin
			wx = wx[ok]
			wy = wy[ok]
		endif else count = 0
	endrep until ibad eq 0 or count eq 0

	refl = keyword_set(ref)
	nofr = refl or (n_elements(ofr) ne 2)

	ycen = max(wy,cind)
	xcen = wx[cind]
	xrn = max(wx,min=wmin)/50
	mid = where(wx ge xcen - xrn and wx le xcen + xrn)
	wxcen = Peak_cent(wx[mid],wy[mid])
	wx = wx - wxcen

	lind = where(wx lt 0, comp = rind)
	lwx = wx[lind]
	lwy = wy[lind]
	rwx = wx[rind]
	rwy = wy[rind]

	dum = where(lwx le (min(lwx,max=max) + max)/2)
	lpar = Linfit_mm(lwx[dum],lwy[dum])
	ltet = atan(lpar[1])
	if nofr then begin
		tem = -lwx*sin(ltet) + lwy*cos(ltet) - lpar[0]*cos(ltet)
		lwx = lwx*cos(ltet) + lwy*sin(ltet) - lpar[0]*sin(ltet)
		lwy = tem
		dum = where(lwy ge max(lwy)/2,ndum)
		dum = where(lwx ge 2*lwx[dum[0]])
		lcen = Circle_fit(lwx[dum],lwy[dum],rad=lrad,_extra=_e)
	endif else lcen = [-1,1]*ofr

	dum = where(rwx ge (min(rwx,max=max) + max)/2)
	rpar = Linfit_mm(rwx[dum],rwy[dum])
	rtet = atan(rpar[1])
	if nofr then begin
		tem = -rwx*sin(rtet) + rwy*cos(rtet) - rpar[0]*cos(rtet)
		rwx = rwx*cos(rtet) + rwy*sin(rtet) - rpar[0]*sin(rtet)
		rwy = tem
		dum = where(rwy ge max(rwy)/2,ndum)
		dum = where(rwx le 2*rwx[dum[ndum-1]])
		rcen = Circle_fit(rwx[dum],rwy[dum],rad=rrad,_extra=_e)
	endif else rcen = ofr

	if refl then begin
		icen =  ([-1,1]*lcen + rcen)/2
		dum = Kink_sumsq(icen,lwx,lwy,rwx,rwy)
		rcen = QND_ext('Kink_sumsq',x_ini=icen,step=sqrt(total(icen^2))/4,/warn)
		lcen = [-1,1]*rcen
	endif

	x = x - wxcen
	lind = where(x lt 0, comp = rind)
	lx = x[lind]
	ly = y[lind]
	rx = x[rind]
	ry = y[rind]
	gap = min(rx) - max(lx)

	tem = -lx*sin(ltet) + ly*cos(ltet) - lpar[0]*cos(ltet)
	lx = lx*cos(ltet) + ly*sin(ltet) - lpar[0]*sin(ltet)
	ly = tem
	lx = lx - lcen[0]
	dum = where(lx ge 0)
	cor = Unwind(lx[dum],ly[dum],rad=abs(lcen[1]))
	lx[dum] = cor[0,*]
	ly[dum] = cor[1,*]

	tem = -rx*sin(rtet) + ry*cos(rtet) - rpar[0]*cos(rtet)
	rx = rx*cos(rtet) + ry*sin(rtet) - rpar[0]*sin(rtet)
	ry = tem
	rx = rx - rcen[0]
	dum = where(rx le 0)
	cor = Unwind(rx[dum],ry[dum],rad=abs(rcen[1]))
	rx[dum] = cor[0,*]
	ry[dum] = cor[1,*]

	lx = lx - max(lx) - gap/2
	rx = rx - min(rx) + gap/2
	ss = sort(s)
	x = ([lx,rx])[ss] + wxcen
	y = ([ly,ry])[ss]

	res[0,*] = x
	res[1,*] = y

	if keyword_set(sho) then begin
		plot, resx, resy, /nodata, _extra = _e
		oplot, resx, resy, col = !pcol.red, psym = 3, _extra = _e
		oplot, x, y, col = !pcol.green, psym = 3, _extra = _e
	endif

	return, res
end