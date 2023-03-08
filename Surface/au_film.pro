Pro Au_film, din, dout, steps = stp, layers = lay, gap = gap, dfac = dfc, $
	qz = qzv, energy = ekev, roughness = roff, show = sho, _extra = _e

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam
	on_error, 1

	elm = ['Au', 'C', 'H', 'S', 'O', 'H']
	whi = Elecomp(elm)
	ela = abctab[whi].a
	elz = abctab[whi].z
	wei = ela*[1,12,26,1,1,2]
	nel = [1,3,2]
	neltot = round(total(nel))
	nnel = n_elements(nel)
	k = 0
	ind = lonarr(neltot)
	for i = 0l, nnel-1 do begin
		ind[k:k+nel[i]-1] = i
		wei[k:k+nel[i]-1] = wei[k:k+nel[i]-1]/total(wei[k:k+nel[i]-1])
		k = k + nel[i]
	endfor
	ro = [abctab[whi[0]].ro,0.8435,1]

	dnst = 5
	nst = Default(stp,dnst) > dnst
	nly = Default(lay,1,/dtyp) > 1
	wdf = Default(dfc,1.,/dtyp) < 1
	if n_elements(wdf) ne nly then begin
		if n_elements(wdf) eq 1 then wdf = replicate(wdf,nly) $
		else message, 'Density_factor - Layer_number mismatch'
	endif

	if din ge dout then message, 'Outer size must be larger than inner!'
	rin = 0.5*din
	rout = 0.5*dout
	nstout = round(nst*(rout - rin)/(2.*rout)) > 1
	dz = replicate((rout-rin)/nstout,nst)
	dz[nstout:nst-nstout-1] = 2*rin/(nst-2*nstout)
	if n_elements(gap) eq 1 and nly gt 1 then begin
		if gap gt 0 then begin
			dz = [dz,gap]
			nst = nst + 1
			nful = nst*nly
		endif else message, 'Gap must be positive!'
		gapfl = 1
	endif else begin
		nful = nst*nly + 1
		gapfl = 0
	endelse
	z = [0.,total(dz,/cum)] - rout

	hexvol = 2*sqrt(3)*rout^2*dz
	pvol = fltarr(3,nst)
	for j = 0l, nst-1 do begin
		pvol[0,j] = Sp_slice(z[j],z[j+1],rin)
		pvol[1,j] = Sp_slice(z[j],z[j+1],rout)
	endfor
	pvol[0,*] = pvol[0,*]/hexvol
	pvol[1,*] = pvol[1,*]/hexvol - pvol[0,*]

	vol = fltarr(3,nful)
	tarr = fltarr(nful)
	for i = 0l, nly-1 do begin
		vol[*,i*nst:i*nst+nst-1] = wdf[i]*pvol
		tarr[i*nst:i*nst+nst-1] = dz
	endfor

	if not gapfl then vol[2,nful-1] = 1.
	vol[2,*] = 1 - total(vol[0:1,*],1)
	tarr = tarr[0:nful-2]

	rarr = vol
	for j = 0l, nful-1 do rarr[*,j] = ro*rarr[*,j]
	rarr = total(rarr,1)

	lwei = fltarr(neltot,nful)
	for j = 0l, nful-1 do lwei[*,j] = vol[ind,j]*ro[ind]*wei/rarr[j]

	elar = strarr(neltot,nful)
	for j = 0l, nful-1 do elar[*,j] = elm
	elar = reform(elar,neltot*nful)
	numels = replicate(neltot,nful)
	garr = reform(lwei,neltot*nful)

	if n_elements(roff) ne 0 then begin
		wroff = fltarr(nful)
		wroff[0] = roff
	endif

	if keyword_set(sho) then begin
		z = [0,total(tarr,/cum)]
		sden = rarr
		for j = 0l, nful-1 do sden[j] = sden[j]*total(elz/ela*lwei[*,j])
		sden = sden/sden[nful-1]
		cwin = !d.window
		window, 1
		wset, 1
		Densplot, [z,max(z)],[0,sden]
		wset, cwin
	endif

	plot, qzv, Reflect(qz= qzv, energy= ekev, elem = elar, num_elem = numels, $
	dens = rarr, wei = garr, thi = tarr, rough = wroff), _extra = _e
	wshow

	return
end