Pro Spread_wid, muds, progress = prg, _extra = _e

	on_error, 1

	x = Make_grid([0,2],0.1,/step,dim=nx)
	scomp = replicate(1,nx)
	n = n_elements(muds)
	wids = 0*muds
	stas = fix(wids)
	pfl = keyword_set(prg)
	for i = 0l, n-1 do begin
		f = Spread(x,mud=muds[i],/ref,stat=sta)
		if Arreq(sta,scomp) then begin
			stas[i] = 1
			wids[i] = Splinroot(Splin_coeffs(x,f-0.5))
		endif
		if pfl then print, i, muds[i], wids[i]
	endfor

	dum = where(stas,ndum)
	if ndum gt 0 then plot, 1 - exp(-muds[dum]), wids[dum], _extra = _e

	return
end