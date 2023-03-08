Pro TBM_17_mshow, e, thick = thi, range = ran, _extra = _e

	on_error, 1

	csig = 0.99
	psi = 86.

	r_e = 1.687e4
	r_gc = 9.892e6

	e_c = 19.52
	oogam = 73
	if n_elements(e) ne 1 then message, 'Only scalar energy accepted!'
	sig = 0.57*(e_c/e)^0.43*oogam
	dum = Bragg_angle(ene=e,crys='si',ind=[1,1,1],dar=dar,/rad)
	bran = 1e6/2*dar

	n = n_elements(thi)
	cols = [!pcol.blue,!pcol.green,!pcol.red]
	text = strarr(n+1)
	lcols=lonarr(n+1)
	text[0] = ' Ideal'
	lcols[0] = !pcol.black
	for i = 0, n-1 do begin
		d = 1e-3*Default(thi[i])
		tetg = 1e6*sqrt(r_gc*d^2/r_e^3)
		maxan = (psi/2)^3/tetg^2
		wran = Default(ran,maxan)
		dist = Grav_sag(sig,tetg,ran=wran,prec='lo',nosag=ndist,con=csig,/full)
		norm = max(ndist[1,*])
		if i eq 0 then plot, ndist[0,*], ndist[1,*]/norm, thi=2 ,$
		xtit = 'Angle (!7l!xrad)',ytit = 'Relative intensity', _extra = _e
		oplot, dist[0,*], dist[1,*]/norm, thi = 2, color = cols[i mod 3]
		text[i+1] = strcompress(string(thi[i]) + ' mm') + '  '
		lcols[i+1] = cols[i mod 3]
	endfor

	Legend_mm, text = text, line=lonarr(n+1), col= lcols, _extra = _e
	return
end