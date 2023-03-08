Pro BM_17_mshow, e, thick = thi, noerase = noe, _extra = _e

	on_error, 1

	e_c = 19.52
	oogam = 73

	csig = 0.99
	psi = 86.

	r_e = 1.687e4
	r_gc = 9.892e6
	dd = 0.045
	d = 1e-3*Default(thi,1e3*dd,/dtyp)

	tetg = 1e6*sqrt(r_gc*d^2/r_e^3)
	maxan = (psi/2)^3/tetg^2

	if n_elements(e) ne 1 then message, 'Only scalar energy accepted!'
	sig = 0.57*(e_c/e)^0.43*oogam
	dum = Bragg_angle(ene=e,crys='si',ind=[1,1,1],dar=dar,/rad)
	bran = 1e6/2*dar

	dist = Grav_sag(sig,tetg,ran=maxan,con=csig,/full)

	if not keyword_set(noe) then plot, dist[0,*], dist[1,*], /nodata, _extra=_e
	oplot, dist[0,*], dist[1,*], col = !pcol.red
	dum = where(abs(dist[0,*]) le bran)
	oplot, dist[0,dum], dist[1,dum], col = !pcol.green

	return
end