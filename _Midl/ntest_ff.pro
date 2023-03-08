Pro NTest_FF, a, b, c, qval = qvl, radians = rad, precision = prc, $
	tet = ctet, int = fint, _extra = _e

	on_error, 1

	if keyword_set(rad) then amult = 1d else amult = !dpi/180

	wa = Default(a,1d,/dtyp)
	wb = Default(b,wa,/dtyp)
	wc = Default(c,wa>wb,/dtyp)
	wq = Default(qvl,0d,/dtyp)

	tem = systime(1)
	wprc = 1 > Default(prc,1,/dtyp)
	if n_elements(wprc) eq 1 then wprc = [wprc,wprc]
	nphi = 2l^wprc[0] + 1
	wphi = make_grid([0d,!dpi/2],nphi)
	ntet = 2l^wprc[1] + 1
	ctet = make_grid([0d,1d],ntet)
	stet = sqrt(1-ctet^2)
	wint = 0d*ctet

	print
	print, nphi, ntet

	for i = 0, ntet-1 do begin
		prod = (Sp_beselj(wq*wa*stet[i]*cos(wphi)/2,0)*$
				Sp_beselj(wq*wb*stet[i]*sin(wphi)/2,0))^2
		wint[i] = Integ(wphi,prod,/val)
	endfor
	wint = 2/!dpi*Sp_beselj(wq*wc*ctet/2,0)^2*wint
	plot, ctet, wint, _extra=_e
	fint = Integ(ctet,wint,/val)
	print, fint
	print
	print, systime(1) - tem

	return
end