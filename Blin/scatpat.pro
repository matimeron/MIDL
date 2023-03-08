Pro Scatpat, kr, alpha = alp, distance= dst, size= dsz, degrees= deg, _extra= _e

	on_error, 1

	n = 512

	if keyword_set(deg) then walp = !dtor*alp else walp = alp
	xzgrid = Make_grid(0.5*dsz/dst*[[-1,1],[1,-1]],[n,n])
	x = reform(xzgrid[0,*,*])
	z = reform(xzgrid[1,*,*])
	sqr = sqrt(x^2 + z^2 + 1)

	q = 2*kr*sqrt((sin(walp)^2+ ((sqr-1)*cos(2*walp)+ z*sin(2*walp))/(2*sqr))>0)
	ff = Sp_form_factor(q)

	Display_mm, ff, reform(x[*,0]), reform(z[0,*]), _extra = _e

	return
end