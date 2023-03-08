Pro Quickfit, x, y, ave_width = awi, down_factor = dof, refine = ref, show = sho

	on_error, 1

	awi = Default(awi,4.,/dtyp) > 1
	n = 2*awi
	hind = lindgen(awi) + 1
	dof = Default(dof,4.,/dtyp) > 1
	max = Max(y,loc)
	ind = loc + [-reverse(hind),hind]
	s1 = total(y[ind])
	s2 = total(y[ind]^2)
	hei = s1/n
	top = n*s2 - s1^2
	if top gt 0 then err = sqrt((n*s2 - s1^2)/(n*(n-1))) else err = sqrt(hei)

	dum = where(x lt x[loc] and y lt (hei - dof*err),ndum)
	if ndum gt 0 then fir = dum[-1] + 1 else fir = 0l
	dum = where(x gt x[loc] and y lt (hei - dof*err),ndum)
	if ndum gt 0 then las = dum[0] - 1 else las = n_elements(y)-1

	fx = x[fir:las]
	fy = y[fir:las]
	c = Linfit_mm(fx,fy,ord=2)

	if keyword_set(sho) then begin
		window, 0
		plot, x, y
		oplot, fx, fy, col = !pcol.red
		oplot, x, Poleval(x,c), thi=2, col= !pcol.green
	endif

	cen = -c[1]/(2*c[2])
	amp = -(c[1]^2 - 4*c[0]*c[2])/(4*c[2])
	sig = sqrt((c[1]^2 - 4*c[0]*c[2])/(8*c[2]^2))

	print
	print, '	Center = ', cen
	print, '	Sigma  = ', sig
	print  
	dum = peak_fit(fx,fy)
	print, '	Fcenter= ', dum[4]
	print, '	Fsigma = ', dum[5]

	if keyword_set(ref) then begin
		xen = sig*(4*err/amp)^(1./4)
		print
		print, xen
		dum = where(abs(x-cen) lt xen)
		fx = x[dum]
		fy = y[dum]
		c = Linfit_mm(fx,fy,ord=2)
		if keyword_set(sho) then begin
			window, 1
			plot, x, y
			oplot, fx, fy, col = !pcol.red
			oplot, x, Poleval(x,c), thi=2, col= !pcol.green
		endif
		cen = -c[1]/(2*c[2])
		amp = -(c[1]^2 - 4*c[0]*c[2])/(4*c[2])
		sig = sqrt((c[1]^2 - 4*c[0]*c[2])/(8*c[2]^2))

		print
		print, '	Center = ', cen
		print, '	Sigma  = ', sig
		print
		dum = peak_fit(fx,fy)
		print, '	Fcenter= ', dum[4]
		print, '	Fsigma = ', dum[5]
		
	endif

	return
end

	