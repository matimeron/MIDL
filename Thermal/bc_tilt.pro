Function BC_tilt, temp, dims, pow, kappa = kap, hval = hvl, thick = thi, $
	xtilt = xti, ytilt = yti, radians= rad, eps = eps, val = val, _extra = _e

	on_error, 1


	whi = One_of(xti,yti)
	if whi lt 0 then message, 'X-tilt or Y-tilt, please specify?'
	ang = !pi/2*[1,1]
	if keyword_set(rad) then mult = 1. else mult = !dtor
	eps = Default(eps,1e-4,/dtyp)*mult
	done = (refl = 0)
	repeat begin
		if refl then begin
			uang = ang
			ang[whi] = ang[whi]/2
			lang = ang
		endif
		val = BC_etemp(dims, pow, kappa= kap, hval= hvl, thick= thi, $
		xang = ang[0], yang = ang[1], /rad, _extra= _e)
		if val le temp then done = 1 else refl = 1
	endrep until done

	while refl do begin
		ang = (lang + uang)/2
		val = BC_etemp(dims, pow, kappa= kap, hval= hvl, thick= thi, $
		xang = ang[0], yang = ang[1], /rad, _extra= _e)
		if uang[whi] - lang[whi] gt eps then begin
			if val le temp then lang = ang else uang = ang
		endif else refl = 0
	endwhile

	return, ang[whi]/mult
end