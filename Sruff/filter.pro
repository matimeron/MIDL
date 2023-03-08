Pro Filter, ene = ene, thickness = thi, nslices = nsli, diamond = dia, $
	transmitted = tran, absorbed = abso, show = sho, zval = z, _extra = _e

	on_error, 1

	if keyword_set(dia) then den = 3.53 else den = 2.266

	slithi = 1.*thi/nsli
	z = slithi*findgen(nsli+1)
	tran = fltarr(nsli+1)
	tran[0] = 1
	for i = 1, nsli do tran[i] = $
		tran[i-1]*exp(-0.1*slithi*abs_coeff(ene,elem='c',den=den))
	abso = -Dif(tran)
	if keyword_set(sho) then abso[0] = Toler() else abso[0] = 0

	if keyword_set(sho) then begin
		window, 0
		plot, z, tran, tit = 'Transmitted fraction', xtit = 'Z (mm)', _extra=_e
		window, 1
		plot, z, abso, tit='Fraction absorbed in preceding slice',$
			xtit='Z (mm)', _extra=_e
	endif

	return
end