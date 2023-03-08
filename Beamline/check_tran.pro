Pro Check_tran, efirst = efi, emax = ema, thi = thi, base = bas, _extra = _e

	on_error, 1

	wefi = Default(efi,88./21,/dtyp)
	wema = Default(ema,500.,/dtyp)
	wthi = Default(thi,1.27,/dtyp)
	wbas = Default(bas,3,/dtyp)
	if wbas mod 2 eq 0 then message, 'base harmonic must be odd!'

	hmax = floor(wema/wefi)
	mhmax = floor(hmax/wbas)
	mhar = 2*lindgen((mhmax+1)/2) + 1
	har = wbas*mhar
	ene = wefi*har
	tra = FPU_fix(exp(-wthi*Abs_coeff(ene,elem='pb')))
	n = n_elements(har)
	bw = (flux = fltarr(n))
	for i = 0, n-1 do begin
		dum = Bragg_angle(ene=wema,crys='si',ind=mhar[i]*[1,1,1],eta=eta)
		flux[i] = UND_flux(ene[i],per=27,har=har[i],ban=eta,/def,_extra = _e)
		bw[i] = eta
	endfor
	counts = total(flux*tra)
	abene = total(flux*tra*ene/!srcon.scal)
	print, counts
	print, abene

	return
end