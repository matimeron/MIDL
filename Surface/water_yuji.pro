Pro Water_yuji, ref, energy = ene, rough = rof, _extra =_e

	on_error, 1

	wene = Default(ene,10.)
	wrof = Default(rof,2.7)

	rref = ref
	wat = Water(reform(ref[0,*]),ene=wene,rough=wrof)
	rref[1,*] = rref[1,*]/wat

	Scan_show, rref, xtit = 'Q!dz!n', ytit = 'R/R!df!n', _extra = _e
	info = strarr(2)
	info = ['Energy = ' + string(wene,form='(f8.3)'), 'Rough = ' + string(wrof,form='(f8.3)')]

	Legend_mm, text = strcompress(info)

	return
end