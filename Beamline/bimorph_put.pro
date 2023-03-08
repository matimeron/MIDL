Pro Bimorph_put, volt

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	Bimorph_init

	if n_elements(volt) ne nseg then message, $
	string(nseg,form="('Input needs ',i0,' values!')")
	evnam = 'Bimorph_VFM:SET-VTRGT'+ string(indgen(16),form='(i02)')
	fvolt = float(volt)
	for i = 0, nseg-1 do dum = caput(evnam[i],fvolt[i])

	return
end