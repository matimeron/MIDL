Function Bimorph_get

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	Bimorph_init

	evnam = 'Bimorph_VFM:GET-VOUT'+ string(indgen(nseg),form='(i02)')
	res = fltarr(nseg)
	for i = 0, nseg-1 do begin
		dum = caget(evnam[i],val)
		res[i] = val
	endfor

	return, res
end