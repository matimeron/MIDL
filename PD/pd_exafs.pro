Pro PD_exafs, snum, first = resf, second = ress, _extra = _e

	on_error, 1

	lis = Range_proc(snum)
	ns = n_elements(lis)
	lam = Scan_field_read(lis,'lam')
	ene = !srcon.conv/lam
	ord = sort(ene)
	ene = ene[ord]
	lis = lis[ord]

	resf = (ress = fltarr(3,ns))
	resf[0,*] = (ress[0,*] = ene)
	dbuf = (sbuf = fltarr(2,ns))

	for i = 0l, ns-1 do begin
		dbuf[*,i] = Scan_PD_max(lis[i],/tot,/norm,/glob)
		sbuf[*,i] = Scan_column(lis[i],'monc')
	endfor
	ebuf = sqrt(dbuf/sbuf)

	resf[1,*] = dbuf[0,*]
	resf[2,*] = ebuf[0,*]
	ress[1,*] = dbuf[1,*]
	ress[2,*] = ebuf[1,*]
;	resf = Scan_scale(resf,1/reform(sbuf[0,*]))
;	ress = Scan_scale(ress,1/reform(sbuf[1,*]))

	Scan_show, resf, ress, lcol= [!pcol.green,!pcol.blue], $
		xtit= 'Energy (keV)', _extra = _e

	return
end
	