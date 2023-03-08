Pro Iso_chfit, file, lag_range = lag, aresid = ars, bresid = brs, _extra = _e

	on_error, 1

	fil = File_get(file)

	adat = Iso_read(fil,/adat,_extra=_e)
	acof = Peak_fit(adat,/edge,/fix,bac=3,fit=afit)
	ars = transpose([[reform(adat[0,*])],[reform(adat[1,*])-afit]])
	bdat = Iso_read(fil,/bdat,_extra=_e)
	bcof = Peak_fit(bdat,/edge,/fix,bac=3,fit=bfit)
	brs = transpose([[reform(bdat[0,*])],[reform(bdat[1,*])-bfit]])

	window, 0
	Scan_show, ars, brs, lcol = [!pcol.dred,!pcol.blue]
	dlen = (size(ars))[2]
	sfac = (max(ars[0,*],min=min) - min)/(dlen-1)
	window, 1
	corr = CM_corr(ars[1,*],brs[1,*],lag=[Default(lag,(dlen+1)/2)])
	plot, sfac*findgen(n_elements(corr)), corr
	wset, 0

	return
end

	