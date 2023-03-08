Pro LS_batch, file = fil, _extra = _e

	common ls_data, lsdat
	on_error, 1

	if n_elements(fil) ne 0 then LS_load, file

	wdat = lsdat.scan[1:lsdat.nscan]
	sor = Lexisort(wdat.ord,wdat.pres)
	wdat = wdat[sor]
	tens = fltarr(lsdat.nscan)
	uord = wdat[Sorpurge(wdat.ord,net=net)].ord
	for i = 0, net-1 do begin
		loc = where(wdat.ord eq uord[i])
		tens[loc] = LS_gamma(wdat[loc].cent,order=uord[i],_extra=_e)
	endfor
	tens = 1e3*tens

	Tabulate, wdat.ord, wdat.cent, wdat.pres, tens, wdat.pres + tens, head = $
	['Order','Freq. (kHz)','Pressure (mN/m)','Tension (mN/m)','Sum (mN/m)'], $
	tit = lsdat.stuff, form = ['i2','f6.2','f6.2','f6.2','f6.2']

	return
end