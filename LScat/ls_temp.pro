Pro LS_temp, wait = wai

	common ls_data, lsdat
	on_error, 1

	fun = 'mlorentz_fun'
	wwai = Default(wei,1)
	dum = Fnamparse(lsdat.file,pat=pat)
	spat = pat + lsdat.stuff + ' fits' + sdep(/ds)
	check = file_search(spat,/test_dir,/mark_dir) eq spat
	if not check then file_mkdir, spat

	for i = 1, lsdat.nscan do begin
		cur = lsdat.scan[i]
		dat = Rascii(pat + cur.sfile + '_bgs.txt')
		fit = dat
		par = [0,0,0,cur.amp,cur.cent,cur.hwid]
		fit[1,*] = call_function(fun,par,fit[0,*])
		Scan_show, fit
		Wascii, fit, spat + cur.sfile + '_fit.txt', /auto, call=2
	endfor

	return
end