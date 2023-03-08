Pro LS_wtemp, wait = wai

	common ls_data, lsdat
	on_error, 1
	ptit = ['',' 1st',' 2nd', ' 3rd', ' 4th', ' 5th', ' 6th'] + ' order'  

	wwai = Default(wei,1)
	dum = Fnamparse(lsdat.file,pat=pat)
	spat = pat + lsdat.stuff + ' partial' + sdep(/ds)
	check = file_search(spat,/test_dir,/mark_dir) eq spat
	if not check then file_mkdir, spat

	wdat = lsdat.scan[1:lsdat.nscan]
	ords = wdat[Sorpurge(wdat.ord,net=nord)].ord
	
	for i = 0, nord-1 do begin
		dum = where(wdat.ord eq ords[i])
		nams = wdat[dum].sfile
		pres = wdat[dum].pres
		cens = wdat[dum].cent
		cerr = wdat[dum].cent_err
		wids = wdat[dum].hwid
		werr = wdat[dum].hwid_err
		Tabulate, nams, pres, cens, cerr, wids, werr, /auto, call=2,	$
		header = ['File','Pressure','Center','Cen_err','Width','Wid_err'], $
		form=['a24','5f7.3'], file= spat+ lsdat.stuff+ ptit[ords[i]]+ '.txt'
	endfor

	return
end