Function GIXdat, lim = lim, trim = trm, show = sho, title = tit

	on_error, 1

	wlim = Default(lim,0.2)
	dat = Rascii(file=fil)
	tit = Fnamparse(fil)
	dum = where(dat[0,*] ge 0 and dat[0,*] le wlim and dat[1,*] gt 0)
	dat = dat[*,dum]
	if keyword_set(trm) then dat = Scan_trim(dat,xstep=trm,var=0.1,/spec)
	if keyword_set(sho) then Scan_show, dat, /ylog, tit = tit, xtit = 'Q!dz!n'

	return, dat
end