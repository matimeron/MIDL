Pro CXwrite, dat, file = fname, title = tit, verbose = vrb, _extra = _e

	on_error, 1
	tlen = 80

	siz = size(dat)
	if Arreq(siz[0:1],[2,3]) then begin
		out = strarr(siz[2]+1)
		out[0] = string(replicate(32b,tlen))
		if n_elements(tit) eq 1 then out[0] = strmid(tit + out[0],0,tlen)
		for i = 1, siz[2] do out[i] = string(dat[*,i-1],form='(3(4x,g15.5))')
	endif else message, 'Data must be in the form of a [3,*] array!'

	rname = File_get(fname,stat=stat,/write,/over,def='dat',_extra=_e)
	if stat then begin
		openw, unit, rname, /get_lun
		printf, unit, out
		free_lun, unit

		if keyword_set(vrb) then begin
			print
			print, '	Saved ' + rname
			print
		endif
	endif else message, 'Missing file name, not saved!', /cont

	return
end