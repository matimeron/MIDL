Pro UN_write, dat, slice = sli, _extra = _e

	on_error, 1

	siz = size(dat)
	if siz[0] lt 3 then message, 'Not a valid 2D data!'

	nct = siz[1]
	len = siz[2]*siz[3]
	if Isnum(sli) then begin
		if sli ge 0 and sli lt (nct-2) then begin
			nc = 3l
			col = [0l,1l,sli+2]
		endif else message, 'Slice value out of range!'
	endif else begin
		nc = nct
		col = lindgen(nc)
	endelse
	wdat = make_array(nc,len,type=Type(dat))
	for i = 0, nc-1 do wdat[i,*] = reform(transpose(dat[col[i],*,*]),len)
	form = strcompress('(' + string(nc) + 'g13.6)',/rem)
	sdat = string(wdat,form=form)

	spac = string(replicate(32b,13))
	tem = spac + ['X','Y',string(col[2:*]-2)]
	headf = strjoin(strmid(tem,12,13,/rev))
	sep = ' ' + string(replicate(95b,12))
	heads = strjoin(replicate(sep,nc))
	sdat = [headf,heads,sdat]

	fnam = File_get(default='txt',/write,/over,stat=stat,_extra=_e)
	if stat then begin
		openw, unit, fnam, /get_lun
		printf, unit, sdat, form = '(a)'
		free_lun, unit
	endif else message, 'File not written!', /cont

	return
end

