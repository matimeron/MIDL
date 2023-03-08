Pro OUndulator_movie, cur, dist, unfile = file, sli_range = sran, step = stp, $
	wait = wai, save = sav, file = fnam, rate = rat, _extra = _e

	on_error, 1
	brep = 30

	if n_elements(sran) eq 2 then sli= Make_grid(sran,stp,/step,dim=n) $
	else message, 'Range needs 2 elements!'

	sfl = keyword_set(sav)
	for i = 0l, n[0]-1 do begin
		Und_slice, cur, dist, file=file, sli=sli[i], /har, $
		mark=[-1.5,-1,1.5,1], rad=1, _extra = _e
		if sfl then begin
			if i eq 0 then begin
				frep = round(brep/Default(rat,1)) > 1
				handle = mpeg_open([!d.x_size,!d.y_size],_extra=_e)
			endif
			mpeg_put, handle, window= !d.window, frame= i*frep+1, /ord
		endif else wait, Default(wai,0.1)
	endfor
	if sfl then begin
		mpeg_put, handle, window= !d.window, frame= 0, /ord
		mpfile = File_get(fnam,stat=stat,/write,/over,def='mpg',_extra=_e)
		if stat then mpeg_save, handle, file = mpfile
		mpeg_close, handle
	endif

	return
end
	