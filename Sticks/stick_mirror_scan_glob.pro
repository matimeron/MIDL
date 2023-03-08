Pro Stick_mirror_scan_glob, theta=tet, y_range = yrn, step = stp, $
	wait = wai, save = sav, file = fnam, rate = rat, _extra = _e

	on_error, 1
	brep = 30

	case n_elements(yrn) of
		0	:	message, 'Missing Y_RANGE input!'
		1	:	wyrn = [0,yrn]
		else:	wyrn = yrn[0:1]
	endcase
	if n_elements(stp) eq 0 then message, 'Missing step size!'
	yy = Make_grid(float(wyrn),stp,/step,dim=n)

	sfl = keyword_set(sav)
	if sfl then window, 5, xsi=640*2, ysi=512*2
	for i = 0l, n[0]-1 do begin
		Stick_mirror_scan, theta=tet, y = yy[i], _extra = _e
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