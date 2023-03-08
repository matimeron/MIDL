Pro Stick_gid_glob, phi_range = phr, step = stp, $
	wait = wai, save = sav, file = fnam, rate = rat, _extra = _e

	on_error, 1
	brep = 30

	case n_elements(phr) of
		0	:	message, 'Missing PHI_RANGE input!'
		1	:	wphr = [0,phr]
		else:	wphr = phr[0:1]
	endcase
	if n_elements(stp) eq 0 then message, 'Missing step size!'
	phi = Make_grid(float(wphr),stp,/step,dim=n)

	sfl = keyword_set(sav)
	if sfl then window, 5, xsi=640, ysi=512
	for i = 0l, n[0]-1 do begin
		Stick_gid, phi = phi[i], _extra = _e
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