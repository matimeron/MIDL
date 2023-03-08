Pro GID_build_movie, dat, wait= wai, save= sav, file= fnam, rate= rat, _extra=_e

	on_error, 1
	brep = 30

	wdat = reform(dat[2,*,*])
	max = max(wdat)
	dim = size(wdat,/dim)
	wdat = 0*wdat + Toler()

	sfl = keyword_set(sav)
	wrat = Default(rat,1)
	iswin = (Wherinstruct('wne',_e))[0]
	if iswin ge 0 then winfl = 1 else winfl = 0

	for i = 0l, dim[0] do begin
		PD_sum, wdat, bin=[1,2], auz=256, amax=max, xtit='frame #', ytit='pix',$
		_extra =_e
		if sfl then begin
			if i eq 0 then begin
				frep = round(brep/wrat) > 1
				handle = mpeg_open([!d.x_size,!d.y_size],_extra=_e)
			endif
			mpeg_put, handle, window= !d.window, frame= i*frep+1, /ord
		endif else wait, Default(wai,1.)/wrat
		if i eq 0 and winfl then _e.(iswin) = 0
		if i lt dim[0] then wdat[i,*] = dat[2,i,*]	
	endfor

	if sfl then begin
		mpeg_put, handle, window= !d.window, frame= dim[0]*frep+1, /ord
		mpfile = File_get(fnam,stat=stat,/write,/over,def='mpg',_extra=_e)
		if stat then mpeg_save, handle, file = mpfile
		mpeg_close, handle
	endif

	return
end