Pro Undulator_movie, cur, dist, unfile = file, sli_range = sran, step = stp, $
	harm_units = haru, bandwidth = ban, horizontal = hor, vertical = ver, $
	wait = wai, save = sav, file = fnam, rate = rat, flux = flu, _extra = _e

	on_error, 1
	brep = 30

	file=file_get(file,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
	if stat then begin
		u = UN_struct()
		openr, datun, file, /get_lun, /block
		readu, datun, u
		free_lun, datun
	endif else message, 'Cannot find file!'

	if n_elements(sran) eq 2 then sli= Make_grid(sran,stp,/step,dim=n) $
	else message, 'Range needs 2 elements!'
	flu = 0*sli

	ityp = One_of(hor,ver,/nozer)
	sfl = keyword_set(sav)
	for i = 0l, n[0]-1 do begin
		Und_slice, cur, dist, file=file, sli=sli[i], har = haru, ban = ban, $
		out = out, tflux = tfl, show = (ityp lt 0), _extra = _e
		flu[i] = tfl
		if ityp ge 0 then begin
			if i eq 0 then begin
				siz = size(out,/dim)
				buf = make_array(siz+[1,0,0],typ=4)
				dxy = [out[0,1,0]-out[0,0,0],out[1,0,1]-out[1,0,0]]
				tit = ['Horizontal','Vertical'] + ' profile; '
			endif
			buf[0:2,*,*] = out
			ifl = [ityp,1-ityp]
			idat = Img_int(buf,xy_int=ifl[0],z_int=ifl[1])
			idat = Scan_scale(idat,dxy[1-ityp])
			window, 0
			Scan_show, idat,thi=2,xsty=1,ysty=1, $
			tit = tit[ityp] + string(sli[i],form='("E = ",f5.2, "keV")'), $
			xtit = 'mm', ytit = 'ph/mm'
		endif
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