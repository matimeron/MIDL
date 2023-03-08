Pro DCM_movie, arange= arn, step= stp, voff= vof, loff= lof, aperture= ape,$
	wait= wai, save= sav, mfile= mfil, rate= rat, _extra= _e

	common dcmov_file, lpath

	on_error, 1
	brep = 30

	ang = make_grid(reverse(arn),stp,/step,dim=nang)
	nang = nang[0]

	wai = Default(wai,-1e-6,/dtyp)
	if wai gt 0 then print, string([13b,9b,9b]) + $
		'Hit "Q" to exit, "P" for previous, any other key to continue'

	if keyword_set(sav) and wai le 0 then begin
		if !d.window lt 0 then window, 0
		mfil = File_get( $
			mfil,path=lpath,stat=stat,/write,/over,def='mp4',/auto,_extra=_e)
		dum = Fnamparse(mfil,path=lpath)
		vob = IDLffVideoWrite(mfil)
		vids = vob.AddVideoStream(!d.x_size,!d.y_size,30)
		wrep = round(brep/Default(rat,1)) > 1
		sfl = 1
	endif else sfl = 0

	for i = 0, nang-1 do begin
		DCM_pic, ang[i], voff= vof, loff= lof, aperture= ape, _extra= _e
		if sfl then for j = 0, wrep-1 do dum = vob.Put(vids,TVRD(true=1))

		if wai gt 0 then begin
			dum = (dum = get_kbrd())
			if Streq(dum,'p',1) then i = (i-2) > (-1) $
			else if Streq(dum,'q',1) then break
		endif else if wai lt 0 then wait, abs(wai)
	endfor
	if sfl then vob.Cleanup

	return
end