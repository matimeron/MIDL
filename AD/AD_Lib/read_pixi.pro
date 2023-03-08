Function Read_PIXI, filename

	on_error, 1
	hlen = 16l

	wfnam = File_get(filename,stat=stat,_extra=_e)
	if stat then begin
		openr, pixun, wfnam, /get_lun, /swap_if_big
		head = ulonarr(hlen/4)
		readu, pixun, head
		dim = head[0:1]
		img = lonarr(dim)
		readu, pixun, img
		free_lun, pixun
	endif else message, 'Bad or missing file!'

	return, img
end