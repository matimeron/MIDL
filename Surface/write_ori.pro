Pro Write_ori, ldat

	on_error, 1

	siz = size(ldat)
	if Arreq(siz[0:1],[3,3]) then begin
		len = siz[2]*siz[3]
		wdat = [[reform(transpose(ldat[0,*,*]),len)],$
				[reform(transpose(ldat[1,*,*]),len)],$
				[reform(transpose(ldat[2,*,*]),len)]]
		Wascii, transpose(wdat)
	endif else message, 'Invalid input!'

	return
end