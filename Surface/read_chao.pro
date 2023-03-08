Function Read_Chao, filnam, _extra = _e

	on_error, 1

	rdat = Rascii(filnam,npo=npo,_extra=_e)
	if npo[0] lt 3 then message, 'Not a valid 2D data!'

	sec = (where(rdat[0,*] ne rdat[0,0]))[0]
	fir = npo[1]/sec

	res = fltarr(npo[0],fir,sec)
	for i = 0, npo[0]-1 do res[i,*,*] = transpose(reform(rdat[i,*],sec,fir))

	return, res
end