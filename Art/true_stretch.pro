Function True_stretch, cvec, len

	on_error, 1

	bas = 256l
	pow = bas^[0,1,2]
	col = (bas-1)*pow

	wvec = long(cvec)
	tabl = lonarr(3,len)
	for i = 0, 2 do begin
		tem = (cvec and col[i])/pow[i]
		tabl[i,*] = long(interpol(tem,len))
	endfor

	return, reform(tabl[0,*] + bas*(tabl[1,*] + bas*tabl[2,*]))
end
