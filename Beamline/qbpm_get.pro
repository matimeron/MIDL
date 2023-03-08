Function QBPM_get, dt, vertical = ver, horizontal = hor, raw = raw

	on_error, 1

	dt = Default(dt,0.01)
	pvnam = '15IDA:SIS3820:mca' + string([5,6,7,8],form='(i0)') + '.VAL'
	pvnam = strcompress(pvnam,/rem)

	lim = lonarr(4)
	raw = []
	for i = 0, 3 do begin
		dum = caget(pvnam[i],chan)
		raw = [[raw],[chan]]
		lim[i] = (where(chan eq 0))[0]
	endfor
	raw = Cast(transpose(raw),4)
	len = (size(raw,/dim))[1]
	dum = where(lim eq -1, ndum)
	if ndum gt 0 then lim[dum] = len
	len = len < min(lim)
	wraw = raw[*,len]
	res = fltarr(2,len)
	res[0,*] = dt*findge

	whi = One_of(ver,hor) > 0
	res[1,*] = (wraw[whi+2,*] - wraw[whi,*])/(wraw[whi+2,*] + wraw[whi,*])

	return, res
end