Pro LD_multi, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	angles = ang, horizontal = hor, offset = off, channels = chan, color = col, $
	_extra = _e

	on_error, 1
	snam = strcompress('s_' + sindgen(8),/remove)
	dnam = strcompress('d_' + sindgen(8),/remove)

	nsc = Scan_list_ver(s_0,s_1,s_2,s_3,s_4,s_5,s_6,s_7,flag=lfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!'

	doff = 0
	for i = 0, nsc - 1 do begin
		dum = $
		execute(dnam[i]+'= LD_read('+ snam[i]+',ang=ang,hor=hor,_extra=_e)')
		dum = execute('doff = doff > max(('+dnam[i]+')[1,*])')
		if i eq 0 then begin
			if keyword_set(chan) then xco = findgen(512) else xco = reform(d_0[0,*])
		endif
	endfor
	woff = Default(off,doff)

	for i = 0, nsc - 1 do begin
		dum = execute('('+dnam[i]+')[0,*] = xco')
		dum = execute('('+dnam[i]+')[1,*] = ('+dnam[i]+')[1,*] + i*woff')
	endfor

	shcom = 'Scan_show,'
	colis = 'lcol = ['
	for i = 0, nsc - 1 do begin
		shcom = shcom + dnam[i] + ','
		if keyword_set(col) then colis = colis + string(!pcol.(i)) +',' $
		else colis = colis + '0,'
	endfor
	colis = strmid(colis,0,strlen(colis)-1) + '],'
	shcom = shcom + colis + '_extra = _e'
	dum = execute(shcom)

	return
end
