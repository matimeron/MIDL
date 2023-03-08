Pro LD_average, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	angles = ang, horizontal = hor, offset = off, result = res, _extra = _e

	on_error, 1
	snam = strcompress('s_' + sindgen(8),/remove)
	dnam = strcompress('d_' + sindgen(8),/remove)

	nsc = Scan_list_ver(s_0,s_1,s_2,s_3,s_4,s_5,s_6,s_7,flag=lfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!'

	for i = 0, nsc - 1 do dum = $
	execute(dnam[i]+'= LD_read('+ snam[i]+',ang=ang,hor=hor,_extra=_e)')

	res = d_0

	for i = 1, nsc - 1 do begin
		dum = execute('tem = ' + dnam[i])
		res[1,*] = res[1,*] + tem[1,*]
		res[2,*] = sqrt(res[2,*]^2 + tem[2,*]^2)
	endfor
	res[1:2,*] = res[1:2,*]/nsc

	Scan_show, res, _extra = _e

	return
end
