Pro ID_14_save, dat, coeff, show_wc = sho, no_save = nos

	on_error, 1

	name = ['x','y','power','dx','dy','povar','povci']
	if not Arreq((size(dat))[0:1],[3,5]) then message, 'Invalid data!'
	if Type(coeff) eq 7 then cname = coeff + name else cname = name

	x = reform(dat[0,*,*])
	y = reform(dat[1,*,*])
	power = reform(dat[2,*,*])
	dx = reform(dat[3,*,*])
	dy = reform(dat[4,*,*])
	povar = power/(dx*dy)
	povci = power/(2*(dx+dy))

	if keyword_set(sho) then begin
		print
		print, format = '(28x,"X",10x,"Y",8x,"VAL",9x,"DX",9x,"DY")'
		print
		max = max(power,ind)
		print, x[ind], y[ind], power[ind], dx[ind], dy[ind], $
		form = '("Worst case POWER : ",5(f10.3,:," "))'
		max = max(povar,ind)
		print, x[ind], y[ind], power[ind], dx[ind], dy[ind], $
		form = '("Worst case POVAR : ",5(f10.3,:," "))'
		max = max(povci,ind)
		print, x[ind], y[ind], power[ind], dx[ind], dy[ind], $
		form = '("Worst case POVCI : ",5(f10.3,:," "))'
		print
	endif

	if not keyword_set(nos) then begin
		for i = 0, n_elements(name) - 1 do $
		dum = execute(cname[i] + ' = ' + name[i])
		file = File_get(stat=stat,/wri,def='sav')
		if stat then dum = $
		execute("save, file='" + file + "',"+ strjoin(cname,',')) $
		else print, 'Not executed!'
	endif

	return
end