Pro Pstop, x, fang, sang, length= len, radius= rad, calc= cang, window= win, $
	_extra= _e

	on_error, 1

	cx = 2.5
	bang = 1
	thi = 25

	len = Default(len,[85,45,25])
	rad = Default(rad,[-1,250,40])
	y = (z = (a = [0]))
	wwin = Default(win,0,/dtyp)

	if n_elements(len) eq n_elements(rad) then begin
		for i = 0l, n_elements(len) - 1 do begin
			n = n_elements(y)
			y1 = y[n-1]
			z1 = z[n-1]
			a1 = a[n-1]
			ang = !dtor*a1
			if rad[i] gt 0 then begin
				zrat = Make_grid([0,len[i]],1,/step)/rad[i]
				yrat = (cos(ang) - sqrt(cos(ang)^2 - 2*sin(ang)*zrat - zrat^2))
				y = [y,y1+rad[i]*yrat]
				z = [z,z1+rad[i]*zrat]
				a = [a,!radeg*asin(sin(ang) + zrat)]
			endif else begin
				y = [y,y1+len[i]*tan(ang)]
				z = [z,z1+len[i]]
				a = [a,a1]
			endelse
		endfor
	endif else message, 'Inputs discrepancy!'
	abang = !dtor*bang
	tem = y*cos(abang) + z*sin(abang)
	z = z*cos(abang) - y*sin(abang)
	y = tem
	a = a + bang

	sfl = Isnum(sang)

	dum = where(x ge cx)
	yy = x[dum] - cx
	wfang = fang[dum]
	if sfl then wsang = sang[dum]

	window, wwin
	plot, yy, wfang, /nodata, _extra = _e
	oplot, yy, wfang, col = !pcol.lred
	if sfl then oplot, yy, wsang, col = !pcol.dred
	oplot, y, a, col = !pcol.green

	dum = where(x le cx)
	yy = reverse(cx - x[dum])
	wfang = reverse(fang[dum])
	if sfl then wsang = reverse(sang[dum])

	window, wwin + 1
	plot, yy, wfang, /nodata, _extra = _e
	oplot, yy, wfang, col = !pcol.lred
	if sfl then oplot, yy, wsang, col = !pcol.dred
	oplot, y, a, col = !pcol.green

	tlen = total(len)
	print
	print, tlen*cos(abang), tlen*sin(abang) + thi*cos(abang)
	print, max(z), max(y)
	print

	sx = [cx - reverse(y),cx + y]
	sa = [reverse(a),a]
	cang = Splin_eval(x,Splin_coeffs(sx,sa,/seg))

	return
end