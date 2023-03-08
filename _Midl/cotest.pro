Pro Cotest, r = r, g = g, b =b

	on_error, 1

	wr = Default(r,[0.,0.],/dtyp)
	wg = Default(g,[0.,0.],/dtyp)
	wb = Default(b,[0.,0.],/dtyp)
	q = [n_elements(wr),n_elements(wg),n_elements(wb)]
	if not Arreq(q,[2,2,2]) then message, 'Eh!'

	n = 256l
	l = n - 1
	k = lindgen(n)
	fac = alog(k+1)/alog(n)

	red = fac*round(wr[0]*l + (wr[1] - wr[0])*k)
	green = fac*round(wg[0]*l + (wg[1] - wg[0])*k)
	blue = fac*round(wb[0]*l + (wb[1] - wb[0])*k)

	tvlct, red, green, blue
	plvar_keep, act = 'sav'
	device, deco = 0
	arr = bytarr(2*n,2*n)
	for i = 0l, 2*n - 1 do arr[*,i] = i/2
	tvscl, arr
	plvar_keep, act = 'res'

	return
end

