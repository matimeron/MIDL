Pro Pstop_temp, x, sx, sy, pow, ctilt, shift = shf, window = win, temp = temp,$
	_extra = _e

	on_error, 1
	kap = 0.365
	h = 0.015
	thi = 25
	wtemp = 25.

	wshf = Default(shf,0,/dtyp)
	ashf = abs(wshf)
	n = n_elements(x)
	delx = (x[n-1]-x[0])/(n-1)
	xshf = delx*ashf
	mess = 'Beamstop shift '
	case Sign(wshf) of
		-1	:	begin
					wsx = ([replicate(sx[0],ashf),sx])[0:n-1]
					wsy = ([replicate(sy[0],ashf),sy])[0:n-1]
					wpow = ([replicate(pow[0],ashf),pow])[0:n-1]
					print
					print,mess+ 'left by '+ string(xshf,form='(f5.2)')+ ' mm'
					print
				end
		0	:	begin
					wsx = sx
					wsy = sy
					wpow = pow
				end
		1	:	begin
					wsx = ([sx,replicate(sx[n-1],ashf)])[ashf:ashf+n-1]
					wsy = ([sy,replicate(sy[n-1],ashf)])[ashf:ashf+n-1]
					wpow = ([pow,replicate(pow[n-1],ashf)])[ashf:ashf+n-1]
					print
					print,mess+ 'right by '+ string(xshf,form='(f5.2)')+ ' mm'
					print
				end
	endcase

	temp = fltarr(n)
	for i = 0, n-1 do temp[i] = wtemp + $
	BC_etemp([wsx[i],wsy[i]],wpow[i],kap=kap,h=h,thi=thi,xan=ctilt[i])

	window, Default(win,0)
	plot, x, temp, /nodata, _extra = _e
	oplot, x, temp, col = !pcol.green
	oplot, x, 0*temp + 300, col = !pcol.dred, line = 2
	oplot, x, 0*temp + 425, col = !pcol.lred, line = 2

	return
end