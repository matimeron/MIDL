Pro ptest, x, p

	on_error, 1

	n = floor(1./p)
;	x = make_grid([0,1],1e-3,/step)
	y = (yy = 0*x)
	xx = x - floor(x)
	for i = 0, n do begin
		dum = where(xx gt (i-0.5)*p and xx le ((i+0.5)*p),/null)
		y[dum] = i*p
		yy[dum] = i
		y[where(xx gt (i-0.5)*p and xx le ((i+0.5)*p),/null)] = i*p
	endfor
	dum = where(xx gt (1.+n*p)/2 and xx le 1,/null)
	y[dum] = 1
	yy[dum] = 0
	y = y + floor(x)
	yy = yy + floor(x)
	plot, x, abs(y-x)
stop
	return
end