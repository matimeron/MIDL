Pro Smiley, d1, d2, distance = dst, theta = tet, omega = omg, yaw = tau, $
	vert_focus = vd2, vangle = van, _extra = _e

	on_error, 1

	np = 201
	mult = 1e-3

	wtet = mult*tet
	womg = mult*omg
	wtau = mult*Default(tau,0.)
	kap = 2.*d2/(d1+d2)
	kfac = 2*kap*(kap-1) > 0
	philim = acos(kfac/(kfac+1))
	philo = Root('smiley_fun',[-philim,0],par=[-womg/2,kap,wtet,wtau])
	phihi = Root('smiley_fun',[0,philim],par=[womg/2,kap,wtet,wtau])
	wphi = Make_grid([philo,phihi],np)
	weps = asin(Smiley_fun(wphi,[0,kap,wtet,wtau]))

	tem = 2*sin(wphi/2)^2
	r_int = (fir = (sec = (w_dir = fltarr(3,np))))

	r_int[0,*] = sin(wtet)*cos(wtau)*sin(wphi) + cos(2*wtet)*sin(wtau)*tem
	r_int[1,*] = sin(2*wtet)*tem
	r_int[2,*] = sin(wtet)*sin(wtau)*sin(wphi) - cos(2*wtet)*cos(wtau)*tem
	r_int = kap*d1*r_int

	ttem = 2*sin(wphi)^2
	fir[0,*] = sin(weps) - cos(weps)*sin(wtet)*sin(2*wphi)
	fir[1,*] = (sin(weps)*cos(wtau) + cos(weps)*sin(wtau))*cos(wtet)*sin(2*wphi)
	fir[2,*] = sin(weps)*sin(wtet)*sin(2*wphi) + cos(weps)
	sec[0,*] = sin(weps)*(sin(wtet)^2*sin(wtau)^2 + cos(wtau)^2) + $
			cos(weps)*cos(wtet)^2*cos(wtau)*sin(wtau)
	sec[1,*] = (-sin(weps)*sin(wtau) + cos(weps)*cos(wtau))*cos(wtet)*sin(wtet)
	sec[2,*] = sin(weps)*cos(wtet)^2*cos(wtau)*sin(wtau) + $
			cos(weps)*(sin(wtet)^2*cos(wtau)^2 + sin(wtau)^2)

	w_dir[0,*] = fir[0,*] - ttem*sec[0,*]
	w_dir[1,*] = fir[1,*] - ttem*sec[1,*]
	w_dir[2,*] = fir[2,*] - ttem*sec[2,*]

	if Isnum(vd2) then begin
		rtan = 2.*d1*vd2/((d1 + vd2)*sin(wtet))
		delvan = 2*r_int[2,*]/rtan
		w_dir[1,*] = w_dir[1,*] + delvan
		vfl = 1
	endif else vfl = 0

	wrat_x = w_dir[0,*]/w_dir[2,*]
	wrat_y = w_dir[1,*]/w_dir[2,*]
	prodst = dst - r_int[2,*]

	x = r_int[0,*] + prodst*wrat_x
	y = r_int[1,*] + prodst*wrat_y

	x_apr = 1.*d1*(d2 - dst)/d2*sin(weps)
	y_apr = ((d1*d2 - dst*(d1 - d2))*sin(weps)/d2^2 + 2.*dst*sin(wtau)/d2)* $
			(d1 + d2)*sin(weps)/(2*sin(wtet))
	if vfl then y_apr = y_apr + dst*delvan

	xran = (max(abs(x)) > max(abs(x_apr)))*[-1,1]/mult
	yran = [min(y) < min(y_apr), max(y) > max(y_apr)]/mult

	wset, 0
	plot, x/mult, y/mult, xran = xran, yran = yran, tit = 'Beam profile', $
	xtit = '(mm)', ytit = '(mm)', /nodata, _extra = _e
	oplot, x/mult, y/mult, thi = 3, col = !pcol.red
	oplot, x_apr/mult, y_apr/mult, thi = 1, col = !pcol.blue

	if keyword_set(van) then begin
		x_ang = w_dir[0,*]
		y_ang = w_dir[1,*]
;		if max(abs(y_ang)) lt mult^3 then y_ang = 0*y_ang
		drat = 1.*d1/d2
		x_ang_apr = -drat*sin(weps)
		y_ang_apr = ((1 - drat^2)*sin(weps) + 2*(1 + drat)*sin(wtau))* $
				sin(weps)/(2*sin(wtet))
		if vfl then y_ang_apr = y_ang_apr + delvan

		xran = (max(abs(x_ang)) > max(abs(x_ang_apr)))*[-1,1]/mult^2
		yran = [min(y_ang)< min(y_ang_apr), max(y_ang)> max(y_ang_apr)]/mult^2

		window, 1
		wset, 1
		plot, x_ang/mult^2, y_ang/mult^2, tit = 'Vertical angle', /nodata, $
		xran= xran, yran= yran, xtit= '(!7l!xr)', ytit= '(!7l!xr)', _extra = _e
		oplot, x_ang/mult^2, y_ang/mult^2, thi = 3, col = !pcol.red
		oplot, x_ang_apr/mult^2, y_ang_apr/mult^2, thi = 1, col = !pcol.blue
		wset, 0
		wshow
	endif

	return
end
