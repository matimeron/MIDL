Pro Comprot, eps, theta = tet, psi = psi, degrees = deg, clean=cle

	on_error, 1

	eps = Default(eps,0d,/dtyp)
	if keyword_set(deg) then mult = !dpi/180 else mult = 1d
	wtet = mult*tet
	wpsi = mult*psi

	ax = [eps*cos(wpsi),eps*sin(wpsi),sqrt(1-eps^2)]

	rot = mat_rot(wtet,ax=ax,clean=cle)
	arot = mat_rot(wtet,ax=[0,0,1d],clean=cle)
	stet = sin(wtet)
	cctet = 1 - cos(wtet)

	fir = [[0d,0d,stet*sin(wpsi)+cctet*cos(wpsi)],$
			[0d,0d,-stet*cos(wpsi)+cctet*sin(wpsi)],$
			[-stet*sin(wpsi)+cctet*cos(wpsi),stet*cos(wpsi)+cctet*sin(wpsi),0d]]
	sec = [[cctet*cos(wpsi)^2,stet/2+cctet*sin(wpsi)*cos(wpsi),0d],$
			[-stet/2+cctet*sin(wpsi)*cos(wpsi),cctet*sin(wpsi)^2,0d],$
			[0d,0d,-cctet]]

	pcomb = wpsi + wtet/2
	mcomb = wpsi - wtet/2
	fir1 = [[0d,0d,sin(pcomb)],$
			[0d,0d,-cos(pcomb)],$
			[-sin(mcomb),cos(mcomb),0d]]
	fir1 = 2*sin(wtet/2)*fir1
	sec1=[[sin(wtet/2)*(1+cos(2*wpsi)),cos(wtet/2)+sin(wtet/2)*sin(2*wpsi),0d],$
	[-(cos(wtet/2)-sin(wtet/2)*sin(2*wpsi)),sin(wtet/2)*(1-cos(2*wpsi)),0d],$
	[0d,0d,-2*sin(wtet/2)]]
	sec1 = sin(wtet/2)*sec1
	
	arot = arot + eps*fir + eps^2*sec

	print
	print, rot
	print
	print, arot
	print
	print, rot-arot
stop
	return
end