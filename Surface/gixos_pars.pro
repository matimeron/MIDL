Pro Gixos_pars, dth, s1h = s1h, len = len, short = sho, long = lon

	on_error, 1

	trol = 225.
	ftpl = 100.
	lsh = [270.,250.]
	llo = [270.,650.]

	case One_of(len,sho,lon) of
		0	:	l = len
		1	:	l = lsh
		2	:	l = llo
	endcase

	phi = !dtor*dth
	sfac = l[1]*(trol*phi - s1h)
	sd = sfac/(2*l[0] + l[1] - trol) > 0
	su = sfac/(2*l[0] + l[1] + trol) > 0

	s1o = trol*phi/3
	so = 2*l[1]*trol*phi/(3*(2*l[0] + l[1] + trol))

	print
	print, form = "('To block downstream, S4-5 	= ',f5.3,' mm')", sd
	print, form = "('Angular acceptance 		= ',f5.3,' deg')",!radeg*sd/l[1]
	print, form = "('Throughput = 			= ',e9.2)", s1h*sd^2/l[1]
	print
	print, form = "('To block upstream, S4-5 	= ',f5.3,' mm')", su
	print, form = "('Angular acceptance 		= ',f5.3,' deg')",!radeg*su/l[1]
	print, form = "('Throughput = 			= ',e9.2)", s1h*su^2/l[1]
	print
	print, form = "('Optimal S1H 			= ',f5.3,' mm')", s1o
	print, form = "('Optimal S4-5 			= ',f5.3,' mm')", so
	print, form = "('Angular acceptance 		= ',f5.3,' deg')",!radeg*so/l[1]
	print, form = "('Throughput = 			= ',e9.2)", s1o*so^2/l[1]

	return
end