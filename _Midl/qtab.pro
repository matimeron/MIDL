Pro Qtab, ene = ene, qstep = qst, $
	phi1 = phi1, phi2 = phi2, chi = chi, alp = alp, ttet = ttet

	wqst = Default(qst,0.1)
	k = 2*!pi*ene/!srcon.conv
	qz = make_grid([0,1],.1,/step)
	alp = asin(qz/(2*k))
	phi1 = bragg_angle(ene=ene,crys='ge',ind=[1,1,1],/rad)
	phi2 = bragg_angle(ene=ene,crys='ge',ind=[2,2,0],/rad)
	chi = asin(sin(alp)/sin(2*(phi2-phi1)))
	ttet = atan(cos(chi)*tan(2*(phi2-phi1)))

	phi1 = !radeg*phi1
	phi2 = !radeg*phi2
	chi = !radeg*chi
	alp = !radeg*alp
	ttet = !radeg*ttet

	print
	print, phi1, phi2
	print
	Tabulate, qz, chi, alp, ttet, form=['f4.2','f6.3','f6.3','f6.3'], $
	head=['Qz','Chi','Alpha','2Theta']

	return
end