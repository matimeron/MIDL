Pro angtab, ene, qran, qstep

	on_error, 1

	k = float(2*!dpi*ene/!srcon.conv)

	qval = Make_grid(qran,qstep,/step)
	phi1 = Bragg_angle(ene=ene,crys='ge',ind=[1,1,1],/rad)
	phi2 = Bragg_angle(ene=ene,crys='ge',ind=[2,2,0],/rad)
	dphi = 2*(phi2-phi1)

	alp = asin(qval/(2*k))
	chi = asin(sin(alp)/sin(dphi))
	ttet = atan(cos(chi)*tan(dphi))

	tabulate, qval, !radeg*chi, !radeg*alp, !radeg*ttet, $
	form=replicate('f6.3',4), title = string(ene,form='(f4.1)') + $
	' keV angle parameters', head = ['Qz','Chi','Alpha','2*Teta'] 

	return
end