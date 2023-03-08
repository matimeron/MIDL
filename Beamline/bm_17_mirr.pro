Function BM_17_mirr, e, thick = thi, _extra = _e

	on_error, 1

	e_c = 19.52
	oogam = 73.
	csig = 0.99
	psi = 86.

	r_e = 1.687e4
	r_gc = 9.892e6
	dd = 0.045
	d = 1e-3*Default(thi,1e3*dd,/dtyp)

	tetg = 1e6*sqrt(r_gc*d^2/r_e^3)
	nrman = (psi/2)^3/tetg^2

	sig = 0.57*(e_c/e)^0.43*oogam
	dum = Bragg_angle(ene=e,crys='si',ind=[1,1,1],dar=dar,/rad)
	ran = 1e6/2*dar < nrman

	nen = n_elements(e)
	res = (nrm = fltarr(nen))
	for i = 0l, nen-1 do begin
		res[i] = Grav_sag(sig[i],tetg,ran=ran[i],con=csig,prec='low',_extra=_e)
		nrm[i] = Grav_sag(sig[i],tetg,ran=nrman,con=csig,prec='low',_extra=_e)
	endfor

	return, res/nrm
end