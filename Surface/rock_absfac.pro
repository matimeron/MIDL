Pro Rock_absfac, ene, qz, thi= thi, alpha= alp, absorb= mu, result= res, $
	_extra = _e

	on_error, 1

	ic = complex(0,1)
	k = 2*!pi*ene/!srcon.conv
	wthi = cast(thi,4)
	walp = alp*!dtor
	wmu = Default(mu,0.,/dtyp)

	qran = qz*(qz/(2*k) - walp)*[-1,1]
	wq = Make_grid(qran,1001)

	fac = (1 + exp(-2*wmu*k*wthi*qz^3/(qz^4 - 4*k^2*wq^2)))/2

	rat = 1/Abs_mm(fac)^2
	plot, wq, rat, _extra = _e
	res = transpose([[wq],[rat/min(rat)],[0*rat]])

	return
end