Function Diffpat, grid, qran, qres, order = ord, qvals = dq

	ic = dcomplex(0,1)
	tpi = 2*!dpi
	typ = (Type(grid) < Type(qran)) > 4
	qm = tpi*Default(ord,0,/dtyp)
	dq = Make_grid(tpi*qran,tpi*qres,/step,dim= nq)
	q = qm + dq
	nq = nq[0]
	res = make_array(nq,type=typ)

	for i = 0, nq - 1 do res(i) = Abs_mm(total(exp(ic*q(i)*grid)))

	return, Cast(res^2,typ,typ,/fix)
end