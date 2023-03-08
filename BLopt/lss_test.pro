Pro LSS_test, ene, samloc = sal, mirloc = mil, slope_err = sle, _extra = _e

	on_error, 1
	abc0 = ABC_gen(ene,_extra=_e)
	sigs = 1/Diagovec(abc0.amat)
	rsgsq = sigs[1]
	asgsq = sigs[3]

	psqi = rsgsq*asgsq
	qsqi = rsgsq
	rvli = 0*asgsq
	pqri = [psqi,qsqi,rvli]
	pqrj = PQR_prop(pqri,l=mil,rpsq=psq,rqsq=qsq,rr=rvl)
	focl = 1./(1./(sal-mil) + rvl/qsq)

	abc1 = ABC_prop(abc0,dis=Cast(mil,4))
	abc2 = ABC_foc(abc1,focl = [0,focl])
	abc3 = ABC_sler(abc2,sler=[0,sle])

	for i = -10, 10 do begin
		print, i
		abc4 = ABC_prop(abc3,dis=Cast(sal-mil + i,4))
		abc_show, abc4, vran=[-0.2,0.2]
		wait, 1
	endfor

	return
end