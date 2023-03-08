Function Fprime_kern, ecl, eval=evl, edge=ede, power=pow, hwid=hwd, muvals=muv,$
	standard = sta

	on_error, 1

	res = Fprime_fun(ecl,edge=ede,pow=pow,hwid=hwd,muv=muv,/esq,standard=sta)
	zer = where(ecl eq evl,kzer,comp=nozer,ncomp=knozer)
	if kzer gt 0 then res[zer] = 0
	if knozer gt 0 then res[nozer] = res[nozer]/(evl^2 - ecl[nozer]^2)

	return, FPU_fix(res)
end