Function Lamel_pat, qd, n, frac = frc, contrast = con, form_factor = ffac

	on_error, 1

	i = dcomplex(0,1)
	nqd = n_elements(qd)
	eiqd = exp(i*qd)
	ffac = dcomplex(0d*qd + 1 - con + 2*frc*con)
	noz = where(qd ne 0, nnoz)
	if nnoz gt 0 then ffac[noz] = $
	(eiqd[noz] - con*(eiqd[noz] - 2*exp(i*frc*qd[noz]) + 1) - 1)/(i*qd[noz])

	arr = dcomplexarr(nqd,n)
	arr[*,0] = ffac
	for p = 1l, n-1 do arr[*,p] = arr[*,p-1]*eiqd
	ffac = (Abs_mm(ffac))^2

	return, FPU_fix((Abs_mm(total(arr,2))/n)^2)
end
