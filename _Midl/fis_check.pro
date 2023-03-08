Function Fis_check, x, par

	on_error, 1

	if n_elements(par) eq 3 then wpar = par else wpar = [par,0]
	res = x - (alog(x) + wpar[0]*wpar[2])/(wpar[0]*wpar[1]) - 1

	return, res
end