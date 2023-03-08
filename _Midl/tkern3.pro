Function Tkern3, t, pv

	on_error, 1
	res = t*exp(-t^pv[0])*sin(pv[1]*t)
	return, FPU_fix(res)
end