Function Tkern2, t, pv

	on_error, 1
	res = t*exp(-t^pv[0])*beselj(pv[1]*t,0,doub=Isnum(t,/doub))
	return, FPU_fix(res)
end