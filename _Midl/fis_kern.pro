Function Fis_kern, x, y, coe, _extra = e

	on_error, 1
	res = [-coe[0]*y[0],coe[0]*coe[1]*y[0]-1]*y[1]

	return, fpu_fix(res)
end