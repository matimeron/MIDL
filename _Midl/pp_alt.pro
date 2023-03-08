Function PP_alt, x, y, coe, _extra = _e

	on_error, 1

	res = [y[1],y[1]^2 + coe*y[0]*y[1] - y[0]^2 - y[0]]

	return, res
end