Function PP_form, x, y, coe, _extra = _e

	on_error, 1

	res = [1.,-1]*y*(coe[[0,2]] - coe[[1,3]]*shift(y,-1))

	return, res
end