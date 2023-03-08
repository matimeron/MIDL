Function Grey, val

	on_error, 1

	wval = (0>val<1)*16
	res = Trucol(wval*[1,1,1],sca=16)

	return, res
end