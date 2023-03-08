Pro pinktem, bounds, angles

	on_error, 1
	ran = [-6.,36.]
	nb = n_elements(bounds)
	na = n_elements(angles)
	if na-nb ne 1 then message, 'Bad input lengths'
	if min(bounds) le ran[0] or max(bounds) ge ran[1] then message, 'Bad bounds'

	x = [ran[0],bounds,ran[1]]
	dx = (Dif(x))[1:*]
	dz = dx/tan(!dtor*angles)
	tabulate, x[1:*], dx, total(dx,/cum), dz, total(dz,/cum)

	return
end
