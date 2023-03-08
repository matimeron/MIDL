Function Rhfun, x, par

	on_error, 1

	res = 0.*x - 1.
	dum = where(abs(x) le par[0], ndum)
	if ndum gt 0 then res[dum] = res[dum] - par[1]

	return, res
end