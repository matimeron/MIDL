Function Ave_sumsq, ro, n, show = sho

	on_error, 1
	nn = 1 + Make_range(n-1,def=0)
	res = FPU_fix(1 + 2*ro/(1-ro)*(1 - (1 - ro^nn)/((1-ro)*nn)))
	if keyword_set(sho) then plot, nn, res

	return, res
end