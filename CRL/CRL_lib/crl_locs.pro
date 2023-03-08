Function CRL_locs, dat, values = val

	on_error, 1

	n = Split_xy(dat,x=ene,y=siz)
	mind = Extrema(siz,/min)
	res = ene[mind]
	val = siz[mind]

	return, res
end