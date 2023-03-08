Pro CRL_distor, r, x, pv

	on_error, 1

	wx = Cast(x,4)
	dror = pv*r/(2*wx^2)
	print
	print, r*dror
	print, dror
	print

	return
end