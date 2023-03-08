Pro Speed, n

	nn = Default(n,1,/dtyp) > 1

	time = systime(/sec)
	for i = 0, nn-1 do dum = cycperm(10)
	ttime = systime(/sec)
	print
	print, (ttime-time)/nn

	return
end