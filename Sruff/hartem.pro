Pro Hartem, gtet, k, har = l, const = c

	on_error, 1

	if n_elements(k) ne 2 then message, '2 K values needed!'

	dat0 = harcheck(gtet,k[0],har=l)
	sloc0 = harpeak(gtet,dat0,har=l)/sqrt(k[0]^2 + c)
	plot, gtet, dat0/max(dat0)
	dat1 = harcheck(gtet,k[1],har=l)
	sloc1 = harpeak(gtet,dat1,har=l)/sqrt(k[1]^2 + c)
	oplot, gtet, dat1/max(dat1)

	print
	tabulate, sloc0, sloc1, (sloc1-sloc0)/(sloc1+sloc0)
	print
	print, total(((sloc1-sloc0)/(sloc1+sloc0))^2)

	return
end

	