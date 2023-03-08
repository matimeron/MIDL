Pro Det_response, snum

	y = Scan_column(snum,-1)
	n = Scan_column(snum,-2)
	count = n_elements(y)
	z = y/n
	res = sqrt((1 - (total(z)^2/count + total(z/n))/total(z^2)) > 0)

	print,'	Response variation = ', res

	return
end