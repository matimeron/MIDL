Function Harpeak, gtet, dat, har = l

	on_error, 1

	loc = Extrema(dat,/max,num=n)
	if n gt l/2 then begin
		n = l/2
		loc = loc[0:n-1]
	endif
	res = 0.*loc
	for i = 0, n-1 do begin
		ind = loc[i] + [-1,0,1]
		res[i] = Splinroot(Splin_coeffs(gtet[ind],dat[ind]),der=1)
	endfor

	return, res
end
	