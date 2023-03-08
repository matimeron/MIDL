Function Cycperm, n, list = lis

	on_error, 1

	if Isnum(n,/int) then begin
		lis = ulonarr(n+1)
		lis(0) = 1
		for i = 0, n do begin
			tsum = 0
			for j = 0, i-1 do tsum = tsum + Bincoef(i,j,/int)*lis[j]
			lis[i] = ulong(factorial(i) -tsum)
		endfor
	endif

	res = lis[n]

	return, res
end