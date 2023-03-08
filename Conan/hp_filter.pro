Function HP_filter, arr, cut

	on_error, 1

	n = Split_xy(arr,y=yarr)
	cut = 0 > Default(cut,0,/dtyp) < n/2
	tem = fft(yarr)
	tem[0] = 0
	if cut gt 0 then begin
		tem[1:cut] = 0
		tem[n-cut:n-1] = 0
	endif

	return, Cast(fft(tem,/inverse),4,Type(arr),/fix)
end