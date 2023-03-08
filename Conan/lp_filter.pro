Function LP_filter, arr, cut

	on_error, 1

	n = Split_xy(arr,y=yarr)
	cut = 0 > Default(cut,0,/dtyp) < (n-1)/2
	tem = fft(yarr)
	tem[n/2-cut:n/2+cut] = 0

	return, Cast(fft(tem,/inverse),4,Type(arr),/fix)
end