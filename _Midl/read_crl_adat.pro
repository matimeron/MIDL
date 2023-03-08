Function Read_CRL_adat, fnam, head = hed

	on_error, 1

	tab = string(9b)
	if n_elements(fnam) eq 0 then fnam = File_get()
	tem = Rascline(fnam,lines=lin)
	hed = lin[0]
	dat = lin[1:*]
	n = n_elements(dat)
	res = fltarr(7,n)
	for i = 0, n-1 do begin
		dum = Strparse_mm(dat[i],'	 ',lis)
		res[*,i] = float(lis[1:7])
	endfor

	return, res
end