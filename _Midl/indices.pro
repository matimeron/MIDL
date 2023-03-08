Pro Indices, n

	on_error, 1

	wn = Cast(n,3,3)
	len = (wn+1)*(wn+2)*(wn+3)/6
	ind = lonarr(3,len)
	check = lonarr(len) + 1
	l = 0
	for i = 0, n do begin
		for j = 0, i do begin
			for k = 0, j do begin
				tem = [i,j,k]
				ftot = total(tem mod 2,/pre)
				stot = total(tem,/pre) mod 4
				if l gt 0 and (ftot eq 3 or (ftot eq 0 and stot eq 0)) $
				then ind[*,l] = tem else check[l] = 0
				l = l + 1
			endfor
		endfor
	endfor

	good = where(check,ngood)
	if ngood gt 0 then begin
		ind = ind[*,good]
		sumsq = total(ind^2,1,/pre)
		cind = string(ind,form='("(",3i1,")")')
		s = Lexisort(sumsq,cind)
		ind = ind[*,s]
		cind = cind[s]
		sumsq = sumsq[s]
		Tabulate, cind, sumsq, form = ['a8','i4'],tit = 'Diffraction indices',$
		head = ['Indices','Sum^2']
	endif else message, 'Nothing to show!', /con

	return
end