Function AltPrune, arr, gap

	on_error, 1

	warr = Cast(arr,4)
	eps = Toler(warr)
	if gap le 0 then message, 'GAP must be positive!'

	if n_elements(warr) gt 1 then begin
		mn = min(warr,max=mx)
		mgap = min(Dif(warr,/lin))*(1 - eps)
		nbin = ceil((mx - mn)*(1 + eps)/(gap > mgap))
		lbin = nbin - 1
		res = replicate(-1l,nbin+1)
		tem = histogram(warr,binsize=gap,min=mn,nbins=nbin,reverse=rev)
		j = where(tem gt 0, nj)
		for i = 0l, nj - 1 do begin
			dum = rev[rev[j[i]]:rev[j[i]+1]-1]
			res[j[i]] = dum[0]
			if j[i] eq lbin then begin
				if tem[lbin] gt 1 then res[nbin] = (reverse(dum))[0]
			endif
		endfor
		dum = where(res ge 0)
		res = res[dum]
		res = res[sort(res)]
	endif else res = 0l

	return, res
end
