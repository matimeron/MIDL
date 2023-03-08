Function Running_sum, x, y, step = stp, length = len, $
	backward = back, centered = cent, forward = forw, mean = mea

	on_error, 1

	nxy = Split_xy(x,y,x_ret=wx,y_ret=wy,z_ret=wser)
	rx = Make_grid([min(wx,max=max),max],stp,/step,dim=nres)
	ry = 0*rx
	cnt = lonarr(nres)
	mtyp = One_of(back,cent,forw) > 0
	wlen = abs(Default(len,stp)) > abs(stp)
	hi = rx + mtyp*wlen/2.
	lo = hi - wlen

	for i = 0l, nres[0]-1 do begin
		if mtyp eq 0 then ran = where(wx gt lo[i] and wx le hi[i], nran) $
		else ran = where(wx ge lo[i] and wx lt hi[i], nran)
		cnt[i] = nran
		if nran gt 0 then ry[i] = total(wy[ran])
	endfor

	if keyword_set(mea) then ry = ry/(cnt > 1)

	return, Join_xy(rx,ry)
end