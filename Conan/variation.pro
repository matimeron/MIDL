Function Variation, x, y, normalized = norm

	on_error, 1

	n = Split_xy(x,y,x_ret=wx,y_ret=wy)

	if n gt 1 then begin
		wy = Cast(y,5)
		favsq = (total(wy)/n)^2
		sav = total(wy^2)/n
		res = (sav - favsq)/(favsq - sav/n)
		if not keyword_set(norm) then res = favsq*res
	endif else res = 0*y

	return, Cast(res,4,Type(x),/fix)
end
