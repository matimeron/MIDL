Function NScatq, x, z, distance = dst, radius = rad, approx = apr, correct = cor

	on_error, 1

	wdis = Cast(dst,4)
	wx = x/wdis
	wz = z/wdis
	ws = sqrt(wx^2 + wz^2)

	if keyword_set(apr) then begin
		p = 2*wx/ws
		q = ws
	endif else begin
		p = wx/ws*sqrt(2*(1 + 1/sqrt(1 + ws^2)))
		q = ws*sqrt(2/(1 + ws^2 + sqrt(1 + ws^2)))
	endelse
	if keyword_set(cor) then begin
		if Isnum(rad) then begin
			eps = rad/wdis
			a = 2-q^2
			b = 2+q^2
			c = 4-p^2-q^2
			mat = [[a*q,b*p],[-a*p*q/c,b-a*q^2/c]]
			vec = eps*a*[p,q^2/2]
			corr = Solve_linsys(mat,vec,/row,/svd)
			res = q - corr[1]
		endif else message, "Radius needed for correction!'
	endif else res = q

	return, res
end	