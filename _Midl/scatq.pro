Function Scatq, x, z, distance = dst, radius = rad, approx = apr, correct = cor

	on_error, 1

	wdis = Cast(dst,4)
	wx = x/wdis
	wz = z/wdis
	ws = sqrt(wx^2 + wz^2)

	if keyword_set(apr) then res = ws $
	else res = sqrt(2*ws^2/(1 + ws^2 + sqrt(1 + ws^2)))
	if keyword_set(cor) then begin
		if Isnum(rad) then begin
			eps = rad/wdis
			res = res - eps/(2*ws^2)*(2*wx^2 + ws^4)
		endif else message, "Radius needed for correction!'
	endif

	return, res
end	