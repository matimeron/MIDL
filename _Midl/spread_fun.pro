Function Spread_fun, x, rad = rad, mud = mud, pos = pos

	on_error, 1

	wmud = Default(mud,0)
	if wmud eq 0 then mult = 1 else mult = wmud/(1 - exp(-wmud))
	if rad ne 0 then begin
		if keyword_set(pos) then pn = 1 else pn = -1
		xx = 1 + pn*x
		res = (1 - xx/sqrt(rad^2 + xx^2))*exp(-wmud*x)
	endif else res = 0*x

	return, FPU_fix(mult*res)
end