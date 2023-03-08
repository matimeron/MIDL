Function Avar_grid, low = lo, high = hi, step = st, npoints = len

	on_error, 1

	dum = Codims(lo,hi,st,sam=sam,dim=n,ninp=check)
	if sam and (check eq 3) then begin
		n = n[0] > 1
		s = Lexisort(lo,hi)
		wlo = lo[s]
		whi = hi[s]
		wst = st[s]

		res = []
		for i = n-1, 0, -1 do begin
			if wlo[i] lt whi[i] then begin
				next = Make_grid([wlo[i],whi[i]],wst[i],/step)
				if i lt n-1 then begin
					good = where(next lt min(res),ngood)
					if ngood gt 0 then res = [next[good],res]
				endif else res = next
			endif
		endfor
		res = Fltround(res,dig=6)
	endif else message, 'Mismatched or missing inputs!

	return, res([Sorpurge(res,net=len)])
end