Function Kink_sumsq, ofr, lx, ly, rx, ry, absolute = abs

	common m_kink_keep, def, olx, oly, orx, ory
	on_error, 1

	case n_params() of
		1	:	begin
					if keyword_set(def) then begin
						wlx = olx
						wly = oly
						wrx = orx
						wry = ory
					endif else message, 'Not initialized!'
				end
		5	:	begin
					if n_elements(lx) eq n_elements(ly) then begin
						wlx = (olx = lx)
						wly = (oly = ly)
					endif else message, 'Left sizes mismatch!
					if n_elements(rx) eq n_elements(ry) then begin
						wrx = (orx = rx)
						wry = (ory = ry)
					endif else message, 'Right sizes mismatch!
					def = 1
				end
		else:	message, 'Wrong number of inputs!
	endcase

	wlx = wlx + ofr[0]
	dum = where(wlx ge 0, ndum)
	if ndum gt 0 then begin
		tem = Unwind(wlx[dum],wly[dum],rad=abs(ofr[1]))
		wlx[dum] = tem[0,*]
		wly[dum] = tem[1,*]
	endif

	wrx = wrx - ofr[0]
	dum = where(wrx le 0, ndum)
	if ndum gt 0 then begin
		tem = Unwind(wrx[dum],wry[dum],rad=abs(ofr[1]))
		wrx[dum] = tem[0,*]
		wry[dum] = tem[1,*]
	endif

	if keyword_set(abs) then res = total(abs(wly)) + total(abs(wry)) $
	else res = total(wly^2) + total(wry^2)

	return, FPU_fix(res)
end