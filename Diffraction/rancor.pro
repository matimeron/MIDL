Function Rancor, seed, len, sum = sum

    wlen = Default(len,1l,/dtyp) > 1l
    if wlen gt 1 then begin
		res = [0.,randomn(seed,wlen-1)]
		if keyword_set(sum) then begin
			for s = 0, sum-1 do begin
				for i= 1, wlen-1 do res[i] = res[i-1] + res[i]
				res = res/sqrt(wlen)
			endfor
		endif
;		res = res - res[wlen-1]/(wlen-1)*findgen(wlen)
    endif else res = [0.]

    return, res
end
