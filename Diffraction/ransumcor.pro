Function Ransumcor, seed, len

    wlen = Default(len,1l,/dtyp) > 1l
    if wlen gt 1 then begin
	res = [0.,randomn(seed,wlen-1)]
	tot = total(res)
	for i = 1, wlen-1 do res[i] = res[i-1] + res[i]
	res = res - tot/(wlen-1)*findgen(wlen)
    endif else res = [0.]

    return, res
end
