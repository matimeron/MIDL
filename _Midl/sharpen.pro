Function Sharpen, arr, wid, order = ord, _extra = _e

	on_error, 1

	lim = ([25,55])[Isnum(arr,/doub)]
	typ = Calctype(arr,0.)
	word = Default(ord,2d,/dtyp) > 1d
	coe = Bincoef(word,1+lindgen(word))*(-1)^lindgen(word)
	lim = ([25,55])[Isnum(arr,/doub)]
	if max(wid) gt lim then $
		message, 'Width excessive, accuracy may be lost',/con

	res = 0*arr
	for i = 0, word-1 do begin
		if i eq 0 then tem = arr else tem = Smooth_mm(tem,wid,_extra=_e)
		res = res + coe[i]*tem
	endfor

	return, Cast(res,typ,typ,/fix)
end