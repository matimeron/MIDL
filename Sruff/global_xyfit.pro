Function Global_xyfit, data, show = sho, wait = wai, _extra = _e

	on_error, 1

	siz = size(data)
	if siz[0] ne 3 then message, 'bad input!'
	n = siz[1] - 2
	res = fltarr(6,n)
	shofl = keyword_set(sho)
	if shofl then wai = Default(wai,1)
	for i = 0, n-1 do begin
		res[*,i] = XYfit(data,sli=i,/gau,qua=[1,1,1],show_fit=shofl,_extra=_e)
		if shofl then wait, wai
	endfor

	return, res
end